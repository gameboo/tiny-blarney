#ifdef __cplusplus
extern "C" {
#endif

#include <fcntl.h>
#include <libgen.h>
#include <sys/stat.h>
#include <unistd.h>
#include <errno.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <dirent.h>

#include "simulator_interface_helpers.h"

// create path
//////////////
static int create_dir_path (char const *path, mode_t mode) {
  int res = -1; // initialise return status to error
  DIR* dir = opendir (path);
  int errsv = errno;
  if (dir) { // check if the dir path is already existing, and end recursion
    closedir (dir);
    res = 0; // no error encountered, success
  } else if (ENOENT == errsv) { // otherwise, create it and its parent
    // copy path for string manipulation
    size_t len = strlen (path) + 1;
    char* parentpath = (char*) malloc (len * sizeof (char));
    strcpy (parentpath, path);
    parentpath = dirname (parentpath);
    // try to create the parent hierarchy
    res = create_dir_path (parentpath, mode | 0111);
    // if it succeeded, try to create the directory itself
    if (res == 0) res = mkdir (path, mode | 0111);
  }
  // return error status
  return res;
}

// create a blue unix fifo descriptor
/////////////////////////////////////
static bit_channel_t* bit_channel_create ( char const *pathname
                                         , mode_t mode
                                         , int flags
                                         , size_t e_bitwidth ) {
  bit_channel_t* desc = (bit_channel_t*) malloc (sizeof (bit_channel_t));
  // copy pathname
  size_t n = strlen (pathname);
  desc->pathname = (char*) malloc (n + 1);
  memcpy (desc->pathname, pathname, n + 1);
  // file descriptor, initially set to -1
  desc->fd = -1;
  // the desired mode for file creation (unix permissions rwx.rwx.rwx)
  desc->mode = mode;
  // flags to open the fifo with (O_RDONLY, O_WRONLY, O_RDWR, O_NONBLOCK...)
  desc->flags = flags;
  // the size of the element being exchanged in bits and bytes
  desc->bit_width = e_bitwidth;
  desc->byte_width = (e_bitwidth) ? DIVCEIL(e_bitwidth, 8) : 0;
  // element buffer
  if (desc->byte_width) desc->buf = (uint8_t*) malloc (desc->byte_width);
  else desc->buf = NULL;
  // running byte count to account for currently read/written bytes, initially
  // set to 0
  desc->byte_count = 0;
  // return the initialized bit channel descriptor
  return desc;
}

// destroy a bit channel descriptor (free dynamically allocated memory)
///////////////////////////////////////////////////////////////////////
static void bit_channel_destroy_desc (bit_channel_t* desc) {
  free (desc->pathname);
  if (desc->buf) free (desc->buf);
  free (desc);
}

// print a bit channel descriptor (debug)
/////////////////////////////////////////
static void bit_channel_print_desc (bit_channel_t* desc) {
  printf ( "bit_channel_t @ %p:\n"
           ".pathname: %s\n"
           ".fd: %d\n"
           ".mode: 0%o\n"
           ".flags: 0x%0x\n"
           ".bit_width: %lu\n"
           ".buf: %p\n"
           ".byte_count: %ld\n"
         , desc
         , desc->pathname
         , desc->fd
         , desc->mode
         , desc->flags
         , desc->bit_width
         , desc->buf
         , desc->byte_count );
}

// open file descriptor associated with a bit_channel_t
///////////////////////////////////////////////////////
static void bit_channel_open_fifo (bit_channel_t* desc) {
  int fd = open (desc->pathname, desc->flags);
  if (fd == -1) {
    int errsv = errno;
    switch (errsv) {
      case ENXIO: break;
      default:
        bit_channel_print_desc (desc);
        printf ( "%s:l%d: %s: %s (errno: %d)\n"
               , __FILE__, __LINE__, __func__, strerror (errsv), errsv);
        exit (EXIT_FAILURE);
    }
  }
  desc->fd = fd;
}

// create a fifo on the filesystem for other processes to open
//////////////////////////////////////////////////////////////
static void bit_channel_create_fifo (bit_channel_t* desc) {
  // create the parent dir hierarchy (copy path for string manipulation)
  size_t len = strlen (desc->pathname) + 1;
  char* pathcpy = (char*) malloc (len * sizeof (char));
  strcpy (pathcpy, desc->pathname);
  create_dir_path (dirname (pathcpy), desc->mode);
  // create the unix fifo
  if (mkfifo (desc->pathname, desc->mode) == -1) {
    int errsv = errno;
    switch (errsv) {
      case EEXIST: break;
      default:
        bit_channel_print_desc (desc);
        printf ( "%s:l%d: %s: %s (errno: %d)\n"
               , __FILE__, __LINE__, __func__, strerror (errsv), errsv);
        exit (EXIT_FAILURE);
    }
  }
  // open the unix fifo
  bit_channel_open_fifo (desc);
  // set the fifo size
  //fcntl (desc->fd, F_SETPIPE_SZ, DFLT_PIPE_SZ);
}

// close an opened unix fifo file descriptor
////////////////////////////////////////////
static void bit_channel_close_fifo (bit_channel_t* desc) {
  if (close (desc->fd) == -1) {
    int errsv = errno;
    switch (errsv) {
      default:
        bit_channel_print_desc (desc);
        printf ( "%s:l%d: %s: %s (errno: %d)\n"
               , __FILE__, __LINE__, __func__, strerror (errsv), errsv);
        exit (EXIT_FAILURE);
    }
  }
}

// unlink the backing file for a unix fifo descriptor
/////////////////////////////////////////////////////
static void bit_channel_unlink_fifo (bit_channel_t* desc) {
  if (unlink (desc->pathname) == -1) {
    int errsv = errno;
    switch (errsv) {
      default:
        bit_channel_print_desc (desc);
        printf ( "%s:l%d: %s: %s (errno: %d)\n"
               , __FILE__, __LINE__, __func__, strerror (errsv), errsv);
        exit (EXIT_FAILURE);
    }
  }
}

// read bytes from the fifo and aggregate them in the descriptor buffer
///////////////////////////////////////////////////////////////////////
static void bit_channel_read_fifo (bit_channel_t* desc) {
  // check for presence of producer
  if (desc->fd == -1) bit_channel_open_fifo (desc);
  if (desc->fd == -1) return; // immediate return if no producer is present
  // read for appropriate amount of bytes and accumulate in buf
  size_t remaining = desc->byte_width - desc->byte_count;
  ssize_t res = read (desc->fd, desc->buf + desc->byte_count, remaining);
  // handle possible errors
  if (res == -1) {
    int errsv = errno;
    switch (errsv) {
      case EAGAIN: return;
      default:
        bit_channel_print_desc (desc);
        printf ( "%s:l%d: %s: %s (errno: %d)\n"
               , __FILE__, __LINE__, __func__, strerror (errsv), errsv);
        exit (EXIT_FAILURE);
    }
  }
  // on read success, accumulate bytes read
  desc->byte_count += res;
}

// write bytes into the fifo and keep track of how many in the descriptor
/////////////////////////////////////////////////////////////////////////
static void bit_channel_write_fifo (bit_channel_t* desc, const uint8_t* data) {
  // check for presence of consumer
  if (desc->fd == -1) bit_channel_open_fifo (desc);
  if (desc->fd == -1) return;
  // write remaining bytes to write
  size_t remaining = desc->byte_width - desc->byte_count;
  int res = write (desc->fd, data + desc->byte_count, remaining);
  // handle possible errors
  if (res == -1) {
    int errsv = errno;
    switch (errsv) {
      case EBADF:
      case EPIPE:
      case EAGAIN:
        return;
      default:
        bit_channel_print_desc (desc);
        printf ( "%s:l%d: %s: %s (errno: %d)\n"
               , __FILE__, __LINE__, __func__, strerror (errsv), errsv);
        exit (EXIT_FAILURE);
    }
  }
  // on write success, accumulate bytes written
  desc->byte_count += res;
}

////////////////////////////////////////////////////////////////////////////////

// consume an element from the bit channel (return the number of bytes consumed)
ssize_t bit_channel_consume_non_block (bit_channel_t* desc, uint8_t* elemdest) {
  bit_channel_read_fifo (desc);
  if (desc->byte_count == desc->byte_width) {
    desc->byte_count = 0;
    memcpy (elemdest, desc->buf, desc->byte_width);
    return desc->byte_width;
  }
  return desc->byte_count;
}

// consume an element from the bit channel (block until an element is consumed)
uint8_t* bit_channel_consume (bit_channel_t* desc, uint8_t* elemdest) {
  while (bit_channel_consume_non_block (desc, elemdest) != desc->byte_width);
  return elemdest;
}

// produce an element into the bit channel (return the number of bytes produced)
ssize_t bit_channel_produce_non_block (bit_channel_t* desc, uint8_t* elemsrc) {
  // copy on first attempt
  if (desc->byte_count == 0) memcpy (desc->buf, elemsrc, desc->byte_width);
  bit_channel_write_fifo (desc, desc->buf);
  // if full data has been written, reset byte count and return success
  if (desc->byte_count == desc->byte_width) {
    desc->byte_count = 0;
    return desc->byte_width;
  }
  return desc->byte_count;
}

// produce an element into the bit channel (block until an element is produced)
uint8_t* bit_channel_produce (bit_channel_t* desc, uint8_t* elemsrc) {
  while (bit_channel_produce_non_block (desc, elemsrc) != desc->byte_width);
  return elemsrc;
}

// open a bit channel for production
bit_channel_t* bit_channel_create_producer ( char const *pathname
                                           , size_t bitwidth ) {
  // initialise a fifo descriptor
  bit_channel_t* desc = bit_channel_create ( pathname
                                           , 0644
                                           , O_RDWR | O_NONBLOCK
                                           , bitwidth );
  // create and open the unix fifo
  bit_channel_create_fifo (desc);
  // return the descriptor
  return desc;
}

// open a bit channel for consumption
bit_channel_t* bit_channel_create_consumer ( char const *pathname
                                           , size_t bitwidth ) {
  // initialise a fifo descriptor
  bit_channel_t* desc = bit_channel_create ( pathname
                                           , 0622
                                           , O_RDWR | O_NONBLOCK
                                           , bitwidth );
  // create and open the unix fifo
  bit_channel_create_fifo (desc);
  // return the descriptor
  return desc;
}

// desctroy the bit channel described by `desc`
///////////////////////////////////////////////
void bit_channel_destroy (bit_channel_t* desc) {
  bit_channel_close_fifo (desc);
  bit_channel_unlink_fifo (desc);
  bit_channel_destroy_desc (desc);
}

#ifdef __cplusplus
}
#endif
