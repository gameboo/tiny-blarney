#ifndef _SIMULATOR_INTERFACE_HELPERS_H
#define _SIMULATOR_INTERFACE_HELPERS_H

#define DIVCEIL (x, y) (1 + ((x - 1) / y))

// create path
static int create_dir_path (const char* path, mode_t mode);

// handle on a bit channel
typedef struct {
  char* pathname;
  int fd;
  mode_t mode;
  int flags;
  size_t bit_width;
  size_t byte_width;
  uint8_t* buf;
  ssize_t byte_count;
} bit_channel_t;

// create a blue unix fifo descriptor
static bit_channel_t* bit_channel_create ( char* pathname
                                         , mode_t mode
                                         , int flags
                                         , size_t e_bitwidth );

// destroy a bit channel descriptor (free dynamically allocated memory)
static void bit_channel_destroy (bit_channel_t* desc);

// print a bit channel descriptor (debug)
static void bit_channel_print_desc (bit_channel_t* desc);

// open file descriptor associated with a bit_channel_t
static void bit_channel_open_fifo (bit_channel_t* desc);

// create a fifo on the filesystem for other processes to open
static void bit_channel_create_fifo (bit_channel_t* desc);

// close an opened unix fifo file descriptor
static void bit_channel_close_fifo (bit_channel_t* desc);

// unlink the backing file for a unix fifo descriptor
static void bit_channel_unlink_fifo (bit_channel_t* desc);

// desctroy the unix fifo described by `desc`
static void bit_channel_destroy_fifo (bit_channel_t* desc);

// read bytes from the fifo and aggregate them in the descriptor buffer
static void bit_channel_read_fifo (bit_channel_t* desc);

// write bytes into the fifo and keep track of how many in the descriptor
static void bit_channel_write_fifo (bit_channel_t* desc, const uint8_t* data);

////////////////////////////////////////////////////////////////////////////////

// consume an element from the bit channel (return the number of bytes consumed)
ssize_t bit_channel_consume_non_block (bit_channel_t* desc, uint8_t* elemdest);

// consume an element from the bit channel (block until an element is consumed)
uint8_t* bit_channel_consume (bit_channel_t* desc, uint8_t* elemdest);

// produce an element into the bit channel (return the number of bytes produced)
ssize_t bit_channel_produce_non_block (bit_channel_t* desc, uint8_t* elemsrc);

// produce an element into the bit channel (block until an element is produced)
uint8_t* bit_channel_produce (bit_channel_t* desc, uint8_t* elemsrc);

// open a bit channel for production
bit_channel_t* bit_channel_create_producer (char* pathname, size_t bitwidth);

// open a bit channel for consumption
bit_channel_t* bit_channel_create_consumer (char* pathname, size_t bitwidth);

#endif
