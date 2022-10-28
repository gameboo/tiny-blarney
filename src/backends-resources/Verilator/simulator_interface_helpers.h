#ifndef _SIMULATOR_INTERFACE_HELPERS_H
#define _SIMULATOR_INTERFACE_HELPERS_H

#ifdef __cplusplus
extern "C" {
#endif

#define DIVCEIL(x, y) (1 + ((x - 1) / y))

void *bitmemcpy( void *destArg, const size_t destBitOffset
               , const void *srcArg, const size_t srcBitOffset
               , const size_t bitLen );

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

// consume an element from the bit channel (return the number of bytes consumed)
ssize_t bit_channel_consume_non_block (bit_channel_t* desc, uint8_t* elemdest);

// consume an element from the bit channel (block until an element is consumed)
uint8_t* bit_channel_consume (bit_channel_t* desc, uint8_t* elemdest);

// produce an element into the bit channel (return the number of bytes produced)
ssize_t bit_channel_produce_non_block (bit_channel_t* desc, uint8_t* elemsrc);

// produce an element into the bit channel (block until an element is produced)
uint8_t* bit_channel_produce (bit_channel_t* desc, uint8_t* elemsrc);

// open a bit channel for production
bit_channel_t* bit_channel_create_producer ( char const *pathname
                                           , size_t bitwidth );
// open a bit channel for consumption
bit_channel_t* bit_channel_create_consumer ( char const *pathname
                                           , size_t bitwidth );

// desctroy the bit channel described by `desc`
void bit_channel_destroy (bit_channel_t* desc);

#ifdef __cplusplus
}
#endif

#endif
