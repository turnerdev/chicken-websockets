#ifndef UTF8_DECODE_H
#define UTF8_DECODE_H

#include <stdlib.h>
#include <stdint.h>

//inline uint32_t decode(uint32_t* state, uint32_t* codep, uint32_t byte);

extern int countCodePoints(uint8_t* s, size_t count);

/* typedef struct {
   size_t current_index;
   size_t total_index;
   int state;
   int is_valid;
   int ends_on_codepoint;
} utf8_validator_t;

extern void utf8vld_reset (utf8_validator_t* validator);

extern void utf8vld_validate (utf8_validator_t* validator, const uint8_t* data, size_t offset, size_t length);

extern int utf8_valid(const uint8_t* data, size_t len); */

#endif // UTF8_DECODE_H
