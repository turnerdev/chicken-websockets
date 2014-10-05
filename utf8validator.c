#include "utf8validator.h"

static const uint8_t UTF8VALIDATOR_DFA[] =
{
   0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, // 00..1f
   0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, // 20..3f
   0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, // 40..5f
   0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, // 60..7f
   1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9, // 80..9f
   7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7, // a0..bf
   8,8,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2, // c0..df

   0xa,0x3,0x3,0x3,0x3,0x3,0x3,0x3,0x3,0x3,0x3,0x3,0x3,0x4,0x3,0x3, // e0..ef
   0xb,0x6,0x6,0x6,0x5,0x8,0x8,0x8,0x8,0x8,0x8,0x8,0x8,0x8,0x8,0x8, // f0..ff
   0x0,0x1,0x2,0x3,0x5,0x8,0x7,0x1,0x1,0x1,0x4,0x6,0x1,0x1,0x1,0x1, // s0..s0
   1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,1,1,1,1,1,0,1,0,1,1,1,1,1,1, // s1..s2
   1,2,1,1,1,1,1,2,1,2,1,1,1,1,1,1,1,1,1,1,1,1,1,2,1,1,1,1,1,1,1,1, // s3..s4
   1,2,1,1,1,1,1,1,1,2,1,1,1,1,1,1,1,1,1,1,1,1,1,3,1,3,1,1,1,1,1,1, // s5..s6
   1,3,1,1,1,1,1,3,1,3,1,1,1,1,1,1,1,3,1,1,1,1,1,1,1,1,1,1,1,1,1,1  // s7..s8
};

#define UTF8_ACCEPT 0
#define UTF8_REJECT 1

void utf8vld_reset (utf8_validator_t* validator) {
   validator->state = UTF8_ACCEPT;
   validator->current_index = 0;
   validator->total_index = 0;
   validator->is_valid = 1;
   validator->ends_on_codepoint = 1;
}

void utf8vld_validate (utf8_validator_t* validator, const uint8_t* data, size_t offset, size_t length) {

   int state = validator->state;

   for (size_t i = offset; i < length + offset; ++i) {

      state = UTF8VALIDATOR_DFA[256 + (state << 4) + UTF8VALIDATOR_DFA[data[i]]];

      if (state == UTF8_REJECT)
      {
         validator->state = state;
         validator->current_index = i - offset;
         validator->total_index += i - offset;
         validator->is_valid = 0;
         validator->ends_on_codepoint = 0;
         return;
      }
   }

   validator->state = state;
   validator->current_index = length;
   validator->total_index += length;
   validator->is_valid = 1;
   validator->ends_on_codepoint = validator->state == UTF8_ACCEPT;
}

int utf8_valid(const uint8_t* data, size_t len) {
  utf8_validator_t validator;
  utf8vld_reset(&validator);
  utf8vld_validate(&validator, data, 0, len);
  return validator.is_valid;
}
