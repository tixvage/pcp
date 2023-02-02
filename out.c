#include <stdint.h>
typedef uint8_t u8;
typedef int8_t i8;
typedef uint16_t u16;
typedef int16_t i16;
typedef uint32_t u32;
typedef int32_t i32;
typedef uint64_t u64;
typedef int64_t i64;

i32 get_and_return(i32 v) {
return v;
}
i32 multiply(i32 a, i32 b) {
i32 result = a * b;
return result;
}
i32 get_number(void) {
return 20 + 50 * 2;
}
i32 main(void) {
if (1 + 2 == 3) {
i32 x = 0 == 0 == 0;
}
i32 y = multiply(get_and_return(0), get_and_return(0));
y = get_number();
y = get_number() + 50;
return y - 35;
}
