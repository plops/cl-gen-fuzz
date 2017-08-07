#include <array>
#include <stddef.h>
#include <stdint.h>
using namespace std;
bool fuzz_me(const uint8_t *data, size_t data_size) {
  return (((3 <= data_size) && (('F' == data[0]) && ('U' == data[1]))) &&
          (('Z' == data[2]) && ('Z' == data[3])));
}
extern "C" {
int LLVMFuzzerTestOneInput(const uint8_t *data, size_t size) {
  fuzz_me(data, size);
  return 0;
}
} // extern "C"
