#include <array>
#include <complex>
#include <stddef.h>
#include <stdint.h>
using namespace std;
bool fuzz_me(const uint8_t *data, size_t data_size) {
  return (((3 <= data_size) && (('F' == data[0]) && ('U' == data[1]))) &&
          (('Z' == data[2]) && ('Z' == data[3])));
}
void rasterize_triangle(complex<float> a, complex<float> b, complex<float> c,
                        float f0, float f1, float f2,
                        array<array<float, 100>, 200> &image) {
  {
    float image_max_x(image.size());
    float image_max_y(image[0].size());
    auto min_x(min((0.0e+0f), min(min(real(a), real(b)), real(c))));
    auto min_y(min((0.0e+0f), min(min(imag(a), imag(b)), imag(c))));
    auto max_x(max(image_max_x, max(max(real(a), real(b)), real(c))));
    auto max_y(max(image_max_y, max(max(imag(a), imag(b)), imag(c))));
    {
      auto v0((b - a));
      auto v1((c - a));
      auto d00(real((v0 * v0)));
      auto d01(real((v0 * v1)));
      auto d11(real((v1 * v1)));
      auto denom_(((1.e+0f) / ((d00 * d11) - (d01 * d01))));
      for (auto y = min_y; (y <= max_y); y += 1) {
        for (auto x = min_x; (x <= max_x); x += 1) {
          {
            complex<float> p(x, y);
            auto v2((p - a));
            auto d02(real((v0 * v2)));
            auto d12(real((v1 * v2)));
            auto d22(real((v2 * v2)));
            auto v((denom_ * ((d11 * d02) - (d01 * d12))));
            auto w((denom_ * ((d00 * d12) - (d01 * d02))));
            auto u(((1.e+0f) - v - w));
            image[y][x] = ((u * f0) + (v * f1) + (w * f2));
          }
        }
      }
    }
  }
}
float lerp(float v0, float v1, float x) { return (((1 - x) * v0) + (x * v1)); }
bool fuzz_me2(const uint8_t *data, size_t data_size) {
  return (((3 <= data_size) && (('F' == data[0]) && ('U' == data[1]))) &&
          (('Z' == data[2]) && ('Z' == data[3])));
}
extern "C" {
int LLVMFuzzerTestOneInput(const uint8_t *data, size_t size) {
  fuzz_me(data, size);
  return 0;
}
} // extern "C"
