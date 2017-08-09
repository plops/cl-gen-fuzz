#include <array>
#include <complex>
#include <cstddef>
#include <cstdint>
using namespace std;
bool fuzz_me(const uint8_t *data, size_t data_size) {
  return (((3 <= data_size) && (('F' == data[0]) && ('U' == data[1]))) &&
          (('Z' == data[2]) && ('Z' == data[3])));
}
float goldstein_price(complex<float> z) {
  {
    auto x(real(z));
    auto y(imag(z));
    auto xy1((x + y + 1));
    auto x2((x * x));
    auto y2((y * y));
    auto xy3(((2 * x) - (3 * y)));
    return ((1 + (xy1 * xy1 * (19 + (-14 * x) + (3 * x2) + (-14 * y) +
                               (6 * x * y) + (3 * y2)))) *
            (30 + (xy3 * xy3) + (18 + (-32 * x) + (12 * x2) + (48 * y) +
                                 (-36 * x * y) + (27 * y2))));
  }
}
void rasterize_triangle(complex<float> a, complex<float> b, complex<float> c,
                        float f0, float f1, float f2,
                        array<array<float, 100>, 200> &image) {
  {
    const uint32_t image_max_x(image.size());
    const uint32_t image_max_y(image[0].size());
    auto bottom([](float x, float y, float z) -> uint32_t {
      return static_cast<uint32_t>(floor(min(min(x, y), z)));
    });
    auto ceiling([](float x, float y, float z) -> uint32_t {
      return static_cast<uint32_t>(ceil(max(max(x, y), z)));
    });
    uint32_t min_x(min(0u, bottom(real(a), real(b), real(c))));
    uint32_t min_y(min(0u, bottom(imag(a), imag(b), imag(c))));
    uint32_t max_x(max((image_max_x - 1), ceiling(real(a), real(b), real(c))));
    uint32_t max_y(max((image_max_y - 1), ceiling(imag(a), imag(b), imag(c))));
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
int main() {
  {
    const int w(100);
    const int h(200);
    array<array<float, w>, h> im{};
    for (unsigned int j = 0; (j < h); j += 1) {
      for (unsigned int i = 0; (i < w); i += 1) {
        {
          auto x(((3.e+0f) * ((i / ((1.e+0f) * w)) - (5.e-1f))));
          auto y(((3.e+0f) * ((j / ((1.e+0f) * h)) - (5.e-1f))));
          im[i][j] = goldstein_price(complex<float>{x, y});
        }
      }
    }
  }
  return 0;
}