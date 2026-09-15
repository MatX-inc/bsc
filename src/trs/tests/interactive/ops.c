#include <stdio.h>
unsigned int c_mix(unsigned int a, unsigned int b) { return a * 2654435761u ^ b; }
void c_wide(unsigned int* out, const unsigned int* x) {
  for (int i = 0; i < 4; i++) out[i] = x[i] ^ (0x9e3779b9u * (i + 1));
}
