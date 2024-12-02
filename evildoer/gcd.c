#include <inttypes.h>

int64_t gcd(int64_t a, int64_t b) {
    int remainder = a % b;

    if (remainder == 0) {
        return b;
    }

    return gcd(b, remainder);
}

