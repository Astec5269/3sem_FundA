#include <ctype.h>
#include <float.h>
#include <inttypes.h>
#include <limits.h>
#include <math.h>
#include <stdarg.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef enum {
  OK = 0,
  INVALID = 1,
  MEMORY = 2,
  IO = 3,
  RANGE = 4,
  END = 5
} Status;

Status parse_uint(const char *s, uintmax_t max, uintmax_t *out);
Status parse_real(const char *s, double *out);
int same_text(const char *a, const char *b);
Status report(Status s);
static Status finite_one(double x, int base, double eps, int *out);
Status finite_decimal_fractions(int base, double eps, int **out, size_t count,
                                ...);
Status finite_fractions(int base, double eps, int **out, size_t count, ...);

Status parse_uint(const char *s, uintmax_t max, uintmax_t *out) {
  if (!s || !*s || !out)
    return INVALID;
  uintmax_t v = 0;
  for (size_t i = 0; s[i]; ++i) {
    if (s[i] < '0' || s[i] > '9')
      return INVALID;
    unsigned d = (unsigned)(s[i] - '0');
    if (d > max || v > (max - d) / 10)
      return RANGE;
    v = v * 10 + d;
  }
  *out = v;
  return OK;
}

Status parse_real(const char *s, double *out) {
  if (!s || !*s || !out)
    return INVALID;
  const char *p = s;
  if (*p == '+' || *p == '-')
    ++p;
  int digits = 0;
  while (isdigit((unsigned char)*p)) {
    ++p;
    digits = 1;
  }
  if (*p == '.') {
    ++p;
    while (isdigit((unsigned char)*p)) {
      ++p;
      digits = 1;
    }
  }
  if (!digits)
    return INVALID;
  if (*p == 'e' || *p == 'E') {
    ++p;
    if (*p == '+' || *p == '-')
      ++p;
    if (!isdigit((unsigned char)*p))
      return INVALID;
    while (isdigit((unsigned char)*p))
      ++p;
  }
  if (*p)
    return INVALID;
  char *end = NULL;
  long double v = strtold(s, &end);
  if (*end || !isfinite(v) || fabsl(v) > DBL_MAX)
    return RANGE;
  int nonzero = 0;
  for (const char *q = s; *q && *q != 'e' && *q != 'E'; ++q)
    if (*q >= '1' && *q <= '9')
      nonzero = 1;
  /* Reject underflow instead of silently converting a nonzero input to zero. */
  double converted = (double)v;
  if (nonzero && converted == 0.0)
    return RANGE;
  *out = converted;
  return OK;
}

int same_text(const char *a, const char *b) {
  if (!a || !b)
    return 0;
  while (*a && *a == *b) {
    ++a;
    ++b;
  }
  return *a == *b;
}

Status report(Status s) {
  const char *message = "Unknown error";
  switch (s) {
  case OK:
    return OK;
  case INVALID:
    message = "Invalid input or argument";
    break;
  case MEMORY:
    message = "Cannot allocate memory";
    break;
  case IO:
    message = "File or stream error";
    break;
  case RANGE:
    message = "Value or size out of range";
    break;
  case END:
    message = "Unexpected end of input";
    break;
  }
  fprintf(stderr, "ERROR %d: %s\n", s, message);
  return s;
}

static Status finite_one(double x, int base, double eps, int *out) {
  if (!out || !isfinite(x) || x <= 0.0 || x >= 1.0 || !isfinite(eps) ||
      eps <= 0 || base < 2 || base > 36)
    return INVALID;
  char buf[64];
  int len = snprintf(buf, sizeof(buf), "%.15g", x);
  if (len < 0 || (size_t)len >= sizeof(buf))
    return RANGE;
  double roundtrip;
  Status st = parse_real(buf, &roundtrip);
  if (st)
    return st;
  if (fabs(roundtrip - x) > eps)
    return RANGE;
  uintmax_t numerator = 0;
  int fractional = 0, after_dot = 0, exponent = 0;
  size_t i = 0;
  for (; buf[i] && buf[i] != 'e' && buf[i] != 'E'; ++i) {
    if (buf[i] == '.') {
      after_dot = 1;
      continue;
    }
    numerator = numerator * 10 + (unsigned)(buf[i] - '0');
    fractional += after_dot;
  }
  if (buf[i]) {
    ++i;
    int sign = 1;
    if (buf[i] == '-' || buf[i] == '+') {
      if (buf[i] == '-')
        sign = -1;
      ++i;
    }
    while (buf[i])
      exponent = exponent * 10 + (buf[i++] - '0');
    exponent *= sign;
  }
  int twos = fractional - exponent, fives = twos;
  while (twos > 0 && numerator && numerator % 2 == 0) {
    --twos;
    numerator /= 2;
  }
  while (fives > 0 && numerator && numerator % 5 == 0) {
    --fives;
    numerator /= 5;
  }
  *out = (twos <= 0 || base % 2 == 0) && (fives <= 0 || base % 5 == 0);
  return OK;
}

Status finite_decimal_fractions(int base, double eps, int **out, size_t count,
                                ...) {
  if (!out || base < 2 || base > 36 || !isfinite(eps) || eps <= 0)
    return INVALID;
  *out = NULL;
  if (!count)
    return OK;
  if (count > SIZE_MAX / sizeof(int))
    return RANGE;
  int *r = malloc(count * sizeof(*r));
  if (!r)
    return MEMORY;
  Status st = OK;
  va_list ap;
  va_start(ap, count);
  for (size_t i = 0; i < count && !st; ++i)
    st = finite_one(va_arg(ap, double), base, eps, &r[i]);
  va_end(ap);
  if (st) {
    free(r);
    return st;
  }
  *out = r;
  return OK;
}

Status finite_fractions(int base, double eps, int **out, size_t count, ...) {
  if (!out || base < 2 || base > 36 || !isfinite(eps) || eps <= 0)
    return INVALID;
  *out = NULL;
  if (!count)
    return OK;
  if (FLT_RADIX != 2)
    return RANGE;
  if (count > SIZE_MAX / sizeof(int))
    return RANGE;
  int *r = malloc(count * sizeof(*r));
  if (!r)
    return MEMORY;
  Status st = OK;
  va_list ap;
  va_start(ap, count);
  for (size_t i = 0; i < count && !st; ++i) {
    double x = va_arg(ap, double);
    if (!isfinite(x) || x <= 0.0 || x >= 1.0)
      st = INVALID;
    else
      r[i] = base % 2 == 0;
  }
  va_end(ap);
  if (st) {
    free(r);
    return st;
  }
  *out = r;
  return OK;
}

int main(int argc, char **argv) {
  Status st = OK;
  int decimal = argc > 1 && same_text(argv[1], "--decimal");
  if (decimal) {
    --argc;
    ++argv;
  }
  if (argc == 1) {
    int *r = NULL;
    st = decimal ? finite_decimal_fractions(2, 1e-12, &r, 3, 0.125, 0.1, 0.5)
                 : finite_fractions(2, 1e-12, &r, 3, 0.125, 0.1, 0.5);
    if (!st && printf("0.125: %s\n0.1: %s\n0.5: %s\n", r[0] ? "YES" : "NO",
                      r[1] ? "YES" : "NO", r[2] ? "YES" : "NO") < 0)
      st = IO;
    free(r);
  } else if (argc >= 3) {
    uintmax_t base;
    st = parse_uint(argv[1], 36, &base);
    if (!st && base < 2)
      st = INVALID;
    for (int i = 2; i < argc && !st; ++i) {
      double x;
      int *r = NULL;
      st = parse_real(argv[i], &x);
      if (!st)
        st = decimal ? finite_decimal_fractions((int)base, 1e-12, &r, 1, x)
                     : finite_fractions((int)base, 1e-12, &r, 1, x);
      if (!st && printf("%s: %s\n", argv[i], r[0] ? "YES" : "NO") < 0)
        st = IO;
      free(r);
    }
  } else
    st = INVALID;
  if (!st && fflush(stdout))
    st = IO;
  return report(st);
}
