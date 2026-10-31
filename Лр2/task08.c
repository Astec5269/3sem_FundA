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

Status text_length(const char *s, size_t *n);
Status copy_text(const char *s, char **out);
Status parse_uint(const char *s, uintmax_t max, uintmax_t *out);
Status report(Status s);
static int digit_value(char c);
static Status valid_digits(const char *s, int base);
Status add_columns(int base, const char *a, const char *b, char **out);
Status sum_numbers(int base, char **out, size_t count, ...);

Status text_length(const char *s, size_t *n) {
  if (!s || !n)
    return INVALID;
  *n = 0;
  while (s[*n]) {
    if (*n == SIZE_MAX - 1)
      return RANGE;
    ++*n;
  }
  return OK;
}

Status copy_text(const char *s, char **out) {
  size_t n;
  if (!out)
    return INVALID;
  *out = NULL;
  Status st = text_length(s, &n);
  if (st)
    return st;
  char *p = malloc(n + 1);
  if (!p)
    return MEMORY;
  for (size_t i = 0; i <= n; ++i)
    p[i] = s[i];
  *out = p;
  return OK;
}

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

static int digit_value(char c) {
  return c >= '0' && c <= '9' ? c - '0'
         : c >= 'a' && c <= 'z' ? c - 'a' + 10
         : c >= 'A' && c <= 'Z' ? c - 'A' + 10
                                : -1;
}

static Status valid_digits(const char *s, int base) {
  if (!s || !*s || base < 2 || base > 36)
    return INVALID;
  for (; *s; ++s) {
    int d = digit_value(*s);
    if (d < 0 || d >= base)
      return INVALID;
  }
  return OK;
}

Status add_columns(int base, const char *a, const char *b, char **out) {
  if (!out)
    return INVALID;
  *out = NULL;
  Status st = valid_digits(a, base);
  if (!st)
    st = valid_digits(b, base);
  if (st)
    return st;
  size_t na = strlen(a), nb = strlen(b), n = na > nb ? na : nb;
  if (n > SIZE_MAX - 2)
    return RANGE;
  char *r = malloc(n + 2);
  if (!r)
    return MEMORY;
  const char *digits = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ";
  size_t k = n + 1;
  r[k] = 0;
  int carry = 0;
  while (na || nb || carry) {
    int sum = carry;
    if (na)
      sum += digit_value(a[--na]);
    if (nb)
      sum += digit_value(b[--nb]);
    r[--k] = digits[sum % base];
    carry = sum / base;
  }
  if (k == n + 1)
    r[--k] = '0';
  while (r[k] == '0' && r[k + 1])
    ++k;
  memmove(r, r + k, n + 2 - k);
  *out = r;
  return OK;
}

Status sum_numbers(int base, char **out, size_t count, ...) {
  if (!out || base < 2 || base > 36)
    return INVALID;
  *out = NULL;
  char *sum = NULL;
  Status st = copy_text("0", &sum);
  if (st)
    return st;
  va_list ap;
  va_start(ap, count);
  for (size_t i = 0; i < count && !st; ++i) {
    const char *s = va_arg(ap, const char *);
    char *next = NULL;
    st = add_columns(base, sum, s, &next);
    if (!st) {
      free(sum);
      sum = next;
    }
  }
  va_end(ap);
  if (st) {
    free(sum);
    return st;
  }
  *out = sum;
  return OK;
}

int main(int argc, char **argv) {
  char *sum = NULL;
  Status st = OK;
  if (argc == 1)
    st = sum_numbers(16, &sum, 3, "FFFFFFFFFFFFFFFFFFFFFFFF", "1", "000A");
  else if (argc >= 3) {
    uintmax_t base;
    st = parse_uint(argv[1], 36, &base);
    if (!st && base < 2)
      st = INVALID;
    if (!st)
      st = copy_text("0", &sum);
    for (int i = 2; i < argc && !st; ++i) {
      char *next = NULL;
      st = sum_numbers((int)base, &next, 2, (const char *)sum,
                       (const char *)argv[i]);
      if (!st) {
        free(sum);
        sum = next;
      }
    }
  } else
    st = INVALID;
  if (!st && puts(sum) == EOF)
    st = IO;
  free(sum);
  if (!st && fflush(stdout))
    st = IO;
  return report(st);
}
