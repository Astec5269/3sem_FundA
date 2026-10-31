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
Status reverse_text(const char *s, char **out);
Status uppercase_odd(const char *s, char **out);
static int group(unsigned char c);
Status reorder_text(const char *s, char **out);
Status concatenate(const char *const *s, size_t count, unsigned seed,
                   char **out);

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

Status reverse_text(const char *s, char **out) {
  size_t n;
  Status st = text_length(s, &n);
  if (st || !out)
    return st ? st : INVALID;
  st = copy_text(s, out);
  if (st)
    return st;
  for (size_t i = 0; i < n; ++i)
    (*out)[i] = s[n - 1 - i];
  return OK;
}

Status uppercase_odd(const char *s, char **out) {
  Status st = copy_text(s, out);
  if (st)
    return st;
  for (size_t i = 0; (*out)[i]; ++i)
    if (i % 2 && (*out)[i] >= 'a' && (*out)[i] <= 'z')
      (*out)[i] -= 'a' - 'A';
  return OK;
}

static int group(unsigned char c) {
  return c >= '0' && c <= '9' ? 0 : ((c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z')) ? 1 : 2;
}

Status reorder_text(const char *s, char **out) {
  Status st = copy_text(s, out);
  if (st)
    return st;
  size_t k = 0;
  for (int g = 0; g < 3; ++g)
    for (size_t i = 0; s[i]; ++i)
      if (group((unsigned char)s[i]) == g)
        (*out)[k++] = s[i];
  return OK;
}

Status concatenate(const char *const *s, size_t count, unsigned seed,
                   char **out) {
  if (!s || !count || !out)
    return INVALID;
  *out = NULL;
  if (count > SIZE_MAX / sizeof(size_t))
    return RANGE;
  size_t total = 0;
  for (size_t i = 0; i < count; ++i) {
    size_t n;
    Status st = text_length(s[i], &n);
    if (st)
      return st;
    if (n > SIZE_MAX - 1 - total)
      return RANGE;
    total += n;
  }
  size_t *order = malloc(count * sizeof(*order));
  char *r = malloc(total + 1);
  if (!order || !r) {
    free(order);
    free(r);
    return MEMORY;
  }
  for (size_t i = 0; i < count; ++i)
    order[i] = i;
  srand(seed);
  for (size_t i = count; i > 1; --i) {
    size_t j = (size_t)rand() % i, t = order[i - 1];
    order[i - 1] = order[j];
    order[j] = t;
  }
  size_t k = 0;
  for (size_t i = 0; i < count; ++i)
    for (size_t j = 0; s[order[i]][j]; ++j)
      r[k++] = s[order[i]][j];
  r[k] = 0;
  free(order);
  *out = r;
  return OK;
}

int main(int argc, char **argv) {
  if (argc < 3 || argv[1][0] != '-' || !argv[1][1] || argv[1][2])
    return report(INVALID);
  char flag = argv[1][1];
  Status st = OK;
  char *out = NULL;
  if (flag != 'c' && argc != 3)
    return report(INVALID);
  if (flag == 'l') {
    size_t n;
    st = text_length(argv[2], &n);
    if (!st && printf("%zu\n", n) < 0)
      st = IO;
  } else if (flag == 'r')
    st = reverse_text(argv[2], &out);
  else if (flag == 'u')
    st = uppercase_odd(argv[2], &out);
  else if (flag == 'n')
    st = reorder_text(argv[2], &out);
  else if (flag == 'c') {
    if (argc < 4)
      return report(INVALID);
    uintmax_t seed;
    st = parse_uint(argv[3], UINT_MAX, &seed);
    if (!st) {
      size_t count = (size_t)argc - 3;
      const char **strings = malloc(count * sizeof(*strings));
      if (!strings)
        st = MEMORY;
      else {
        strings[0] = argv[2];
        for (size_t i = 1; i < count; ++i)
          strings[i] = argv[i + 3];
        st = concatenate(strings, count, (unsigned)seed, &out);
        free(strings);
      }
    }
  } else
    st = INVALID;
  if (!st && out && puts(out) == EOF)
    st = IO;
  free(out);
  if (!st && fflush(stdout))
    st = IO;
  return report(st);
}
