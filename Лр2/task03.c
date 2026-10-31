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

typedef struct {
  size_t line, column;
} Match;

typedef struct {
  const char *path;
  Match *matches;
  size_t count;
  Status status;
} SearchResult;

Status report(Status s);
void search_free(SearchResult *r, size_t n);
Status find_in_bytes(const unsigned char *data, size_t size, const char *needle,
                     Match **out, size_t *count);
Status search_file(const char *path, const char *needle, Match **out,
                   size_t *count);
Status search_many(const char *needle, SearchResult **out, size_t count, ...);
static Status print_results(const SearchResult *r, size_t n);

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

void search_free(SearchResult *r, size_t n) {
  if (r) {
    for (size_t i = 0; i < n; ++i)
      free(r[i].matches);
    free(r);
  }
}

Status find_in_bytes(const unsigned char *data, size_t size, const char *needle,
                     Match **out, size_t *count) {
  if ((!data && size) || !needle || !*needle || !out || !count)
    return INVALID;
  *out = NULL;
  *count = 0;
  size_t len = strlen(needle), line = 1, column = 1;
  Match *m = NULL;
  for (size_t i = 0; i < size; ++i) {
    size_t j = 0;
    if (len <= size - i) {
      while (j < len && data[i + j] == (unsigned char)needle[j])
        ++j;
      if (j == len) {
        if (*count == SIZE_MAX / sizeof(*m)) {
          free(m);
          *count = 0;
          return RANGE;
        }
        Match *p = realloc(m, (*count + 1) * sizeof(*m));
        if (!p) {
          free(m);
          *count = 0;
          return MEMORY;
        }
        m = p;
        m[(*count)++] = (Match){line, column};
      }
    }
    if (data[i] == '\n') {
      ++line;
      column = 1;
    } else
      ++column;
  }
  *out = m;
  return OK;
}

Status search_file(const char *path, const char *needle, Match **out,
                   size_t *count) {
  if (!path || !*path || !needle || !*needle || !out || !count)
    return INVALID;
  *out = NULL;
  *count = 0;
  FILE *f = fopen(path, "rb");
  if (!f)
    return IO;
  unsigned char *buf = NULL;
  size_t n = 0, cap = 0;
  Status st = OK;
  int c;
  while ((c = fgetc(f)) != EOF) {
    if (n == cap) {
      size_t nc = cap ? cap * 2 : 256;
      if (cap > SIZE_MAX / 2) {
        st = RANGE;
        break;
      }
      unsigned char *p = realloc(buf, nc);
      if (!p) {
        st = MEMORY;
        break;
      }
      buf = p;
      cap = nc;
    }
    buf[n++] = (unsigned char)c;
  }
  if (ferror(f) && !st)
    st = IO;
  if (fclose(f) && !st)
    st = IO;
  if (!st)
    st = find_in_bytes(buf, n, needle, out, count);
  free(buf);
  return st;
}

Status search_many(const char *needle, SearchResult **out, size_t count, ...) {
  if (!out || !needle || !*needle || !count)
    return INVALID;
  *out = NULL;
  if (count > SIZE_MAX / sizeof(SearchResult))
    return RANGE;
  SearchResult *r = calloc(count, sizeof(*r));
  if (!r)
    return MEMORY;
  va_list ap;
  va_start(ap, count);
  for (size_t i = 0; i < count; ++i) {
    r[i].path = va_arg(ap, const char *);
    r[i].status = search_file(r[i].path, needle, &r[i].matches, &r[i].count);
  }
  va_end(ap);
  *out = r;
  return OK;
}

static Status print_results(const SearchResult *r, size_t n) {
  Status st = OK;
  for (size_t i = 0; i < n; ++i) {
    if (printf("FILE %s\n", r[i].path ? r[i].path : "(null)") < 0)
      return IO;
    if (r[i].status) {
      report(r[i].status);
      st = r[i].status;
    } else {
      if (!r[i].count && puts("NO MATCHES") == EOF)
        return IO;
      for (size_t j = 0; j < r[i].count; ++j)
        if (printf("%zu %zu\n", r[i].matches[j].line, r[i].matches[j].column) <
            0)
          return IO;
    }
  }
  return st;
}

int main(int argc, char **argv) {
  if (argc < 3)
    return report(INVALID);
  Status st = OK;
  /* Pairs demonstrate a real multiple-file variadic call, without a fixed CLI
   * limit. */
  for (int i = 2; i < argc; i += 2) {
    SearchResult *r = NULL;
    size_t n = i + 1 < argc ? 2 : 1;
    Status s = n == 2 ? search_many(argv[1], &r, n, (const char *)argv[i],
                                    (const char *)argv[i + 1])
                      : search_many(argv[1], &r, n, (const char *)argv[i]);
    if (!s)
      s = print_results(r, n);
    else
      report(s);
    search_free(r, n);
    if (s)
      st = s;
  }
  if (fflush(stdout))
    st = IO;
  return st;
}
