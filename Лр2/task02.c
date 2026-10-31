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
  uintmax_t id;
  char *name, *surname;
  double salary;
  size_t salary_group;
} Employee;

Status text_length(const char *s, size_t *n);
Status copy_text(const char *s, char **out);
static Status read_part(FILE *f, char **out, int token);
Status read_line(FILE *f, char **out);
Status parse_uint(const char *s, uintmax_t max, uintmax_t *out);
Status parse_real(const char *s, double *out);
Status split_fields(char *s, char **fields, size_t expected);
Status report(Status s);
static int raw_compare(const void *a, const void *b);
static int compare(const void *a, const void *b);
static int compare_desc(const void *a, const void *b);
static int latin(const char *s);
void employees_free(Employee *a, size_t n);
Status sort_employees(Employee *a, size_t n, int descending, double eps);
Status read_employees(FILE *f, Employee **out, size_t *count);

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

static Status read_part(FILE *f, char **out, int token) {
  if (!f || !out)
    return INVALID;
  *out = NULL;
  size_t n = 0, cap = 64;
  char *s = malloc(cap);
  if (!s)
    return MEMORY;
  int c = fgetc(f);
  if (token)
    while (c != EOF && isspace((unsigned char)c))
      c = fgetc(f);
  while (c != EOF && (token ? !isspace((unsigned char)c) : c != '\n')) {
    if (c == 0) {
      free(s);
      return INVALID;
    }
    if (n + 1 == cap) {
      if (cap > SIZE_MAX / 2) {
        free(s);
        return RANGE;
      }
      cap *= 2;
      char *p = realloc(s, cap);
      if (!p) {
        free(s);
        return MEMORY;
      }
      s = p;
    }
    s[n++] = (char)c;
    c = fgetc(f);
  }
  if (ferror(f)) {
    free(s);
    return IO;
  }
  if (c == EOF && !n) {
    free(s);
    return END;
  }
  if (!token && n && s[n - 1] == '\r')
    --n;
  s[n] = 0;
  *out = s;
  return OK;
}

Status read_line(FILE *f, char **out) { return read_part(f, out, 0); }

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

Status split_fields(char *s, char **fields, size_t expected) {
  if (!s || !fields)
    return INVALID;
  size_t n = 0;
  while (*s) {
    while (isspace((unsigned char)*s))
      ++s;
    if (!*s)
      break;
    if (n == expected)
      return INVALID;
    fields[n++] = s;
    while (*s && !isspace((unsigned char)*s))
      ++s;
    if (*s)
      *s++ = 0;
  }
  return n == expected ? OK : INVALID;
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

static int raw_compare(const void *a, const void *b) {
  const Employee *x = a, *y = b;
  return (x->salary > y->salary) - (x->salary < y->salary);
}

static int compare(const void *a, const void *b) {
  const Employee *x = a, *y = b;
  if (x->salary_group != y->salary_group)
    return x->salary_group > y->salary_group ? 1 : -1;
  int c = strcmp(x->surname, y->surname);
  if (c)
    return c;
  c = strcmp(x->name, y->name);
  if (c)
    return c;
  return (x->id > y->id) - (x->id < y->id);
}

static int compare_desc(const void *a, const void *b) { return compare(b, a); }

static int latin(const char *s) {
  if (!s || !*s)
    return 0;
  for (; *s; ++s)
    if (!((*s >= 'a' && *s <= 'z') || (*s >= 'A' && *s <= 'Z')))
      return 0;
  return 1;
}

void employees_free(Employee *a, size_t n) {
  for (size_t i = 0; i < n; ++i) {
    free(a[i].name);
    free(a[i].surname);
  }
  free(a);
}

Status sort_employees(Employee *a, size_t n, int descending, double eps) {
  if ((!a && n) || !isfinite(eps) || eps <= 0 ||
      (descending != 0 && descending != 1))
    return INVALID;
  for (size_t i = 0; i < n; ++i)
    if (!latin(a[i].name) || !latin(a[i].surname) || !isfinite(a[i].salary) ||
        a[i].salary < 0)
      return INVALID;
  if (!n)
    return OK;
  /* Exact ordering is needed for qsort transitivity. Epsilon groups are then
     fixed relative to the smallest salary of each group (not pairwise
     chaining). */
  qsort(a, n, sizeof(*a), raw_compare);
  double anchor = a[0].salary;
  size_t g = 0;
  for (size_t i = 0; i < n; ++i) {
    if (a[i].salary - anchor > eps) {
      ++g;
      anchor = a[i].salary;
    }
    a[i].salary_group = g;
  }
  qsort(a, n, sizeof(*a), descending ? compare_desc : compare);
  return OK;
}

Status read_employees(FILE *f, Employee **out, size_t *count) {
  if (!f || !out || !count)
    return INVALID;
  *out = NULL;
  *count = 0;
  Employee *a = NULL;
  size_t n = 0;
  Status st = OK;
  char *line = NULL;
  while ((st = read_line(f, &line)) == OK) {
    char *v[4];
    Employee e = {0};
    st = split_fields(line, v, 4);
    if (!st)
      st = parse_uint(v[0], UINTMAX_MAX, &e.id);
    if (!st && (!latin(v[1]) || !latin(v[2])))
      st = INVALID;
    if (!st)
      st = parse_real(v[3], &e.salary);
    if (!st && e.salary < 0)
      st = INVALID;
    if (!st)
      st = copy_text(v[1], &e.name);
    if (!st)
      st = copy_text(v[2], &e.surname);
    free(line);
    line = NULL;
    if (!st && n == SIZE_MAX / sizeof(*a))
      st = RANGE;
    if (!st) {
      Employee *p = realloc(a, (n + 1) * sizeof(*a));
      if (!p)
        st = MEMORY;
      else {
        a = p;
        a[n++] = e;
      }
    }
    if (st) {
      free(e.name);
      free(e.surname);
      break;
    }
  }
  if (st == END)
    st = OK;
  if (st) {
    employees_free(a, n);
    return st;
  }
  *out = a;
  *count = n;
  return OK;
}

int main(int argc, char **argv) {
  if (argc != 4 || !((argv[2][0] == '-' || argv[2][0] == '/') &&
                     (argv[2][1] == 'a' || argv[2][1] == 'd') && !argv[2][2]))
    return report(INVALID);
  FILE *f = fopen(argv[1], "r");
  if (!f)
    return report(IO);
  Employee *a = NULL;
  size_t n = 0;
  Status st = read_employees(f, &a, &n);
  if (fclose(f) && !st)
    st = IO;
  if (!st)
    st = sort_employees(a, n, argv[2][1] == 'd', 1e-9);
  /* Entire input has been read and closed before opening output, even for same
   * path. */
  if (!st) {
    f = fopen(argv[3], "w");
    if (!f)
      st = IO;
    else {
      for (size_t i = 0; i < n && !st; ++i)
        if (fprintf(f, "%" PRIuMAX " %s %s %.17g\n", a[i].id, a[i].name,
                    a[i].surname, a[i].salary) < 0)
          st = IO;
      if (fclose(f) && !st)
        st = IO;
    }
  }
  employees_free(a, n);
  return report(st);
}
