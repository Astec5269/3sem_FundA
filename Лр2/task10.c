#include <ctype.h>7
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

Status report(Status s);
Status shift_polynomial(double eps, double a, double **out, int degree, ...);

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

Status shift_polynomial(double eps, double a, double **out, int degree, ...) {
  if (!out || degree < 0 || !isfinite(eps) || eps <= 0 || !isfinite(a))
    return INVALID;
  *out = NULL;
  size_t n = (size_t)degree + 1;
  if (n > SIZE_MAX / sizeof(double) || n > SIZE_MAX / sizeof(long double))
    return RANGE;
  double *f = malloc(n * sizeof(*f)), *g = calloc(n, sizeof(*g));
  long double *work = calloc(n, sizeof(*work));
  if (!f || !g || !work) {
    free(f);
    free(g);
    free(work);
    return MEMORY;
  }
  Status st = OK;
  va_list ap;
  va_start(ap, degree);
  for (size_t i = 0; i < n; ++i) {
    f[i] = va_arg(ap, double);
    if (!isfinite(f[i]))
      st = INVALID;
  }
  va_end(ap);
  /* Horner in t=x-a: repeatedly multiply by t+a and add f_i. */
  if (!st) {
    work[0] = f[n - 1];
    size_t used = 0;
    for (size_t i = n - 1; i > 0 && !st; --i) {
      work[used + 1] = work[used];
      for (size_t j = used; j > 0; --j)
        work[j] = work[j - 1] + a * work[j];
      work[0] = a * work[0] + f[i - 1];
      ++used;
      for (size_t j = 0; j <= used; ++j)
        if (!isfinite(work[j]))
          st = RANGE;
    }
    for (size_t j = 0; j < n && !st; ++j) {
      if (fabsl(work[j]) > DBL_MAX)
        st = RANGE;
      else
        g[j] = fabsl(work[j]) <= eps ? 0 : (double)work[j];
    }
  }
  free(f);
  free(work);
  if (st) {
    free(g);
    return st;
  }
  *out = g;
  return OK;
}

int main(int argc, char **argv) {
  (void)argv;
  if (argc != 1)
    return report(INVALID);
  double *g = NULL;
  Status st = shift_polynomial(1e-12, 2.0, &g, 3, 1.0, 2.0, 3.0, 4.0);
  if (!st)
    for (int i = 0; i <= 3; ++i)
      if (printf("g%d = %.12g\n", i, g[i]) < 0)
        st = IO;
  free(g);
  if (!st && fflush(stdout))
    st = IO;
  return report(st);
}
