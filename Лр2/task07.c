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
  const double *coordinates;
  size_t dimension;
} Vector;

typedef void (*UniversalNorm)(void);

typedef Status (*NormFunction)(const Vector *, const void *, double, double *);

typedef struct {
  UniversalNorm function;
  const void *context;
} Norm;

typedef struct {
  const double *elements;
  size_t dimension;
} Matrix;

typedef struct {
  Vector *vectors;
  size_t count;
  double value;
} Longest;

Status report(Status s);
static Status valid_vector(const Vector *v, double eps);
Status norm_inf(const Vector *v, const void *context, double eps, double *out);
Status norm_p(const Vector *v, const void *context, double eps, double *out);
Status norm_matrix(const Vector *v, const void *context, double eps,
                   double *out);
void longest_free(Longest *r, size_t n);
Status longest_vectors(size_t dimension, double eps, Longest **out, size_t vc,
                       size_t nc, ...);

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

static Status valid_vector(const Vector *v, double eps) {
  if (!v || !v->coordinates || !v->dimension || !isfinite(eps) || eps <= 0)
    return INVALID;
  for (size_t i = 0; i < v->dimension; ++i)
    if (!isfinite(v->coordinates[i]))
      return INVALID;
  return OK;
}

Status norm_inf(const Vector *v, const void *context, double eps, double *out) {
  (void)context;
  Status st = valid_vector(v, eps);
  if (st || !out)
    return st ? st : INVALID;
  double max = 0;
  for (size_t i = 0; i < v->dimension; ++i) {
    double x = fabs(v->coordinates[i]);
    max = fmax(max, x);
  }
  *out = max;
  return OK;
}

Status norm_p(const Vector *v, const void *context, double eps, double *out) {
  Status st = valid_vector(v, eps);
  if (st || !context || !out)
    return st ? st : INVALID;
  double p = *(const double *)context;
  if (!isfinite(p) || p < 1.0)
    return INVALID;
  double max = 0;
  for (size_t i = 0; i < v->dimension; ++i)
    max = fmax(max, fabs(v->coordinates[i]));
  if (max <= eps) {
    *out = 0;
    return OK;
  }
  long double sum = 0;
  for (size_t i = 0; i < v->dimension; ++i)
    sum += powl(fabsl(v->coordinates[i]) / max, p);
  long double value = max * powl(sum, 1.0L / p);
  if (!isfinite(value) || value > DBL_MAX)
    return RANGE;
  *out = (double)value;
  return OK;
}

/* Определитель ведущего главного блока k x k методом Гаусса.
   Работаем с копией: исходная матрица не изменяется. */
static Status leading_determinant(const Matrix *a, size_t k, double eps,
                                  long double *out) {
  if (!a || !a->elements || !out || !k || k > a->dimension ||
      !isfinite(eps) || eps <= 0)
    return INVALID;
  if (a->dimension > SIZE_MAX / a->dimension ||
      k > SIZE_MAX / k || k * k > SIZE_MAX / sizeof(long double))
    return RANGE;
  long double *b = malloc(k * k * sizeof(*b));
  if (!b)
    return MEMORY;
  Status st = OK;
  for (size_t i = 0; i < k; ++i)
    for (size_t j = 0; j < k; ++j) {
      b[i * k + j] = a->elements[i * a->dimension + j];
      if (!isfinite(b[i * k + j]))
        st = INVALID;
    }
  long double det = 1.0L;
  for (size_t col = 0; col < k && !st; ++col) {
    size_t pivot = col;
    for (size_t row = col + 1; row < k; ++row)
      if (fabsl(b[row * k + col]) - fabsl(b[pivot * k + col]) > eps)
        pivot = row;
    if (fabsl(b[pivot * k + col]) <= eps) {
      det = 0;
      break;
    }
    /* Каждая перестановка строк меняет знак определителя. */
    if (pivot != col) {
      for (size_t j = 0; j < k; ++j) {
        long double t = b[col * k + j];
        b[col * k + j] = b[pivot * k + j];
        b[pivot * k + j] = t;
      }
      det = -det;
    }
    long double diagonal = b[col * k + col];
    det *= diagonal;
    if (!isfinite(det)) {
      st = RANGE;
      break;
    }
    /* Прибавление кратной другой строки не меняет определитель. */
    for (size_t row = col + 1; row < k && !st; ++row) {
      long double factor = b[row * k + col] / diagonal;
      b[row * k + col] = 0;
      for (size_t j = col + 1; j < k; ++j) {
        b[row * k + j] -= factor * b[col * k + j];
        if (!isfinite(b[row * k + j])) {
          st = RANGE;
          break;
        }
      }
    }
  }
  free(b);
  if (!st)
    *out = det;
  return st;
}

/* Критерий Сильвестра для симметричной матрицы:
   все ведущие главные миноры должны быть положительными.
   Значения <= eps считаем недостаточно положительными. */
static Status check_sylvester(const Matrix *a, double eps) {
  if (!a || !a->elements || !a->dimension || !isfinite(eps) || eps <= 0)
    return INVALID;
  size_t n = a->dimension;
  if (n > SIZE_MAX / n)
    return RANGE;
  for (size_t i = 0; i < n; ++i)
    for (size_t j = 0; j <= i; ++j) {
      double x = a->elements[i * n + j];
      double y = a->elements[j * n + i];
      if (!isfinite(x) || !isfinite(y) ||
          fabsl((long double)x - y) > eps)
        return INVALID;
    }
  for (size_t k = 1; k <= n; ++k) {
    long double determinant = 0;
    Status st = leading_determinant(a, k, eps, &determinant);
    if (st)
      return st;
    if (determinant <= eps)
      return INVALID;
  }
  return OK;
}

Status norm_matrix(const Vector *v, const void *context, double eps,
                   double *out) {
  Status st = valid_vector(v, eps);
  if (st || !context || !out)
    return st ? st : INVALID;
  const Matrix *a = context;
  if (a->dimension != v->dimension)
    return INVALID;
  st = check_sylvester(a, eps);
  if (st)
    return st;
  /* После проверки вычисляем (Ax, x) напрямую. */
  size_t n = v->dimension;
  long double q = 0;
  for (size_t i = 0; i < n; ++i) {
    long double ax = 0;
    for (size_t j = 0; j < n; ++j)
      ax += (long double)a->elements[i * n + j] * v->coordinates[j];
    q += ax * v->coordinates[i];
    if (!isfinite(ax) || !isfinite(q))
      return RANGE;
  }
  if (q < -(long double)eps)
    return RANGE;
  q = sqrtl(fmaxl(q, 0.0L));
  if (!isfinite(q) || q > DBL_MAX)
    return RANGE;
  *out = (double)q;
  return OK;
}

void longest_free(Longest *r, size_t n) {
  if (r) {
    for (size_t i = 0; i < n; ++i)
      free(r[i].vectors);
    free(r);
  }
}

Status longest_vectors(size_t dimension, double eps, Longest **out, size_t vc,
                       size_t nc, ...) {
  if (!out || !dimension || !vc || !nc || !isfinite(eps) || eps <= 0)
    return INVALID;
  *out = NULL;
  if (vc > SIZE_MAX / sizeof(Vector) || vc > SIZE_MAX / sizeof(double) ||
      nc > SIZE_MAX / sizeof(Longest))
    return RANGE;
  Vector *v = malloc(vc * sizeof(*v));
  double *values = malloc(vc * sizeof(*values));
  Longest *r = calloc(nc, sizeof(*r));
  if (!v || !values || !r) {
    free(v);
    free(values);
    free(r);
    return MEMORY;
  }
  va_list ap;
  va_start(ap, nc);
  Status st = OK;
  for (size_t i = 0; i < vc; ++i) {
    v[i] = va_arg(ap, Vector);
    if (valid_vector(&v[i], eps) || v[i].dimension != dimension)
      st = INVALID;
  }
  for (size_t j = 0; j < nc && !st; ++j) {
    Norm norm = va_arg(ap, Norm);
    if (!norm.function) {
      st = INVALID;
      break;
    }
    NormFunction f = (NormFunction)norm.function;
    double max = 0;
    for (size_t i = 0; i < vc && !st; ++i) {
      st = f(&v[i], norm.context, eps, &values[i]);
      if (!st && (!isfinite(values[i]) || values[i] < -eps))
        st = INVALID;
      if (!st)
        max = fmax(max, values[i]);
    }
    if (!st) {
      r[j].vectors = malloc(vc * sizeof(Vector));
      if (!r[j].vectors)
        st = MEMORY;
      else {
        r[j].value = max;
        for (size_t i = 0; i < vc; ++i)
          if (fabs(values[i] - max) <= eps)
            r[j].vectors[r[j].count++] = v[i];
      }
    }
  }
  va_end(ap);
  free(v);
  free(values);
  if (st) {
    longest_free(r, nc);
    return st;
  }
  *out = r;
  return OK;
}

int main(int argc, char **argv) {
  (void)argv;
  if (argc != 1)
    return report(INVALID);
  const double x[] = {3, 4}, y[] = {-3, -4}, z[] = {5, 0}, a[] = {2, 0, 0, 1},
               p = 2;
  Vector v1 = {x, 2}, v2 = {y, 2}, v3 = {z, 2};
  Matrix matrix = {a, 2};
  Norm ni = {(UniversalNorm)norm_inf, NULL}, np = {(UniversalNorm)norm_p, &p},
       nm = {(UniversalNorm)norm_matrix, &matrix};
  Longest *r = NULL;
  Status st = longest_vectors(2, 1e-9, &r, 3, 3, v1, v2, v3, ni, np, nm);
  for (size_t i = 0; i < 3 && !st; ++i) {
    if (printf("NORM %zu MAX %.10g COUNT %zu\n", i + 1, r[i].value,
               r[i].count) < 0)
      st = IO;
    for (size_t j = 0; j < r[i].count && !st; ++j)
      if (printf("%.10g %.10g\n", r[i].vectors[j].coordinates[0],
                 r[i].vectors[j].coordinates[1]) < 0)
        st = IO;
  }
  longest_free(r, 3);
  if (!st && fflush(stdout))
    st = IO;
  return report(st);
}
