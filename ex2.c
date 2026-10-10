#include <stdio.h>
#include <string.h>
#include <math.h>

enum errors {
    OK,
    ARGC_ERROR,
    PARSE_ERROR,
    RANGE_ERROR
};

static enum errors parse_epsilon(const char *s, double *out_eps) {
    if (s == NULL || out_eps == NULL) {
        return PARSE_ERROR;
    }
    if (*s == '\0') {
        return PARSE_ERROR;
    }
    int i = 0;
    int sign = 1;
    if (s[i] == '-') {
        sign = -1;
        i++;
    } else if (s[i] == '+') {
        i++;
    }
    int digits = 0;
    double result = 0;
    while (s[i] >= '0' && s[i] <= '9') {
        result = result * 10 + (s[i] - '0');
        i++;
        digits++;
    }
    if (s[i] == '.') {
        i++;
        double y = 0.1;
        while (s[i] >= '0' && s[i] <= '9') {
            result += (s[i] - '0') * y;
            y /= 10.0;
            i++;
            digits++;
        }
    }
    if (s[i] != '\0') {
        return PARSE_ERROR;
    }
    if (digits == 0) {
        return PARSE_ERROR;
    }
    result *= sign;
    if (result <= 0) {
        return RANGE_ERROR;
    }
    *out_eps = result;
    return OK;
}

/* log(n!) = sum_{i=2}^{n} log(i). Без lgamma (POSIX). */
static double log_factorial(int n) {
    double s = 0.0;
    for (int i = 2; i <= n; ++i) {
        s += log((double)i);
    }
    return s;
}

static int is_prime(int n) {
    if (n < 2) return 0;
    if (n == 2) return 1;
    if (n % 2 == 0) return 0;
    for (int d = 3; d * d <= n; d += 2) {
        if (n % d == 0) return 0;
    }
    return 1;
}

double limit_e(double eps) {
    double n = 1;
    double current = 0, previous = 0;
    do {
        previous = current;
        n *= 2;
        current = pow(1.0 + 1.0 / n, n);
    } while (fabs(previous - current) >= eps);
    return current;
}

double row_e(double eps) {
    double sum = 1.0;
    double x = 1.0;
    int n = 1;
    do {
        x *= 1.0 / n;
        sum += x;
        n++;
    } while (x >= eps);
    return sum;
}

double equation_e(double eps) {
    double x = 3;
    double f = 1;
    while (fabs(f) > eps) {
        f = log(x) - 1;
        x = x - f * x;
    }
    return x;
}

/* Предел числа Пи по формуле из таблицы:
   pi = lim (2^n * n!)^4 / (n * ((2n)!)^2)
   Вычисляем через логарифмы, чтобы избежать переполнения. */
double limit_pi(double eps) {
    double n = 1;
    double current = 0, previous = 0;
    do {
        previous = current;
        n *= 2;
        double log_num = 4.0 * (n * log(2.0) + log_factorial((int)n));
        double log_den = log(n) + 2.0 * log_factorial((int)(2.0 * n));
        current = exp(log_num - log_den);
    } while (fabs(previous - current) >= eps);
    return current;
}

double row_pi(double eps) {
    double sum = 0;
    double sign = 1;
    int n = 1;
    double x;
    do {
        x = 1.0 / (2 * n - 1);
        sum += sign * x;
        sign = -sign;
        n++;
    } while (x >= eps);
    return 4 * sum;
}

/* Решаем уравнение cos x = -1. Корень кратный (m = 2),
   поэтому используем модифицированный метод Ньютона:
   x = x - m * f(x) / f'(x), где m = 2. */
double equation_pi(double eps) {
    double x = 3.14;
    double f;
    do {
        f = cos(x) + 1.0;
        double df = -sin(x);
        if (fabs(df) < 1e-15) break;
        x = x - 2.0 * f / df;
    } while (fabs(f) >= eps);
    return x;
}

double limit_ln(double eps) {
    double n = 1;
    double current = 0, previous = 0;
    do {
        previous = current;
        n *= 2;
        current = n * (pow(2.0, 1.0 / n) - 1.0);
    } while (fabs(previous - current) >= eps);
    return current;
}

/* Ряд ln 2 сходится очень медленно.
   Ограничиваем число итераций, чтобы избежать зависания. */
double row_ln(double eps) {
    double sum = 0;
    double sign = 1;
    int n = 1;
    double x;
    int max_iter = 10000000;
    do {
        x = 1.0 / n;
        sum += sign * x;
        sign = -sign;
        n++;
        if (n > max_iter) break;
    } while (x >= eps);
    return sum;
}

double equation_ln(double eps) {
    double x = 1;
    double f = 1;
    while (fabs(f) > eps) {
        f = exp(x) - 2;
        x = x - f / exp(x);
    }
    return x;
}

double limit_sqrt(double eps) {
    double current = -0.5;
    double previous = 0;
    while (fabs(current - previous) >= eps) {
        previous = current;
        current = previous - previous * previous / 2 + 1;
    }
    return current;
}

double row_sqrt(double eps) {
    double sum = 0;
    double x;
    int k = 2;
    do {
        x = pow(2.0, -k);
        sum += x;
        k++;
    } while (x >= eps);
    return pow(2.0, sum);
}

double equation_sqrt(double eps) {
    double x = 1;
    double f = 1;
    while (fabs(f) > eps) {
        f = x * x - 2;
        x = x - f / (2 * x);
    }
    return x;
}

static double limit_gamma_sum(int m) {
    double sum = 0;
    double c = 1;
    for (int k = 1; k <= m; ++k) {
        c = c * (m - k + 1) / k;
        double sign = (k % 2 == 0) ? 1.0 : -1.0;
        sum += sign * c * log_factorial(k) / k;
    }
    return sum;
}

double limit_gamma(double eps) {
    int m = 1;
    double current = 0, previous = 0;
    do {
        previous = current;
        m++;
        current = limit_gamma_sum(m);
    } while (fabs(previous - current) >= eps);
    return current;
}

/* Здесь используется acos(-1.0) — это стандартный способ получить pi.*/
double row_gamma(double eps) {
    const double pi = acos(-1.0);
    double pi_sq_over_6 = pi * pi / 6.0;
    double sum = 0;
    int k_max = (int)(1.0 / eps) + 10;
    if (k_max > 10000000) k_max = 10000000;
    for (int k = 2; k <= k_max; ++k) {
        double l = floor(sqrt((double)k));
        sum += 1.0 / (l * l) - 1.0 / k;
    }
    return sum - pi_sq_over_6;
}

double equation_gamma(double eps) {
    int t = 10;
    double current = 0, previous = 0;
    do {
        previous = current;
        t *= 2;
        double product = 1.0;
        for (int p = 2; p <= t; ++p) {
            if (is_prime(p)) {
                product *= (p - 1.0) / p;
            }
        }
        current = log((double)t) * product;
    } while (fabs(previous - current) >= eps);
    return -log(current);
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Используется %s <eps>\n", argv[0]);
        return ARGC_ERROR;
    }
    double eps = 0;
    enum errors err = parse_epsilon(argv[1], &eps);
    switch (err) {
        case OK:
            break;
        case PARSE_ERROR:
            fprintf(stderr, "Некорректное число: '%s'\n", argv[1]);
            return err;
        case RANGE_ERROR:
            fprintf(stderr, "Число должно быть положительным\n");
            return err;
        default:
            fprintf(stderr, "Непредвиденная ситуация\n");
            return err;
    }
    printf("\tПредел\tРяд\tУравнение\n");
    printf("e:       %lf %lf %lf\n", limit_e(eps), row_e(eps), equation_e(eps));
    printf("Pi:      %lf %lf %lf\n", limit_pi(eps), row_pi(eps), equation_pi(eps));
    printf("ln2:     %lf %lf %lf\n", limit_ln(eps), row_ln(eps), equation_ln(eps));
    printf("sqrt(2): %lf %lf %lf\n", limit_sqrt(eps), row_sqrt(eps), equation_sqrt(eps));
    printf("Gamma:   %lf %lf %lf\n", limit_gamma(eps), row_gamma(eps), equation_gamma(eps));
    return OK;
}