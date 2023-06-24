/*
 * This file is part of weakc.
 * weakc is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * any later version.
 * weakc is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * You should have received a copy of the GNU General Public License
 * along with weakc. If not, see <https://www.gnu.org/licenses/>.
 */

#include <assert.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <stdio.h>
#include <math.h>

typedef struct tensor {
    uint32_t num_dims;
    uint32_t *dim_sizes;
    double *elements;
} tensor_t;

void rt_memcpy(char *restrict dst, const char *restrict src, size_t size) {
    for (size_t i = 0; i < size; ++i) {
	dst[i] = src[i];
    }
}

void rt_assert(int cond) {
    assert(cond);
}

void *rt_malloc(size_t size) {
    return malloc(size);
}

void rt_print_nil() {
    printf("Nil\n");
}

void rt_print_boolean(int val) {
    if (val) printf("True\n");
    else printf("False\n");
}

void rt_print_string(const char *val) {
    printf("%s\n", val);
}

void rt_print_number_sub(double val) {
    long precision = 1000000000;

    if (val < 0.0) {
	printf("-");
	val = -val;
    }

    val += 0.5 / precision;
    long integral = val;
    long fractional = (val - integral) * precision;
    if (fractional < 0) {
	printf("inf");
    } else {
	printf("%ld.", integral);
	for (long i = precision / 10; i > 1; i /= 10) {
	    if (i > fractional) {
		printf("0");
	    }
	}
	printf("%ld", fractional);
    }
}

void rt_print_number(double val) {
    rt_print_number_sub(val);
    printf("\n");
}

void rt_print_tensor(const tensor_t *val) {
    printf("[");
    size_t total_size = 1;
    for (unsigned i = 0; i < val->num_dims; ++i) {
	total_size *= val->dim_sizes[i];
    }
    for (size_t i = 0; i < total_size - 1; ++i) {
	rt_print_number_sub(val->elements[i]);
	printf(", ");
    }
    rt_print_number_sub(val->elements[total_size - 1]);
    printf("] sa [");
    for (unsigned i = 0; i < val->num_dims - 1; ++i) {
	printf("%u, ", val->dim_sizes[i]);
    }
    printf("%u]\n", val->dim_sizes[val->num_dims - 1]);
}

void *rt_line() {
    char *line = NULL;
    size_t size;
    ssize_t code = getline(&line, &size, stdin);
    assert(code != -1);
    return line;
}

double rt_power_reals(double a, double b) {
    return pow(a, b);
}

tensor_t *rt_shaped_as(const tensor_t *a, const tensor_t *b) {
    tensor_t *r = malloc(sizeof(tensor_t));
    assert(b->num_dims == 1);
    r->num_dims = b->dim_sizes[0];
    r->dim_sizes = malloc(r->num_dims * sizeof(uint32_t));
    size_t num_elements_b = 1;
    for (uint32_t i = 0; i < r->num_dims; ++i) {
	r->dim_sizes[i] = (uint32_t) b->elements[i];
	num_elements_b *= r->dim_sizes[i];
    }
    size_t num_elements_a = 1;
    for (uint32_t i = 0; i < a->num_dims; ++i) {
	num_elements_b *= a->dim_sizes[i];
    }
    assert(num_elements_a == num_elements_b);
    r->elements = malloc(num_elements_b * sizeof(double));
    memcpy(r->elements, a->elements, num_elements_b * sizeof(double));
    return r;
}
