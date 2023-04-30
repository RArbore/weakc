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
#include <stdio.h>

typedef struct tensor {
    unsigned num_dims;
    unsigned *dim_sizes;
    double *elements;
} tensor;

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

void rt_print_number(double val) {
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
    printf("\n");
}

void rt_print_tensor(const tensor *val) {
    printf("[");
    size_t total_size = 1;
    for (unsigned i = 0; i < val->num_dims; ++i) {
	total_size *= val->dim_sizes[i];
    }
    for (size_t i = 0; i < total_size - 1; ++i) {
	printf("%f, ", val->elements[i]);
    }
    printf("%f] sa [", val->elements[total_size - 1]);
    for (unsigned i = 0; i < val->num_dims - 1; ++i) {
	printf("%u, ", val->dim_sizes[i]);
    }
    printf("%u]\n", val->dim_sizes[val->num_dims - 1]);
}
