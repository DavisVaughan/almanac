#ifndef ALMANAC_ADJUSTMENTS_H
#define ALMANAC_ADJUSTMENTS_H

#include "r.h"

sexp adj_following_impl(sexp x, sexp events);
double adj_following_one(double x, double* p_begin, double* p_end);

sexp adj_preceding_impl(sexp x, sexp events);
double adj_preceding_one(double x, double* p_begin, double* p_end);

#endif
