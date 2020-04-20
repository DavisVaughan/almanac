#ifndef ALMANAC_ADJUSTMENTS_H
#define ALMANAC_ADJUSTMENTS_H

#include "r.h"

sexp adj_following_impl(sexp x, sexp events);
sexp adj_preceding_impl(sexp x, sexp events);
sexp adj_modified_following_impl(sexp x, sexp events);
sexp adj_modified_preceding_impl(sexp x, sexp events);
sexp adj_nearest_impl(sexp x, sexp events);

double adj_following_one(double x, double* p_begin, double* p_end);
double adj_preceding_one(double x, double* p_begin, double* p_end);
double adj_modified_following_one(double x, double* p_begin, double* p_end);
double adj_modified_preceding_one(double x, double* p_begin, double* p_end);
double adj_nearest_one(double x, double* p_begin, double* p_end);

#endif
