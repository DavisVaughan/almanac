#ifndef ALMANAC_UTILS_H
#define ALMANAC_UTILS_H

#include "r.h"

// -----------------------------------------------------------------------------

extern sexp classes_date;

// -----------------------------------------------------------------------------

extern sexp syms_class;

// -----------------------------------------------------------------------------

static inline sexp r_new_vector(r_type type, r_ssize size) {
  return Rf_allocVector(type, size);
}

// -----------------------------------------------------------------------------

static inline double* r_dbl_deref(sexp x) {
  return REAL(x);
}

static inline int* r_int_deref(sexp x) {
  return INTEGER(x);
}

static inline int* r_lgl_deref(sexp x) {
  return LOGICAL(x);
}

// -----------------------------------------------------------------------------

static inline double r_dbl_get(sexp x, r_ssize i) {
  return r_dbl_deref(x)[i];
}

static inline double r_int_get(sexp x, r_ssize i) {
  return r_int_deref(x)[i];
}

static inline int r_lgl_get(sexp x, r_ssize i) {
  return r_lgl_deref(x)[i];
}

// -----------------------------------------------------------------------------

static inline sexp r_poke_attr(sexp x, sexp sym, sexp value) {
  return Rf_setAttrib(x, sym, value);
}

static inline sexp r_poke_class(sexp x, sexp cls) {
  return r_poke_attr(x, syms_class, cls);
}

// -----------------------------------------------------------------------------

static inline R_xlen_t r_length(sexp x) {
  return Rf_xlength(x);
}

// -----------------------------------------------------------------------------

static inline bool r_dbl_is_missing(double x) {
  return ISNAN(x);
}

static inline bool r_int_is_missing(int x) {
  return x == NA_INTEGER;
}

// -----------------------------------------------------------------------------

static inline void r_init_date(sexp x) {
  r_poke_class(x, classes_date);
}

// -----------------------------------------------------------------------------

#endif
