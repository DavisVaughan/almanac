#ifndef ALMANAC_R_H
#define ALMANAC_R_H

#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>

#define r_null R_NilValue
#define r_ssize R_xlen_t
#define sexp SEXP
#define r_type SEXPTYPE

#define r_int_na NA_INTEGER
#define r_dbl_na NA_REAL

#endif
