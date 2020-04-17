#include "utils.h"

// -----------------------------------------------------------------------------

sexp classes_date = NULL;

// -----------------------------------------------------------------------------

sexp syms_class = NULL;

// -----------------------------------------------------------------------------

static sexp r_new_shared_vector(r_type type, R_xlen_t n) {
  sexp out = r_new_vector(type, n);
  R_PreserveObject(out);
  MARK_NOT_MUTABLE(out);
  return out;
}

static sexp r_string(const char* x) {
  return Rf_mkChar(x);
}

// -----------------------------------------------------------------------------

void almanac_init_utils() {
  classes_date = r_new_shared_vector(STRSXP, 1);
  SET_STRING_ELT(classes_date, 0, r_string("Date"));

  syms_class = R_ClassSymbol;
}
