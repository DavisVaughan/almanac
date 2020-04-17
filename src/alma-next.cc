#include "alma-next.h"
#include "utils.h"
#include <algorithm>

sexp alma_next_impl(sexp x, sexp occurrences, const bool inclusive) {
  r_ssize size = r_length(x);

  const double* p_x = r_dbl_deref(x);

  const double* p_begin = r_dbl_deref(occurrences);
  const double* p_end = p_begin + r_length(occurrences);

  sexp out = PROTECT(r_new_vector(REALSXP, size));
  double* p_out = r_dbl_deref(out);

  for (r_ssize i = 0; i < size; ++i) {
    const double elt = p_x[i];

    if (r_dbl_is_missing(elt)) {
      p_out[i] = elt;
      continue;
    }

    const double* p_loc;

    if (inclusive) {
      p_loc = std::lower_bound(p_begin, p_end, elt);
    } else {
      p_loc = std::upper_bound(p_begin, p_end, elt);
    }

    // No events after `elt`
    if (p_loc == p_end) {
      p_out[i] = r_dbl_na;
      continue;
    }

    p_out[i] = *p_loc;
  }

  r_poke_attr(out, syms_class, classes_date);

  UNPROTECT(1);
  return out;
}

// Very similar to `alma_next_impl()`, but uses an adjustment to upper/lower_bound
// https://stackoverflow.com/questions/9989731/algorithm-function-for-finding-last-item-less-than-or-equal-to-like-lower-bou

sexp alma_previous_impl(sexp x, sexp occurrences, const bool inclusive) {
  r_ssize size = r_length(x);

  const double* p_x = r_dbl_deref(x);

  const double* p_begin = r_dbl_deref(occurrences);
  const double* p_end = p_begin + r_length(occurrences);

  sexp out = PROTECT(r_new_vector(REALSXP, size));
  double* p_out = r_dbl_deref(out);

  for (r_ssize i = 0; i < size; ++i) {
    const double elt = p_x[i];

    if (r_dbl_is_missing(elt)) {
      p_out[i] = elt;
      continue;
    }

    const double* p_loc;

    if (inclusive) {
      p_loc = std::upper_bound(p_begin, p_end, elt);
    } else {
      p_loc = std::lower_bound(p_begin, p_end, elt);
    }

    // No events before `elt`
    if (p_loc == p_begin) {
      p_out[i] = r_dbl_na;
      continue;
    }

    // All important decrement that makes this work
    --p_loc;

    p_out[i] = *p_loc;
  }

  r_poke_attr(out, syms_class, classes_date);

  UNPROTECT(1);
  return out;
}
