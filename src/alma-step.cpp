#include "alma-step.h"
#include "adjustments.h"
#include "utils.h"

sexp alma_step_impl(sexp x, sexp n, sexp events) {
  const double* p_x = r_dbl_deref(x);
  const int* p_n = r_int_deref(n);

  double* p_events_begin = r_dbl_deref(events);
  double* p_events_end = p_events_begin + r_length(events);

  const r_ssize size = r_length(x);

  sexp out = PROTECT(r_new_vector(REALSXP, size));
  double* p_out = r_dbl_deref(out);

  const bool n_vectorized = r_length(n) != 1;

  for (r_ssize i = 0; i < size; ++i) {
    double elt = p_x[i];

    if (r_dbl_is_missing(elt)) {
      p_out[i] = elt;
      continue;
    }

    // TODO: Assumes positive `n`
    double n_elt = n_vectorized ? p_n[i] : p_n[0];

    for (r_ssize j = 0; j < n_elt; ++j) {
      ++elt;
      elt = adj_following_one(elt, p_events_begin, p_events_end);
    }

    p_out[i] = elt;
  }

  r_poke_attr(out, syms_class, classes_date);

  UNPROTECT(1);
  return out;
}
