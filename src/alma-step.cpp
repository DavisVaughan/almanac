#include "alma-step.h"
#include "adjustments.h"
#include "utils.h"

static double alma_step_forward(double x, int n, double* p_begin, double* p_end);
static double alma_step_backward(double x, int n, double* p_begin, double* p_end);

// -----------------------------------------------------------------------------

sexp alma_step_impl(sexp x, sexp n, sexp events, r_ssize size) {
  const double* p_x = r_dbl_deref(x);
  const int* p_n = r_int_deref(n);

  double* p_events_begin = r_dbl_deref(events);
  double* p_events_end = p_events_begin + r_length(events);

  sexp out = PROTECT(r_new_vector(REALSXP, size));
  double* p_out = r_dbl_deref(out);

  const bool x_vectorized = r_length(x) != 1;
  const bool n_vectorized = r_length(n) != 1;

  for (r_ssize i = 0; i < size; ++i) {
    double elt = x_vectorized ? p_x[i] : p_x[0];
    int n_elt = n_vectorized ? p_n[i] : p_n[0];

    if (r_dbl_is_missing(elt)) {
      p_out[i] = elt;
      continue;
    }
    if (r_int_is_missing(n_elt)) {
      p_out[i] = NA_REAL;
      continue;
    }

    if (n_elt >= 0) {
      elt = alma_step_forward(elt, n_elt, p_events_begin, p_events_end);
    } else {
      n_elt = std::abs(n_elt);
      elt = alma_step_backward(elt, n_elt, p_events_begin, p_events_end);
    }

    p_out[i] = elt;
  }

  r_poke_attr(out, syms_class, classes_date);

  UNPROTECT(1);
  return out;
}

// -----------------------------------------------------------------------------

static double alma_step_forward(double x, int n, double* p_begin, double* p_end) {
  for (r_ssize i = 0; i < n; ++i) {
    ++x;
    x = adj_following_one(x, p_begin, p_end);
  }

  return x;
}

static double alma_step_backward(double x, int n, double* p_begin, double* p_end) {
  for (r_ssize i = 0; i < n; ++i) {
    --x;
    x = adj_previous_one(x, p_begin, p_end);
  }

  return x;
}
