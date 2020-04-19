#include "adjustments.h"
#include "utils.h"
#include <algorithm>

static double* binary_find(double* p_begin, double* p_end, double x);

// -----------------------------------------------------------------------------

sexp adj_following_impl(sexp x, sexp events) {
  const r_ssize size = r_length(x);

  sexp out = PROTECT(r_new_vector(REALSXP, size));
  double* p_out = r_dbl_deref(out);

  const double* p_x = r_dbl_deref(x);

  double* p_events_begin = r_dbl_deref(events);
  double* p_events_end = p_events_begin + r_length(events);

  for (r_ssize i = 0; i < size; ++i) {
    p_out[i] = adj_following_one(p_x[i], p_events_begin, p_events_end);
  }

  r_init_date(out);

  UNPROTECT(1);
  return out;
}

double adj_following_one(double x, double* p_begin, double* p_end) {
  // Locate `x` if it is an event
  double* p_x_loc = binary_find(p_begin, p_end, x);

  // Increment by 1 as long as it is still an event
  while (p_x_loc != p_end && x == *p_x_loc) {
    ++x;
    ++p_x_loc;
  }

  return x;
}

// -----------------------------------------------------------------------------

sexp adj_preceding_impl(sexp x, sexp events) {
  const r_ssize size = r_length(x);

  sexp out = PROTECT(r_new_vector(REALSXP, size));
  double* p_out = r_dbl_deref(out);

  const double* p_x = r_dbl_deref(x);

  double* p_events_begin = r_dbl_deref(events);
  double* p_events_end = p_events_begin + r_length(events);

  for (r_ssize i = 0; i < size; ++i) {
    p_out[i] = adj_preceding_one(p_x[i], p_events_begin, p_events_end);
  }

  r_init_date(out);

  UNPROTECT(1);
  return out;
}

double adj_preceding_one(double x, double* p_begin, double* p_end) {
  // Locate `x` if it is an event
  double* p_x_loc = binary_find(p_begin, p_end, x);

  // `x` is not an event
  if (p_x_loc == p_end) {
    return x;
  }

  // Continually step backwards until `x` is either no longer
  // an event, or we step before the start of the events
  double* p_before_begin = p_begin - 1;

  while (p_x_loc != p_before_begin && x == *p_x_loc) {
    --x;
    --p_x_loc;
  }

  return x;
}

// -----------------------------------------------------------------------------

/*
 * Return a pointer to the location of `x` in the range of begin->end. If not
 * found, returns `p_end`.
 *
 * Adapted from:
 * https://stackoverflow.com/questions/446296/where-can-i-get-a-useful-c-binary-search-algorithm
 */
static double* binary_find(double* p_begin, double* p_end, double x) {
  double* p_loc = std::lower_bound(p_begin, p_end, x);

  if (p_loc != p_end && !(x < *p_loc)) {
    // Found
    return p_loc;
  } else {
    // Not found
    return p_end;
  }
}