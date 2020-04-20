#include "exports.h"
#include "utils.h"

// -----------------------------------------------------------------------------
#include "adjustments.h"

extern "C" sexp export_adj_following_impl(sexp x, sexp events) {
  return adj_following_impl(x, events);
}

extern "C" sexp export_adj_preceding_impl(sexp x, sexp events) {
  return adj_preceding_impl(x, events);
}

extern "C" sexp export_adj_modified_following_impl(sexp x, sexp events) {
  return adj_modified_following_impl(x, events);
}

extern "C" sexp export_adj_modified_preceding_impl(sexp x, sexp events) {
  return adj_modified_preceding_impl(x, events);
}

extern "C" sexp export_adj_nearest_impl(sexp x, sexp events) {
  return adj_nearest_impl(x, events);
}

// -----------------------------------------------------------------------------
#include "alma-next.h"

extern "C" sexp export_alma_next_impl(sexp x, sexp events, sexp inclusive) {
  const bool inclusive_ = r_lgl_get(inclusive, 0);
  return alma_next_impl(x, events, inclusive_);
}

extern "C" sexp export_alma_previous_impl(sexp x, sexp events, sexp inclusive) {
  const bool inclusive_ = r_lgl_get(inclusive, 0);
  return alma_previous_impl(x, events, inclusive_);
}

// -----------------------------------------------------------------------------
#include "alma-seq.h"

extern "C" sexp export_alma_search_impl(sexp events, sexp from, sexp to, sexp inclusive) {
  const double from_ = r_dbl_get(from, 0);
  const double to_ = r_dbl_get(to, 0);
  const bool inclusive_ = r_lgl_get(inclusive, 0);
  return alma_search_impl(events, from_, to_, inclusive_);
}

// -----------------------------------------------------------------------------
#include "alma-step.h"

extern "C" sexp export_alma_step_impl(sexp x, sexp n, sexp events, sexp size) {
  const r_ssize size_ = r_int_get(size, 0);
  return alma_step_impl(x, n, events, size_);
}

// -----------------------------------------------------------------------------
#include "initialize.h"

extern "C" sexp export_almanac_init() {
  return almanac_init();
}

// -----------------------------------------------------------------------------
#include "months.h"

extern "C" sexp export_test_month_from_days(sexp x) {
  return test_month_from_days(x);
}
