#include "exports.h"
#include "utils.h"
#include "alma-next.h"
#include "alma-seq.h"
#include "alma-step.h"
#include "initialize.h"

extern "C" sexp export_alma_seq_impl(sexp occurrences,
                                     sexp from,
                                     sexp to,
                                     sexp inclusive) {
  const double from_ = r_dbl_get(from, 0);
  const double to_ = r_dbl_get(to, 0);
  const bool inclusive_ = r_lgl_get(inclusive, 0);

  return alma_seq_impl(occurrences, from_, to_, inclusive_);
}

extern "C" sexp export_alma_next_impl(sexp x, sexp occurrences, sexp inclusive) {
  const bool inclusive_ = r_lgl_get(inclusive, 0);
  return alma_next_impl(x, occurrences, inclusive_);
}

extern "C" sexp export_alma_previous_impl(sexp x, sexp occurrences, sexp inclusive) {
  const bool inclusive_ = r_lgl_get(inclusive, 0);
  return alma_previous_impl(x, occurrences, inclusive_);
}

extern "C" sexp export_alma_step_impl(sexp x, sexp n, sexp events, sexp size) {
  const r_ssize size_ = r_int_get(size, 0);
  return alma_step_impl(x, n, events, size_);
}

extern "C" sexp export_almanac_init() {
  return almanac_init();
}
