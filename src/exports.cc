#include "exports.h"
#include "utils.h"
#include "alma-seq.h"
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

extern "C" sexp export_almanac_init() {
  return almanac_init();
}
