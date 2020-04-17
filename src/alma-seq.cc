#include "alma-seq.h"
#include "utils.h"
#include <algorithm>

sexp alma_seq_impl(sexp occurrences, double from, double to, bool inclusive) {
  r_ssize size = r_length(occurrences);

  const double* p_begin = r_dbl_deref(occurrences);
  const double* p_end = p_begin + size;

  const double* p_start;
  const double* p_stop;

  if (inclusive) {
    p_start = std::lower_bound(p_begin, p_end, from);
    p_stop = std::upper_bound(p_start, p_end, to);
  } else {
    p_start = std::upper_bound(p_begin, p_end, from);
    p_stop = std::lower_bound(p_start, p_end, to);
  }

  r_ssize out_size = p_stop - p_start;

  sexp out = PROTECT(r_new_vector(REALSXP, out_size));
  double* p_out = r_dbl_deref(out);

  std::copy(p_start, p_stop, p_out);

  r_poke_attr(out, syms_class, classes_date);

  UNPROTECT(1);
  return out;
}
