#include "alma-seq.h"
#include "utils.h"
#include <algorithm>

sexp alma_search_impl(sexp events, double from, double to, bool inclusive) {
  r_ssize size = r_length(events);

  const double* p_begin = r_dbl_deref(events);
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

  r_init_date(out);

  UNPROTECT(1);
  return out;
}
