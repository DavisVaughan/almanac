#include "months.h"
#include "utils.h"
#include <limits>

sexp test_month_from_days(sexp x) {
  const r_ssize size = r_length(x);
  const double* p_x = r_dbl_deref(x);

  sexp out = PROTECT(r_new_vector(INTSXP, size));
  int* p_out = r_int_deref(out);

  for (r_ssize i = 0; i < size; ++i) {
    p_out[i] = month_from_days(p_x[i]);
  }

  UNPROTECT(1);
  return out;
}

/*
 * Extract the [1, 12] month value from the number of days since 1970-01-01.
 * http://howardhinnant.github.io/date_algorithms.html#civil_from_days
 */
const unsigned month_from_days(double x) {
  // Should be extremely rare
  // .Date(.Machine$integer.max - 719468L) == "5879610-09-09"
  if (x > std::numeric_limits<int>::max() - 719468) {
    Rf_errorcall(R_NilValue, "Maximum date value reached. Cannot compute civil months.");
  }

  // .Date(-.Machine$integer.max) == "-5877641-06-24"
  if (x < std::numeric_limits<int>::min() + 1) {
    Rf_errorcall(R_NilValue, "Minimum date value reached. Cannot compute civil months.");
  }

  int days = static_cast<int>(x);

  // Day shift from 1970-01-01 base to 0000-03-01.
  // Gives a [Mar, Feb] year system for easier leap calculations.
  days += 719468;

  // Era (set of 400 years)
  const int era = (days >= 0 ? days : days - 146096) / 146097;

  // Day of era [0, 146096]
  const unsigned doe = static_cast<unsigned>(days - era * 146097);

  // Year of era [0, 399]
  const unsigned yoe = (doe - doe / 1460 + doe / 36524 - doe / 146096) / 365;

  // Day of year [0, 365]
  const unsigned doy = doe - (365 * yoe + yoe / 4 - yoe / 100);

  // Month point [0, 11] in the [Mar, Feb] year system
  const unsigned mp = (5 * doy + 2) / 153;

  // Month [1, 12] in the [Jan, Dec] year system
  const unsigned m = mp + (mp < 10 ? 3 : -9);

  return m;
}
