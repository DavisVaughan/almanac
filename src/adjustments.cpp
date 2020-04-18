#include "adjustments.h"
#include <algorithm>

static double* binary_find(double* p_begin, double* p_end, double x);

static bool is_holiday(const double x,
                       const double* p_events_begin,
                       const double* p_events_end);

// -----------------------------------------------------------------------------

double adj_following_one(double x,
                         double* p_events_begin,
                         double* p_events_end) {
  while (is_holiday(x, p_events_begin, p_events_end)) {
    ++x;
  }

  return x;
}

// -----------------------------------------------------------------------------

static bool is_holiday(const double x,
                       const double* p_events_begin,
                       const double* p_events_end) {
  return std::binary_search(p_events_begin, p_events_end, x);
}

static double* binary_find(double* p_begin, double* p_end, double x) {
  double* p_loc = std::lower_bound(p_begin, p_end, x);

  if (p_loc != p_end && x >= *p_loc) {
    return p_loc; // found
  } else {
    return p_end; // not found
  }
}
