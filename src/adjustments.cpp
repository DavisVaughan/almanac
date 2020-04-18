#include "adjustments.h"
#include <algorithm>

static double* binary_find(double* p_begin, double* p_end, double x);

// -----------------------------------------------------------------------------

double adj_following_one(double x,
                         double* p_events_begin,
                         double* p_events_end) {
  // Locate `x` if it is an event
  double* p_events_x = binary_find(p_events_begin, p_events_end, x);

  // Increment by 1 as long as it is still an event
  while (p_events_x != p_events_end && x == *p_events_x) {
    ++x;
    ++p_events_x;
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
