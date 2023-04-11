#ifndef ALMANAC_EXPORTS_H
#define ALMANAC_EXPORTS_H

#include "r.h"

// -----------------------------------------------------------------------------
// #include "adjustments.h"

extern "C" sexp export_adj_following_impl(sexp x, sexp events);
extern "C" sexp export_adj_preceding_impl(sexp x, sexp events);
extern "C" sexp export_adj_modified_following_impl(sexp x, sexp events);
extern "C" sexp export_adj_modified_preceding_impl(sexp x, sexp events);
extern "C" sexp export_adj_nearest_impl(sexp x, sexp events);

// -----------------------------------------------------------------------------
// #include "alma-next.h"

extern "C" sexp export_alma_next_impl(sexp x, sexp events, sexp inclusive);
extern "C" sexp export_alma_previous_impl(sexp x, sexp events, sexp inclusive);
extern "C" sexp export_alma_locate_next(sexp x, sexp events, sexp inclusive);
extern "C" sexp export_alma_locate_previous(sexp x, sexp events, sexp inclusive);

// -----------------------------------------------------------------------------
// #include "alma-search.h"

extern "C" sexp export_alma_search_impl(sexp events, sexp from, sexp to, sexp inclusive);

// -----------------------------------------------------------------------------
// #include "alma-step.h"

extern "C" sexp export_alma_step_impl(sexp x, sexp n, sexp events, sexp size);

// -----------------------------------------------------------------------------
// #include "initialize.h"

extern "C" sexp export_almanac_init();

// -----------------------------------------------------------------------------
// #include "months.h"

extern "C" sexp export_test_month_from_days(sexp x);

// -----------------------------------------------------------------------------
#endif
