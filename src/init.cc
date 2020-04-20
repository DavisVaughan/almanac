#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <stdbool.h> // for bool
#include <R_ext/Rdynload.h>

#include "exports.h"

extern "C" {

// .Call entries
static const R_CallMethodDef CallEntries[] = {
  // #include "adjustments.h"
  {"export_adj_following_impl",              (DL_FUNC) &export_adj_following_impl, 2},
  {"export_adj_preceding_impl",              (DL_FUNC) &export_adj_preceding_impl, 2},
  {"export_adj_modified_following_impl",     (DL_FUNC) &export_adj_modified_following_impl, 2},
  {"export_adj_modified_preceding_impl",     (DL_FUNC) &export_adj_modified_preceding_impl, 2},
  {"export_adj_nearest_impl",                (DL_FUNC) &export_adj_nearest_impl, 2},

  // #include "alma-next.h"
  {"export_alma_next_impl",                  (DL_FUNC) &export_alma_next_impl, 3},
  {"export_alma_previous_impl",              (DL_FUNC) &export_alma_previous_impl, 3},

  // #include "alma-seq.h"
  {"export_alma_seq_impl",                   (DL_FUNC) &export_alma_seq_impl, 4},

  // #include "alma-step.h"
  {"export_alma_step_impl",                  (DL_FUNC) &export_alma_step_impl, 4},

  // #include "initialize.h"
  {"export_almanac_init",                    (DL_FUNC) &export_almanac_init, 0},

  // #include "months.h"
  {"export_test_month_from_days",            (DL_FUNC) &export_test_month_from_days, 1},

  {NULL, NULL, 0}
};

void R_init_almanac(DllInfo *dll) {
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
}

} // extern "C"
