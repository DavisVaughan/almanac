#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <stdbool.h> // for bool
#include <R_ext/Rdynload.h>

#include "exports.h"

extern "C" {

// .Call entries
static const R_CallMethodDef CallEntries[] = {
  {"export_alma_seq_impl",      (DL_FUNC) &export_alma_seq_impl, 4},
  {"export_alma_next_impl",     (DL_FUNC) &export_alma_next_impl, 3},
  {"export_alma_previous_impl", (DL_FUNC) &export_alma_previous_impl, 3},
  {"export_almanac_init",       (DL_FUNC) &export_almanac_init, 0},
  {NULL, NULL, 0}
};

void R_init_almanac(DllInfo *dll) {
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
}

} // extern "C"
