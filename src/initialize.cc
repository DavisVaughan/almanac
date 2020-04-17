#include "initialize.h"
#include "utils.h"

extern void almanac_init_utils();

sexp almanac_init() {
  almanac_init_utils();
  return r_null;
}
