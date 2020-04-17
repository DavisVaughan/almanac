#ifndef ALMANAC_EXPORTS_HPP
#define ALMANAC_EXPORTS_HPP

#include "r.h"

extern "C" {

sexp export_alma_seq_impl(sexp occurrences,
                          sexp from,
                          sexp to,
                          sexp inclusive);

sexp export_almanac_init();

}

#endif
