#ifndef ALMANAC_NEXT_H
#define ALMANAC_NEXT_H

#include "r.h"

sexp alma_next_impl(sexp x, sexp occurrences, const bool inclusive);
sexp alma_previous_impl(sexp x, sexp occurrences, const bool inclusive);

#endif
