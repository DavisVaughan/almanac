#ifndef ALMANAC_NEXT_HPP
#define ALMANAC_NEXT_HPP

#include "r.h"

sexp alma_next_impl(sexp x, sexp occurrences, const bool inclusive);
sexp alma_previous_impl(sexp x, sexp occurrences, const bool inclusive);

#endif
