#ifndef ALMANAC_EXPORTS_HPP
#define ALMANAC_EXPORTS_HPP

#include "r.h"

extern "C" {

  sexp export_alma_seq_impl(sexp occurrences,
                            sexp from,
                            sexp to,
                            sexp inclusive);

  sexp export_alma_next_impl(sexp x, sexp occurrences, sexp inclusive);
  sexp export_alma_previous_impl(sexp x, sexp occurrences, sexp inclusive);

  sexp export_alma_step_impl(sexp x, sexp n, sexp events, sexp size);

  sexp export_adj_following_impl(sexp x, sexp events);
  sexp export_adj_preceding_impl(sexp x, sexp events);

  sexp export_almanac_init();

}

#endif
