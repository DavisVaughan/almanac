new_rbundle <- function(rschedules = list(),
                        rdates = new_date(),
                        exdates = new_date(),
                        ...,
                        class = character(),
                        call = caller_env()) {
  vec_check_list(rschedules, call = call)

  check_date(rdates, call = call)
  check_no_missing(rdates, call = call)
  check_finite(rdates, call = call)
  check_date_within_bounds(rdates, call = call)

  check_date(exdates, call = call)
  check_no_missing(exdates, call = call)
  check_finite(exdates, call = call)
  check_date_within_bounds(exdates, call = call)

  new_rschedule(
    rschedules = rschedules,
    rdates = rdates,
    exdates = exdates,
    ...,
    class = c(class, "rbundle")
  )
}
