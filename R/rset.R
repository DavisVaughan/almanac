#' Create a new set-based recurrence schedule
#'
#' @description
#' Often, a single rrule will be sufficient. However, more complex
#' recurrence objects can be constructed by combining multiple rschedules into
#' a _recurrence set_.
#'
#' There are three types of recurrence sets provided in almanac, each of
#' which construct their event sets by performing a set operation on the
#' underlying events of the rschedules in the set.
#'
#' - `runion()` takes the union.
#'
#' - `rintersect()` takes the intersection.
#'
#' - `rsetdiff()` takes the set difference.
#'
#' @details
#' For `rsetdiff()`, the event set is created "from left to right" and depends
#' on the order that the rschedules were added to the set.
#'
#' @param ... `[rschedules]`
#'
#'   rschedule objects to add to the set.
#'
#' @return
#' A runion, rintersect, or rsetdiff.
#'
#' @name rset
#' @examples
#' since <- "2019-04-01"
#' until <- "2019-05-31"
#'
#' on_weekends <- weekly(since = since, until = until) %>%
#'   recur_on_weekends()
#'
#' on_25th <- monthly(since = since, until = until) %>%
#'   recur_on_day_of_month(25)
#'
#' # On weekends OR the 25th of the month
#' ru <- runion(on_weekends, on_25th)
#' alma_events(ru)
#'
#' # On weekends AND the 25th of the month
#' ri <- rintersect(on_weekends, on_25th)
#' alma_events(ri)
#'
#' # On weekends AND NOT the 25th of the month
#' rsd1 <- rsetdiff(on_weekends, on_25th)
#' alma_events(rsd1)
#'
#' # On the 25th of the month AND NOT the weekend
#' rsd2 <- rsetdiff(on_25th, on_weekends)
#' alma_events(rsd2)
NULL

# ------------------------------------------------------------------------------

#' @rdname rset
#' @export
runion <- function(...) {
  rschedules <- list2(...)
  list_check_all_rschedules(rschedules)
  new_runion(rschedules = rschedules)
}

new_runion <- function(
  rschedules = list(),
  rdates = new_date(),
  exdates = new_date(),
  ...,
  class = character()
) {
  cache <- cache_runion$new(
    rschedules = rschedules,
    rdates = rdates,
    exdates = exdates
  )

  new_rset(
    rschedules = rschedules,
    rdates = rdates,
    exdates = exdates,
    cache = cache,
    ...,
    class = c(class, "almanac_runion")
  )
}

#' @rdname rset
#' @export
rintersect <- function(...) {
  rschedules <- list2(...)
  list_check_all_rschedules(rschedules)
  new_rintersect(rschedules = rschedules)
}

new_rintersect <- function(
  rschedules = list(),
  rdates = new_date(),
  exdates = new_date(),
  ...,
  class = character()
) {
  cache <- cache_rintersect$new(
    rschedules = rschedules,
    rdates = rdates,
    exdates = exdates
  )

  new_rset(
    rschedules = rschedules,
    rdates = rdates,
    exdates = exdates,
    cache = cache,
    ...,
    class = c(class, "almanac_rintersect")
  )
}

#' @rdname rset
#' @export
rsetdiff <- function(...) {
  rschedules <- list2(...)
  list_check_all_rschedules(rschedules)
  new_rsetdiff(rschedules = rschedules)
}

new_rsetdiff <- function(
  rschedules = list(),
  rdates = new_date(),
  exdates = new_date(),
  ...,
  class = character()
) {
  cache <- cache_rsetdiff$new(
    rschedules = rschedules,
    rdates = rdates,
    exdates = exdates
  )

  new_rset(
    rschedules = rschedules,
    rdates = rdates,
    exdates = exdates,
    cache = cache,
    ...,
    class = c(class, "almanac_rsetdiff")
  )
}

new_rset <- function(
  rschedules = list(),
  rdates = new_date(),
  exdates = new_date(),
  ...,
  class = character(),
  call = caller_env()
) {
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
    class = c(class, "almanac_rset")
  )
}

# ------------------------------------------------------------------------------

#' @export
rschedule_events.almanac_runion <- function(x) {
  x$cache$get_events()
}

#' @export
rschedule_events.almanac_rintersect <- function(x) {
  x$cache$get_events()
}

#' @export
rschedule_events.almanac_rsetdiff <- function(x) {
  x$cache$get_events()
}

# ------------------------------------------------------------------------------

#' @export
print.almanac_runion <- function(x, ...) {
  print_rset(x, "runion")
}

#' @export
print.almanac_rintersect <- function(x, ...) {
  print_rset(x, "rintersect")
}

#' @export
print.almanac_rsetdiff <- function(x, ...) {
  print_rset(x, "rsetdiff")
}

print_rset <- function(x, name) {
  rschedules <- x$rschedules
  n <- length(rschedules)

  cli::cli_text(cli::format_inline("<{name}[{n}]>"))

  for (i in seq_len(n)) {
    cli_indented()
    print(rschedules[[i]])
    cli::cli_end()
  }

  invisible(x)
}
