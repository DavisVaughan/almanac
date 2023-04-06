#' Create a custom rschedule
#'
#' @description
#' `rcustom()` creates an rschedule from manually defined event dates. This can
#' be useful when combined with [runion()] and [rsetdiff()] if you have a set of
#' fixed event dates to forcibly include or exclude from an rschedule.
#'
#' @param events `[Date]`
#'
#'   A vector of event dates.
#'
#' @return
#' A custom rschedule.
#'
#' @export
#' @examples
#' include <- rcustom("2019-07-05")
#' exclude <- rcustom("2019-07-04")
#'
#' independence_day <- yearly() %>%
#'   recur_on_month_of_year("July") %>%
#'   recur_on_day_of_month(4)
#'
#' # Remove forcibly excluded day
#' independence_day <- rsetdiff(independence_day, exclude)
#'
#' # Add forcibly included day
#' independence_day <- runion(independence_day, include)
#'
#' alma_search("2018-01-01", "2020-12-31", independence_day)
rcustom <- function(events) {
  events <- vec_cast_date(events)
  check_no_missing(events)
  check_finite(events)

  events <- vec_unique(events)
  events <- vec_sort(events)

  new_rcustom(events)
}

new_rcustom <- function(events, ..., class = character()) {
  check_date(events)
  new_rschedule(events = events, ..., class = c(class, "almanac_rcustom"))
}

#' @export
print.almanac_rcustom <- function(x, ...) {
  events <- rcustom_events(x)
  events <- as.character(events)

  n <- length(events)

  if (n > 5L) {
    events <- vec_slice(events, 1:5)
    events <- c(events, cli::format_inline("and {n - 5L} more"))
  }

  cli::cli_text("<rcustom[{n}]>")
  cli::cli_ul(events)

  invisible(x)
}

#' @export
rschedule_events.almanac_rcustom <- function(x) {
  rcustom_events(x)
}

rcustom_events <- function(x) {
  x$events
}
