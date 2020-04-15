#' Is `x` in the schedule?
#'
#' `alma_in()` checks if `x` is in the set of dates defined by the schedule.
#'
#' @param x `[Date]`
#'
#'    A vector of dates.
#'
#' @param schedule `[schedule / rrule]`
#'
#'    A schedule or rrule.
#'
#' @export
alma_in <- function(x, schedule) {
  x <- vec_cast_date(x)

  schedule <- as_schedule(schedule)

  min <- min_date(x)
  max <- max_date(x)

  events <- alma_seq_impl(min, max, schedule)

  vec_in(x, events)
}

min_date <- function(x) {
  out <- suppressWarnings(min(x, na.rm = TRUE))

  # from
  # `min(.Date(NA_real_), na.rm = TRUE)`
  # or
  # `min(.Date(Inf), na.rm = TRUE)`
  # or
  # `min(.Date(numeric()), na.rm = TRUE)`
  if (identical(out, almanac_global_inf_date)) {
    out <- almanac_global_na_date
  }

  out
}

max_date <- function(x) {
  out <- suppressWarnings(max(x, na.rm = TRUE))

  # from
  # `max(.Date(NA_real_), na.rm = TRUE)`
  # or
  # `max(.Date(Inf), na.rm = TRUE)`
  # or
  # `max(.Date(numeric()), na.rm = TRUE)`
  if (identical(out, almanac_global_neg_inf_date)) {
    out <- almanac_global_na_date
  }

  out
}
