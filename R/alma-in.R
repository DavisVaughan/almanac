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

  # Avoid warning with min/max
  if (vec_size(x) == 0L) {
    return(logical())
  }

  # May return corrupt NA values: .Date(Inf) or .Date(-Inf)
  # min(.Date(NA_real_), na.rm = TRUE) == Inf

  min <- min(x, na.rm = TRUE)
  max <- max(x, na.rm = TRUE)

  if (identical(min, global_inf_date)) {
    min <- global_na_date
  }

  if (identical(max, global_neg_inf_date)) {
    max <- global_na_date
  }

  events <- alma_seq_impl(min, max, schedule)

  vec_in(x, events)
}
