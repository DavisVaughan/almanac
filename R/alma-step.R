#' Step relative to an rschedule
#'
#' @description
#' `alma_step()` is useful for shifting dates by "n business days".
#'
#' `alma_step()` steps over a sequence of dates 1 day at a time, for `n` days.
#' After each step, an adjustment is applied to shift to the next non-event
#' date.
#'
#'   - If `n` is positive, [adj_following()] is called.
#'
#'   - If `n` is negative, [adj_preceding()] is called.
#'
#'   - If `n` is zero, it was arbitrarily decided to call [adj_following()] to
#'     roll to the next available non-event date.
#'
#' @details
#' Imagine you are on a Friday and want to shift forward 2 days using an
#' rrule that marks weekends as events. `alma_step()` works like this:
#'
#' - Step forward 1 day to Saturday.
#'
#' - Apply an adjustment of [adj_following()], which rolls forward to Monday.
#'
#' - Step forward 1 day to Tuesday.
#'
#' - Apply an adjustment of [adj_following()], but no adjustment is required.
#'
#' This lends itself naturally to business logic. Two business days from Friday
#' is Tuesday.
#'
#' @inheritParams adj_following
#'
#' @param n `[integer]`
#'
#'   The number of days to step. Can be negative to step backwards.
#'
#' @return
#' A Date vector the same size as `x` shifted by `n` steps.
#'
#' @examples
#' # Make a rrule for weekends
#' on_weekends <- weekly() %>%
#'   recur_on_weekends()
#'
#' # "Step forward by 2 business days"
#' # 2019-09-13 is a Friday.
#' # Here we:
#' # - Step 1 day to Saturday
#' # - Adjust to Monday
#' # - Step 1 day to Tuesday
#' alma_step("2019-09-13", 2, on_weekends)
#'
#' # If Monday, 2019-09-16, was a recurring holiday, we could create
#' # a custom runion and step over that too.
#' on_09_16 <- yearly() %>%
#'   recur_on_ymonth(9) %>%
#'   recur_on_mday(16)
#'
#' rb <- runion() %>%
#'   add_rschedule(on_09_16) %>%
#'   add_rschedule(on_weekends)
#'
#' alma_step("2019-09-13", 2, rb)
#' @export
alma_step <- function(x, n, rschedule) {
  x <- vec_cast_date(x)
  n <- vec_cast(n, integer(), x_arg = "n")

  # Get the common size with nice errors, recycled cheaply internally
  size <- vec_size_common(x = x, n = n)

  validate_rschedule(rschedule, "rschedule")
  events <- rschedule_events(rschedule)

  alma_step_impl(x, n, events, size)
}

alma_step_impl <- function(x, n, events, size) {
  .Call(export_alma_step_impl, x, n, events, size)
}
