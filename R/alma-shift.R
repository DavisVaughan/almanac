#' Shift dates relative to a schedule
#'
#' @description
#'
#' - `alma_jump()` shifts a sequence of dates by "jumping" from `x` to
#'   `x + jump`. After the jump, [alma_adjust()] is called with the `adjustment`
#'   to ensure that if the new dates are events, they are adjusted to the next
#'   available non-event date.
#'
#' - `alma_step()` steps over a sequence of dates 1 day at a time, for `n` days.
#'   After each step, [alma_adjust()] is called with an adjustment of `days(1)`.
#'   This has different results from `alma_jump()` with a jump of `days(n)`, and
#'   is more appropriate for shifting by "n business days".
#'
#' _The performance of `alma_step()` in particular is highly dependent on the
#' `since` date used in the schedule's recurrence rules. Choosing a date that
#' is close to the start of `x` can dramatically improve performance._
#'
#' @details
#'
#' For shifting by "n business days", `alma_step()` is often more appropriate.
#' Imagine you are on a Friday and want to shift forward 2 days using a
#' schedule that marks weekends as events. There are two options:
#'
#' - `alma_jump()` - Jump forward 2 days to Sunday, and apply the `adjustment`.
#'   Assuming `adjustment = days(1)` was used, that means the result is Monday.
#'
#' - `alma_step()` - Step forward 1 day to Saturday, apply an adjustment of
#'   `days(1)`, which rolls forward to Monday. Step forward 1 day to Tuesday,
#'   and no further adjustment is required.
#'
#' The second option more naturally lends itself to business logic. Two business
#' days from Friday is Tuesday.
#'
#' @param jump `[Period(1) / character(1) / integer(1)]`
#'
#'   A lubridate period object, such as [lubridate::days()] or
#'   [lubridate::years()]. This can also be a character string parsable
#'   by [lubridate::period()], or an integer value for the number of days to
#'   jump. Sub-daily periods are not allowed.
#'
#' @param n `[integer]`
#'
#'   The number of days to step. Can be negative to step backwards.
#'
#' @inheritParams alma_adjust
#'
#' @examples
#' # 2019-09-13 is a Friday
#'
#' # Make a rrule for weekends, and adjust the `since` date to be closer
#' # to our relevant dates in question
#' on_weekends <- daily("2019-09-01") %>% recur_on_weekends()
#'
#' # Note that here we "jump" to Sunday, then adjust, leaving us on Monday
#' alma_jump("2019-09-13", days(2), on_weekends)
#'
#' # Here we step 1 day to Saturday, adjust to Monday,
#' # then step 1 day to Tuesday
#' alma_step("2019-09-13", 2, on_weekends)
#'
#' @export
alma_jump <- function(x, jump, schedule, adjustment = days(1)) {
  x <- vec_cast_date(x)
  schedule <- as_schedule(schedule)
  jump <- check_jump(jump)

  args <- vec_recycle_common(x = x, jump = jump)
  x <- args[[1]]
  jump <- args[[2]]

  x <- x + jump
  x <- alma_adjust(x, schedule, adjustment)

  x
}

#' @rdname alma_jump
#' @export
alma_step <- function(x, n, schedule) {
  x <- vec_cast_date(x)
  schedule <- as_schedule(schedule)

  n <- vec_cast(n, integer(), x_arg = "n")

  if (any(is.na(n))) {
    abort("`n` cannot be `NA`.")
  }

  if (length(n) == 1L) {
    alma_step_one(x, n, schedule)
  } else {
    alma_step_multi(x, n, schedule)
  }
}

alma_step2 <- function(x, n, schedule) {
  x <- vec_cast_date(x)
  schedule <- as_schedule(schedule)

  n <- vec_cast(n, integer(), x_arg = "n")

  if (any(is_missing_or_infinite(n))) {
    abort("`n` cannot be `NA` or infinite.")
  }

  events <- schedule$cache$get()

  alma_step_impl(x, n, events)
}

alma_step_impl <- function(x, n, events) {
  .Call(export_alma_step_impl, x, n, events)
}

alma_step_one <- function(x, n, schedule) {
  # Use integers rather than periods.
  # Avoids SLOW update() function from lubridate
  if (n >= 0) {
    one_day <- 1L # days(1)
  } else {
    one_day <- -1L # days(-1)
  }

  adjuster <- make_adjuster(one_day)

  n <- abs(n)

  for (i in seq_len(n)) {
    x <- x + one_day
    x <- adjuster(x, schedule)
  }

  x
}

alma_step_multi <- function(x, n, schedule) {
  args <- vec_recycle_common(x = x, n = n)

  x <- args[[1]]
  n <- args[[2]]

  # Maximum number of rounds of adjusting needed in either direction
  rounds <- max2(abs(n))

  for (i in seq2(1L, rounds)) {
    # 0 == done, 1 == +1 day, -1 == -1 day
    signs <- sign(n)

    step_loc <- signs != 0L

    # The part of x that still needs to step
    one_day <- signs[step_loc]
    x_to_step <- x[step_loc]

    # Make a vectorized adjuster for post-step adjusting
    adjuster <- make_adjuster(one_day)

    # Step and adjust
    stepped <- x_to_step + one_day
    adjusted <- adjuster(stepped, schedule)

    # Overwrite with newly stepped values
    vec_slice(x, step_loc) <- adjusted

    # Decrement n in the correct direction
    n <- n - signs
  }

  x
}

# ------------------------------------------------------------------------------

check_jump <- function(jump) {
  if (is.character(jump)) {
    jump <- period(jump)
  }

  if (!is.period(jump)) {
    if (is.double(jump) || is.integer(jump)) {
      jump <- vec_cast(jump, integer())
      jump <- days(jump)
      return(jump)
    }

    abort("`jump` must be a period or integer.")
  }

  if (is_subdaily(jump)) {
    abort("`jump` must not contain any sub-daily components.")
  }

  jump
}
