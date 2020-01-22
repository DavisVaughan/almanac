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
  n <- vec_cast(n, integer(), x_arg = "n")
  schedule <- as_schedule(schedule)

  if (length(n) == 1L) {
    alma_step_one(x, n, schedule)
  } else {
    alma_step_multi(x, n, schedule)
  }
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

  cache_preload(x, n, schedule)

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

  # Avoid `max(numeric()) = -Inf`
  # Return `x` after tidy recycling
  if (vec_size(n) == 0L) {
    return(x)
  }

  cache_preload(x, n, schedule)

  # Maximum number of rounds of adjusting needed in either direction
  rounds <- max(abs(n))

  for (i in seq_len(rounds)) {
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

# Pre load the cache with the full range of [x, x + n] (or the reverse if n is
# negative) and an additional amount past that as well to account for
# the adjustments. This significantly speeds up the `alma_adjust()` calls.

cache_preload <- function(x, n, schedule) {
  x_min <- min_date(x)
  x_max <- max_date(x)

  n_with_zero <- c(n, 0)

  n_min <- min(n_with_zero)
  n_max <- max(n_with_zero)

  x_max <- x_max + n_max
  x_min <- x_min + n_min

  # Initial cache to get a gauge on number of events that generally occur
  n_events <- length(alma_seq_impl(x_min, x_max, schedule))

  if (n_events == 0L) {
    return()
  }

  if (n_min >= 0) {
    x_max <- x_max + (n_events * 2)
  } else if (n_max <= 0) {
    x_min <- x_min - (n_events * 2)
  } else {
    x_max <- x_max + (n_events * 2)
    x_min <- x_min - (n_events * 2)
  }

  # Perform a secondary extended cache based on the number of events in the
  # original sequence. This is sort of ad hoc and assumes a uniform event
  # sequence, but captures most cases well
  alma_seq_impl(x_min, x_max, schedule)

  invisible(schedule)
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
