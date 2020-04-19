#' Common adjustments
#'
#' @description
#'
#' This family of `adj_*()` functions encode business logic for common
#' adjustments made in the financial world.
#'
#'   - `adj_following()`
#'
#'     Choose the first non-event date after `x`. This is equivalent to using
#'     an adjustment of `+days(1)`.
#'
#'   - `adj_preceding()`
#'
#'     Choose the first non-event date before `x`. This is equivalent to using
#'     an adjustment of `-days(1)`.
#'
#'   - `adj_modified_following()`
#'
#'     Choose the first non-event date after `x`, unless it falls in a
#'     different month, in which case the first non-event date before `x` is
#'     chosen instead.
#'
#'   - `adj_modified_preceding()`
#'
#'     Choose the first non-event date before `x`, unless it falls in a
#'     different month, in which case the first non-event date after `x` is
#'     chosen instead.
#'
#'   - `adj_nearest()`
#'
#'     Choose the nearest non-event date to `x`. If the closest preceding and
#'     following non-event dates are equally far away, the following non-event
#'     date is chosen.
#'
#' @inheritParams alma_adjust
#'
#' @details
#'
#' For examples, see [alma_adjust()].
#'
#' @name adjustments
NULL

#' @rdname adjustments
#' @export
adj_following <- function(x, schedule) {
  x <- vec_cast_date(x)
  schedule <- as_schedule(schedule)

  events <- schedule$cache$get()

  adj_following_impl(x, events)
}

adj_following_impl <- function(x, events) {
  .Call(export_adj_following_impl, x, events)
}

#' @rdname adjustments
#' @export
adj_preceding <- function(x, schedule) {
  x <- vec_cast_date(x)
  schedule <- as_schedule(schedule)

  events <- schedule$cache$get()

  adj_preceding_impl(x, events)
}

adj_preceding_impl <- function(x, events) {
  .Call(export_adj_preceding_impl, x, events)
}

#' @rdname adjustments
#' @export
adj_modified_following <- function(x, schedule) {
  x <- vec_cast_date(x)
  schedule <- as_schedule(schedule)

  events <- schedule$cache$get()

  out <- adj_following_impl(x, events)

  modify <- month(out) != month(x)

  if (any(modify, na.rm = TRUE)) {
    out[modify] <- adj_preceding_impl(x[modify], events)
  }

  out
}

#' @rdname adjustments
#' @export
adj_modified_preceding <- function(x, schedule) {
  x <- vec_cast_date(x)
  schedule <- as_schedule(schedule)

  events <- schedule$cache$get()

  out <- adj_preceding_impl(x, events)

  modify <- month(out) != month(x)

  if (any(modify, na.rm = TRUE)) {
    out[modify] <- adj_following_impl(x[modify], events)
  }

  out
}

#' @rdname adjustments
#' @export
adj_nearest <- function(x, schedule) {
  x <- vec_cast_date(x)
  schedule <- as_schedule(schedule)

  events <- schedule$cache$get()

  following <- adj_following_impl(x, events)
  preceding <- adj_preceding_impl(x, events)

  dist_following <- as.numeric(following - x)
  dist_preceding <- as.numeric(x - preceding)

  preceding_closer <- dist_following > dist_preceding

  following[preceding_closer] <- preceding[preceding_closer]

  following
}

# ------------------------------------------------------------------------------

# `adj_period_factory()` is used by the default `adjustment` in `alma_adjust()`
# and any adjustment that uses a lubridate period object or an integer number.

# It constructs a function with 2 arguments:
# - x: The set of problem dates that need to be adjusted
# - schedule: The original schedule that will be used to check if the
#   adjusted dates are still events or not

adj_period_factory <- function(period) {
  adj_period <- function(x, schedule) {
    size_period <- vec_size(period)
    is_vectorized_period <- size_period != 1L
    new_period <- period

    # Only recycle x to period size, not period to common size
    # (we don't want to slice `period` repeatedly if we don't have to)
    if (is_vectorized_period) {
      x <- vec_recycle(x, size_period)
    }

    # Locate initial problems
    problem_loc <- which(alma_in(x, schedule))

    # While there are still some events, apply `x + period` and recheck
    while(length(problem_loc) != 0L) {
      if (is_vectorized_period) {
        new_period <- period[problem_loc]
      }

      # Apply adjustment on problems
      problems <- x[problem_loc]
      adjusted <- problems + new_period

      # Overwrite existing problems (use `vec_slice<-` for type/size stability)
      vec_slice(x, problem_loc) <- adjusted

      # Recheck
      still_problem_ind <- alma_in(adjusted, schedule)

      # Update location of problems
      problem_loc <- problem_loc[still_problem_ind]
    }

    x
  }

  adj_period
}
