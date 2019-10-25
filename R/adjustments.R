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
  adjuster <- adj_period_factory(1L)
  adjuster(x, schedule)
}

#' @rdname adjustments
#' @export
adj_preceding <- function(x, schedule) {
  adjuster <- adj_period_factory(-1L)
  adjuster(x, schedule)
}

#' @rdname adjustments
#' @export
adj_modified_following <- function(x, schedule) {
  out <- alma_adjust(x, schedule, 1L)

  modify <- month(out) != month(x)

  if (any(modify)) {
    out[modify] <- alma_adjust(x[modify], schedule, -1L)
  }

  out
}

#' @rdname adjustments
#' @export
adj_modified_preceding <- function(x, schedule) {
  out <- alma_adjust(x, schedule, -1L)

  modify <- month(out) != month(x)

  if (any(modify)) {
    out[modify] <- alma_adjust(x[modify], schedule, 1L)
  }

  out
}

#' @rdname adjustments
#' @export
adj_nearest <- function(x, schedule) {
  following <- alma_adjust(x, schedule, 1L)
  preceding <- alma_adjust(x, schedule, -1L)

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
    # Everything is an event to start with
    problem_pos <- seq_along(x)

    # While there are still some events, apply `x + period` and recheck
    while(length(problem_pos) != 0L) {
      # Apply adjustment
      problems <- x[problem_pos]
      adjusted <- problems + period

      # Overwrite existing problems (use `vec_slice<-` for type/size stability)
      vec_slice(x, problem_pos) <- adjusted

      # Recheck
      problem_loc <- sch_in(adjusted, schedule)

      # Update location of problems
      problem_pos <- problem_pos[problem_loc]
    }

    x
  }

  adj_period
}
