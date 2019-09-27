# TODO - Add other adjustment rules from original almanac
# TODO - Add sch_jump() and sch_step()

sch_adjust <- function(x, schedule, adjustment = days(1)) {
  x <- vec_cast_date(x)
  schedule <- as_schedule(schedule)
  adjuster <- make_adjuster(adjustment)

  # Find initial set of events
  problem_loc <- sch_in(x, schedule)

  if (any(problem_loc)) {
    x[problem_loc] <- adjuster(x[problem_loc], schedule)
  }

  x
}

make_adjuster <- function(adjustment) {
  # Already a function?
  if (is_function(adjustment)) {
    if (length(fn_fmls_names(adjustment)) != 2L) {
      abort("An `adjustment` function must have 2 arguments.")
    }
    return(adjustment)
  }

  # character -> period
  if (is.character(adjustment)) {
    adjustment <- period(adjustment)
  }

  # period -> function
  if (is.period(adjustment)) {
    vec_assert(adjustment, size = 1L)

    if (is_subdaily(adjustment)) {
      abort("`adjustment` must not contain any sub-daily components.")
    }

    adjuster <- adj_period_factory(adjustment)
    return(adjuster)
  }

  # integer -> function
  if (is.integer(adjustment) || is.double(adjustment)) {
    adjustment <- vec_cast(adjustment, integer())
    vec_assert(adjustment, size = 1L)

    adjuster <- adj_period_factory(adjustment)
    return(adjuster)
  }

  # formula -> function
  if (is_formula(adjustment, scoped = TRUE, lhs = FALSE)) {
    adjuster <- as_function(adjustment)
    return(adjuster)
  }

  abort("`adjustment` must be a period or a function.")
}

is_subdaily <- function(x) {
  sum(abs(hour(x)), abs(minute(x)), abs(second(x))) != 0L
}

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
