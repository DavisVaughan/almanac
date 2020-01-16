#' Create a business day period
#'
#' @export
bdays <- function(n, schedule) {
  n <- vec_cast(n, integer(), x_arg = "n")
  schedule <- as_schedule(schedule)

  new_bdays(n, schedule)
}

new_bdays <- function(n = integer(), schedule = new_schedule()) {
  if (!is.integer(n)) {
    abort("`n` must be an integer")
  }

  if (!is_schedule(schedule)) {
    abort("`schedule` must be a schedule")
  }

  new_vctr(
    .data = n,
    schedule = schedule,
    class = "almanac_bdays",
    inherit_base_type = FALSE
  )
}

get_bdays_schedule <- function(x) {
  attr(x, "schedule")
}

get_bdays_n <- function(x) {
  vec_data(x)
}

# ------------------------------------------------------------------------------

#' @rdname bdays
#' @export vec_arith.almanac_bdays
#' @method vec_arith almanac_bdays
#' @export
vec_arith.almanac_bdays <- function(op, x, y, ...) {
  UseMethod("vec_arith.almanac_bdays", y)
}

#' @method vec_arith.almanac_bdays default
#' @export
vec_arith.almanac_bdays.default <- function(op, x, y, ...) {
  stop_incompatible_op(op, x, y)
}

#' @method vec_arith.almanac_bdays MISSING
#' @export
vec_arith.almanac_bdays.MISSING <- function(op, x, y, ...) {
  switch(
    op,
    `+` = x,
    `-` = new_bdays(-get_bdays_n(x), get_bdays_schedule(x)),
    stop_incompatible_op(op, x, y)
  )
}

#' @method vec_arith.almanac_bdays almanac_bdays
#' @export
vec_arith.almanac_bdays.almanac_bdays <- function(op, x, y, ...) {
  switch(
    op,
    `+` = op_arith_bdays_bdays(`+`, x, y),
    `-` = op_arith_bdays_bdays(`-`, x, y),
    stop_incompatible_op(op, x, y)
  )
}

op_arith_bdays_bdays <- function(op, x, y) {
  check_identical_schedules(x, y)

  sch <- get_bdays_schedule(x)

  x <- get_bdays_n(x)
  y <- get_bdays_n(y)

  args <- vec_recycle_common(x, y)

  n <- op(args[[1]], args[[2]])

  new_bdays(n, schedule = sch)
}

#' @method vec_arith.almanac_bdays numeric
#' @export
vec_arith.almanac_bdays.numeric <- function(op, x, y, ...) {
  switch(
    op,
    `+` = op_arith_bdays_numeric(`+`, x, y),
    `-` = op_arith_bdays_numeric(`-`, x, y),
    `*` = op_arith_bdays_numeric(`*`, x, y),
    stop_incompatible_op(op, x, y)
  )
}

op_arith_bdays_numeric <- function(op, x, y) {
  sch <- get_bdays_schedule(x)

  x <- get_bdays_n(x)
  y <- get_bdays_n(y)

  if (!is_one_dim(y)) {
    abort("`y` must be 1 dimensional")
  }

  y <- vec_cast(y, integer(), x_arg = "y")

  args <- vec_recycle_common(x, y)

  n <- op(args[[1]], args[[2]])

  new_bdays(n, schedule = sch)
}

#' @method vec_arith.numeric almanac_bdays
#' @export
vec_arith.numeric.almanac_bdays <- function(op, x, y, ...) {
  switch(
    op,
    `+` = op_arith_numeric_bdays(`+`, x, y),
    `-` = op_arith_numeric_bdays(`-`, x, y),
    `*` = op_arith_numeric_bdays(`*`, x, y),
    stop_incompatible_op(op, x, y)
  )
}

op_arith_numeric_bdays <- function(op, x, y) {
  sch <- get_bdays_schedule(y)

  x <- get_bdays_n(x)
  y <- get_bdays_n(y)

  if (!is_one_dim(x)) {
    abort("`x` must be 1 dimensional")
  }

  x <- vec_cast(x, integer(), x_arg = "x")

  args <- vec_recycle_common(x, y)

  n <- op(args[[1]], args[[2]])

  new_bdays(n, schedule = sch)
}

#' @method vec_arith.almanac_bdays Date
#' @export
vec_arith.almanac_bdays.Date <- function(op, x, y, ...) {
  switch(
    op,
    `+` = op_arith_bdays_Date(x, y, plus = TRUE),
    `-` = op_arith_bdays_Date(x, y, plus = FALSE),
    stop_incompatible_op(op, x, y)
  )
}

op_arith_bdays_Date <- function(x, y, plus) {
  n <- get_bdays_n(x)
  sch <- get_bdays_schedule(x)

  args <- vec_recycle_common(n, y)

  n <- args[[1]]
  y <- args[[2]]

  if (!plus) {
    n <- -n
  }

  alma_step(y, n, sch)
}

# Note that `bdays(1) - Sys.Date()` is not defined!

#' @method vec_arith.Date almanac_bdays
#' @export
vec_arith.Date.almanac_bdays <- function(op, x, y, ...) {
  switch(
    op,
    `+` = op_plus_Date_bdays(x, y),
    stop_incompatible_op(op, x, y)
  )
}

op_plus_Date_bdays <- function(x, y) {
  n <- get_bdays_n(y)
  sch <- get_bdays_schedule(y)

  args <- vec_recycle_common(n, x)

  n <- args[[1]]
  x <- args[[2]]

  alma_step(x, n, sch)
}

# Waiting on https://github.com/tidyverse/lubridate/issues/721
# So we can have vec_arith.Period.almanac_bdays. Otherwise the
# support is incomplete
# vec_arith.almanac_bdays.Period <- function(op, x, y, ...) {
#   # Allow Periods with just days to be added here
# }

# ------------------------------------------------------------------------------

check_identical_schedules <- function(x, y) {
  x_sch <- get_bdays_schedule(x)
  y_sch <- get_bdays_schedule(y)

  if (!identical(x_sch, y_sch)) {
    abort("`x` and `y` must have identical schedules to perform arithmetic on them.")
  }

  invisible()
}

is_one_dim <- function(x) {
  vec_dims(x) == 1L
}

vec_dims <- function(x) {
  dim <- dim(x)

  if (is.null(dim)) {
    1L
  } else {
    length(dim)
  }
}
