
# ------------------------------------------------------------------------------

hldy_martin_luther_king_jr_day <- function(adjust_on = NULL, adjustment = NULL) {
  new_hldy(
    "Martin Luther King Jr. Day",
    hldy_martin_luther_king_jr_day_generator,
    adjust_on,
    adjustment
  )
}

hldy_martin_luther_king_jr_day_generator <- function(since, until) {
  hldy_start <- as.Date("1986-01-01")

  # Completely before holiday starts
  if (since < hldy_start && until < hldy_start) {
    rbundle <- rbundle()
    return(rbundle)
  }

  # Straddling holiday start
  if (since < hldy_start && until >= hldy_start) {
    since <- hldy_start
  }

  rrule <- yearly(since = since, until = until)
  rrule <- recur_on_ymonth(rrule, 1L)
  rrule <- recur_on_wday(rrule, 1L, nth = 3L)

  rrule
}

# ------------------------------------------------------------------------------

hldy_christmas <- function(adjust_on = NULL, adjustment = NULL) {
  new_hldy(
    "Christmas",
    hldy_christmas_generator,
    adjust_on,
    adjustment
  )
}

hldy_christmas_generator <- function(since, until) {
  rrule <- yearly(since, until)
  rrule <- recur_on_ymonth(rrule, 12L)
  rrule <- recur_on_mday(rrule, 25L)
  rrule
}

# ------------------------------------------------------------------------------

#' @export
print.hldy <- function(x, ...) {
  print(format(x))
  invisible(x)
}

#' @export
format.hldy <- function(x, ...) {
  name <- hldy_name(x)
  glue("<hldy[{name}]>")
}

hldy_name <- function(x) {
  x$name
}

# ------------------------------------------------------------------------------

new_hldy <- function(name, generator, adjust_on, adjustment) {
  if (!is_string(name)) {
    abort("`name` must be a size 1 character vector.")
  }

  validate_generator(generator)
  validate_adjust_on_and_adjustment(adjust_on, adjustment)

  data <- list(
    name = name,
    generator = generator,
    adjust_on = adjust_on,
    adjustment = adjustment
  )

  structure(data, class = "hldy")
}

# ------------------------------------------------------------------------------

is_hldy <- function(x) {
  inherits(x, "hldy")
}

validate_hldy <- function(x, x_arg = "hldy") {
  if (!is_hldy(x)) {
    glubort("`{x_arg}` must be a hldy.")
  }
  invisible(x)
}

# ------------------------------------------------------------------------------

validate_generator <- function(generator) {
  if (!is_function(generator)) {
    abort("`generator` must be a function.")
  }

  fmls <- fn_fmls(generator)

  if (length(fmls) != 2L) {
    abort("`generator` must have two arguments, `since`, `until`.")
  }

  invisible(generator)
}

validate_adjustment <- function(x, x_arg = "") {
  if (nzchar(x_arg)) {
    x_arg <- glue(" `{x_arg}`")
  }

  if (!is_function(x)) {
    glubort("Input{x_arg} must be a function.")
  }

  fmls <- fn_fmls(x)

  if (length(fmls) != 2L) {
    abort("Input{x_arg} must have two arguments, `x` and `rschedule`.")
  }

  invisible(x)
}
