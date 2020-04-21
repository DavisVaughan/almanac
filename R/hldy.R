
# ------------------------------------------------------------------------------

hldy_martin_luther_king_jr_day <- function(adjustment = adj_nearest) {
  new_hldy(
    "Martin Luther King Jr. Day",
    hldy_martin_luther_king_jr_day_generator,
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

hldy_christmas <- function(adjustment = adj_nearest) {
  new_hldy(
    "Christmas",
    hldy_christmas_generator,
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
format.hldy <- function(x, ...) {
  glue("hldy: {x$name}")
}

#' @export
print.hldy <- function(x, ...) {
  cat(format(x))
  invisible(x)
}

# ------------------------------------------------------------------------------

new_hldy <- function(name, generator, adjustment) {
  if (!is_character(name, n = 1L)) {
    abort("`name` must be a size 1 character vector.")
  }

  generator <- validate_generator(generator)
  adjustment <- validate_adjustment(adjustment, "adjustment")

  data <- list(
    name = name,
    generator = generator,
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

  generator
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
    abort("Input{x_arg} must have two arguments, `x` and `rbundle`.")
  }

  x
}
