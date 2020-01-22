#' Create a business day period
#'
#' @description
#' `bdays()` is similar to [lubridate::days()] in that it constructs an object
#' that can be added to Dates to shift by a set period. It differs in the fact
#' that you can provide a `schedule` to shift relative to.
#'
#' When using `bdays()`, remember that the `schedule` defines events that should
#' be "stepped over". This is powerful, but should be used with some thought,
#' as it is possible to get into an infinite loop. For example, using a
#' `daily()` schedule with `as.Date("2019-01-01") + bdays(1, daily())` would
#' define every day as an "event" to step over. Since we will never advance to
#' a non-event day, we get stuck in an endless loop.
#'
#' @details
#' When added to a Date, this internally calls [alma_step()], meaning that an
#' adjustment is performed after every 1 day step. This is likely the behavior
#' that is desired for shifting by "n business days".
#'
#' @param x `[integer]`
#'
#'   The number of days to step. Can be negative to step backwards.
#'
#' @inheritParams alma_step
#'
#' @export
#' @examples
#' # Create a recurrence rule for weekends
#' on_weekends <- weekly() %>%
#'   recur_on_weekends()
#'
#' # A Thursday and Friday
#' x <- as.Date(c("1970-01-01", "1970-01-02"))
#'
#' # lubridate::days() just shifts forward by 2 days, without paying
#' # attention to the weekend
#' x + days(2)
#'
#' # With bdays(), you can specify a schedule to step relative to. This steps
#' # over the weekend, landing us on Monday and Tuesday.
#' x + bdays(2, on_weekends)
#'
#' # It is also vectorized, and arithmetic is defined on integer input
#' business_days <- bdays(2, on_weekends) * c(2, 3)
#' business_days
#'
#' x + business_days
#'
#' x - business_days
bdays <- function(x, schedule) {
  x <- vec_cast(x, integer(), x_arg = "x")
  schedule <- as_schedule(schedule)

  new_bdays(x, schedule)
}

new_bdays <- function(x = integer(), schedule = new_schedule()) {
  if (!is.integer(x)) {
    abort("`x` must be an integer")
  }

  if (!is_schedule(schedule)) {
    abort("`schedule` must be a schedule")
  }

  new("BDays", x, schedule = schedule)
}

# ------------------------------------------------------------------------------
# S4 constructor

# - Required to be able to set it as a slot in bdays
# - Required to be before the call to setClass("bdays")
setOldClass("schedule")

#' Business Days
#'
#' @description
#' BDays is an S4 class that contains a shift in business days, relative to
#' a schedule. The S4 nature of BDays is an implementation detail, and should
#' mostly be invisible to users.
#'
#' @details
#' BDays objects have two slots:
#'
#' - `.Data` `[integer]`
#'
#'   The number of business days to shift by.
#'
#' - `schedule` `[schedule]`
#'
#'   The schedule to shift relative to.
#'
#' BDays place the number of business days in the `.Data` slot so that it can
#' "contain" (inherit from) `"integer"`. This makes it look like an integer
#' to vctrs and base methods like `length()`.
#'
#' @export
#' @keywords internal
#' @examples
#' new("BDays", 1L, schedule = schedule())
setClass(
  "BDays",
  contains = "integer",
  slots = c(schedule = "schedule")
)

get_bdays_schedule <- function(x) {
  x@schedule
}

get_bdays_days <- function(x) {
  x@.Data
}

#' Internal page for hidden aliases
#'
#' For S4 methods that require a documentation entry but only clutter the index.
#'
#' @usage NULL
#' @format NULL
#' @keywords internal
#' @name hidden-aliases
#'
#' @aliases
#' +,ANY,BDays-method
#' +,BDays,ANY-method
#' +,BDays,BDays-method
#' +,BDays,Period-method
#' +,Period,BDays-method
#' -,ANY,BDays-method
#' -,BDays,ANY-method
#' -,BDays,BDays-method
#' -,BDays,Period-method
#' -,Period,BDays-method
#' *,ANY,BDays-method
#' *,BDays,ANY-method
#' *,BDays,BDays-method
#' *,BDays,Period-method
#' *,Period,BDays-method
#' /,ANY,BDays-method
#' /,BDays,ANY-method
#' /,BDays,BDays-method
#' /,BDays,Period-method
#' /,Period,BDays-method
#' show,BDays-method
#' xtfrm,BDays-method
NULL

# ------------------------------------------------------------------------------
# Printing

#' @export
setMethod("show", signature(object = "BDays"), function(object) {
  x <- get_bdays_days(object)
  schedule <- get_bdays_schedule(object)

  cat_line(glue("<bdays<{sch_summary(schedule)}>>"))

  if (length(x) == 0L) {
    return(invisible(object))
  }

  print(format(object), quote = FALSE)

  invisible(object)
})

#' @export
format.BDays <- function(x, ...) {
  x <- get_bdays_days(x)
  format(x)
}

cat_line <- function(...) {
  cat(paste0(..., "\n", collapse = ""))
}

# ------------------------------------------------------------------------------
# Miscellaneous methods

#' @export
xtfrm.BDays <- function(x) {
  xtfrm(get_bdays_days(x))
}

# ------------------------------------------------------------------------------
# `+`

# - ANY methods are generally used, handing off to vec_arith()
# - A few extra methods are required to resolve ambiguity, otherwise we get
#   Notes about what method was chosen when an error happens

#' @export
setMethod(
  "+",
  signature(e1 = "BDays", e2 = "ANY"),
  function(e1, e2) {
    if (missing(e2)) {
      vec_arith("+", e1, MISSING())
    } else {
      vec_arith("+", e1, e2)
    }
  }
)

#' @export
setMethod(
  "+",
  signature(e1 = "ANY", e2 = "BDays"),
  function(e1, e2) vec_arith("+", e1, e2)
)

# Resolve ambiguity from:
# sch1 <- schedule()
# sch2 <- schedule()
# new_bdays(1L, sch1) + new_bdays(1L, sch2)

#' @export
setMethod(
  "+",
  signature(e1 = "BDays", e2 = "BDays"),
  function(e1, e2) vec_arith("+", e1, e2)
)

# Resolve ambiguity from the following,
# since Period objects have an ANY method:
# new_bdays(1L) + days(1L)
# days(1L) + new_bdays(1L)

#' @export
setMethod(
  "+",
  signature(e1 = "BDays", e2 = "Period"),
  function(e1, e2) stop_incompatible_op("+", e1, e2)
)

#' @export
setMethod(
  "+",
  signature(e1 = "Period", e2 = "BDays"),
  function(e1, e2) stop_incompatible_op("+", e1, e2)
)

# ------------------------------------------------------------------------------
# `-`

#' @export
setMethod(
  "-",
  signature(e1 = "BDays", e2 = "ANY"),
  function(e1, e2) {
    if (missing(e2)) {
      vec_arith("-", e1, MISSING())
    } else {
      vec_arith("-", e1, e2)
    }
  }
)

#' @export
setMethod(
  "-",
  signature(e1 = "ANY", e2 = "BDays"),
  function(e1, e2) vec_arith("-", e1, e2)
)

# Resolve ambiguity from:
# sch1 <- schedule()
# sch2 <- schedule()
# new_bdays(1L, sch1) - new_bdays(1L, sch2)

#' @export
setMethod(
  "-",
  signature(e1 = "BDays", e2 = "BDays"),
  function(e1, e2) vec_arith("-", e1, e2)
)

# Resolve ambiguity from the following,
# since Period objects have an ANY method:
# new_bdays(1L) - days(1L)
# days(1L) - new_bdays(1L)

#' @export
setMethod(
  "-",
  signature(e1 = "BDays", e2 = "Period"),
  function(e1, e2) stop_incompatible_op("-", e1, e2)
)

#' @export
setMethod(
  "-",
  signature(e1 = "Period", e2 = "BDays"),
  function(e1, e2) stop_incompatible_op("-", e1, e2)
)

# ------------------------------------------------------------------------------
# `*`

#' @export
setMethod(
  "*",
  signature(e1 = "BDays", e2 = "ANY"),
  function(e1, e2) {
    if (missing(e2)) {
      vec_arith("*", e1, MISSING())
    } else {
      vec_arith("*", e1, e2)
    }
  }
)

#' @export
setMethod(
  "*",
  signature(e1 = "ANY", e2 = "BDays"),
  function(e1, e2) vec_arith("*", e1, e2)
)

# Resolve ambiguity from:
# sch1 <- schedule()
# sch2 <- schedule()
# new_bdays(1L, sch1) * new_bdays(1L, sch2)

#' @export
setMethod(
  "*",
  signature(e1 = "BDays", e2 = "BDays"),
  function(e1, e2) vec_arith("*", e1, e2)
)

# Resolve ambiguity from the following,
# since Period objects have an ANY method:
# new_bdays(1L) * days(1L)
# days(1L) * new_bdays(1L)

#' @export
setMethod(
  "*",
  signature(e1 = "BDays", e2 = "Period"),
  function(e1, e2) stop_incompatible_op("*", e1, e2)
)

#' @export
setMethod(
  "*",
  signature(e1 = "Period", e2 = "BDays"),
  function(e1, e2) stop_incompatible_op("*", e1, e2)
)

# ------------------------------------------------------------------------------
# `/`

#' @export
setMethod(
  "/",
  signature(e1 = "BDays", e2 = "ANY"),
  function(e1, e2) stop_incompatible_op("/", e1, e2)
)

#' @export
setMethod(
  "/",
  signature(e1 = "ANY", e2 = "BDays"),
  function(e1, e2) stop_incompatible_op("/", e1, e2)
)

# Resolve ambiguity from:
# sch1 <- schedule()
# sch2 <- schedule()
# new_bdays(1L, sch1) / new_bdays(1L, sch2)

#' @export
setMethod(
  "/",
  signature(e1 = "BDays", e2 = "BDays"),
  function(e1, e2) stop_incompatible_op("/", e1, e2)
)

# Resolve ambiguity from the following,
# since Period objects have an ANY method:
# new_bdays(1L) / days(1L)
# days(1L) / new_bdays(1L)

#' @export
setMethod(
  "/",
  signature(e1 = "BDays", e2 = "Period"),
  function(e1, e2) stop_incompatible_op("/", e1, e2)
)

#' @export
setMethod(
  "/",
  signature(e1 = "Period", e2 = "BDays"),
  function(e1, e2) stop_incompatible_op("/", e1, e2)
)

# ------------------------------------------------------------------------------

#' vctrs compatibility functions
#'
#' These functions are the extensions that allow rray objects to
#' work with vctrs.
#'
#' @param x,y Objects.
#' @param op An arithmetic operator as a string.
#' @param ... Used to pass along error message information.
#'
#' @return
#'
#' See the corresponding vctrs function for the exact return value.
#'
#' @name vctrs-compat
#'
NULL

#' @rdname vctrs-compat
#' @export vec_arith.BDays
#' @method vec_arith BDays
#' @export
vec_arith.BDays <- function(op, x, y, ...) {
  UseMethod("vec_arith.BDays", y)
}

#' @method vec_arith.BDays default
#' @export
vec_arith.BDays.default <- function(op, x, y, ...) {
  stop_incompatible_op(op, x, y)
}

#' @method vec_arith.BDays MISSING
#' @export
vec_arith.BDays.MISSING <- function(op, x, y, ...) {
  switch(
    op,
    `+` = x,
    `-` = new_bdays(-get_bdays_days(x), get_bdays_schedule(x)),
    stop_incompatible_op(op, x, y)
  )
}

#' @method vec_arith.BDays BDays
#' @export
vec_arith.BDays.BDays <- function(op, x, y, ...) {
  switch(
    op,
    `+` = op_arith_bdays_bdays(op, x, y),
    `-` = op_arith_bdays_bdays(op, x, y),
    `*` = op_arith_bdays_bdays(op, x, y),
    stop_incompatible_op(op, x, y)
  )
}

op_arith_bdays_bdays <- function(op, x, y) {
  check_identical_schedules(x, y)

  sch <- get_bdays_schedule(x)

  x <- get_bdays_days(x)
  y <- get_bdays_days(y)

  out <- vec_arith(op, x, y)

  new_bdays(out, schedule = sch)
}

#' @method vec_arith.BDays numeric
#' @export
vec_arith.BDays.numeric <- function(op, x, y, ...) {
  switch(
    op,
    `+` = op_arith_bdays_numeric(op, x, y),
    `-` = op_arith_bdays_numeric(op, x, y),
    `*` = op_arith_bdays_numeric(op, x, y),
    stop_incompatible_op(op, x, y)
  )
}

op_arith_bdays_numeric <- function(op, x, y) {
  sch <- get_bdays_schedule(x)
  x <- get_bdays_days(x)

  if (!is_one_dim(y)) {
    abort("`y` must be 1 dimensional")
  }

  y <- vec_cast(y, integer(), x_arg = "y")

  out <- vec_arith(op, x, y)

  new_bdays(out, schedule = sch)
}

#' @method vec_arith.numeric BDays
#' @export
vec_arith.numeric.BDays <- function(op, x, y, ...) {
  switch(
    op,
    `+` = op_arith_numeric_bdays(op, x, y),
    `-` = op_arith_numeric_bdays(op, x, y),
    `*` = op_arith_numeric_bdays(op, x, y),
    stop_incompatible_op(op, x, y)
  )
}

op_arith_numeric_bdays <- function(op, x, y) {
  sch <- get_bdays_schedule(y)
  y <- get_bdays_days(y)

  if (!is_one_dim(x)) {
    abort("`x` must be 1 dimensional")
  }

  x <- vec_cast(x, integer(), x_arg = "x")

  out <- vec_arith(op, x, y)

  new_bdays(out, schedule = sch)
}

# Note that `bdays(1) - Sys.Date()` is not defined!

#' @method vec_arith.BDays Date
#' @export
vec_arith.BDays.Date <- function(op, x, y, ...) {
  switch(
    op,
    `+` = op_plus_bdays_Date(x, y),
    stop_incompatible_op(op, x, y)
  )
}

op_plus_bdays_Date <- function(x, y, plus) {
  days <- get_bdays_days(x)
  sch <- get_bdays_schedule(x)

  # Let `alma_step()` handle the recycling
  alma_step(y, days, sch)
}

#' @method vec_arith.Date BDays
#' @export
vec_arith.Date.BDays <- function(op, x, y, ...) {
  switch(
    op,
    `+` = op_arith_Date_bdays(x, y, minus = FALSE),
    `-` = op_arith_Date_bdays(x, y, minus = TRUE),
    stop_incompatible_op(op, x, y)
  )
}

op_arith_Date_bdays <- function(x, y, minus) {
  days <- get_bdays_days(y)
  sch <- get_bdays_schedule(y)

  if (minus) {
    days <- -days
  }

  # Let `alma_step()` handle the recycling
  alma_step(x, days, sch)
}

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
