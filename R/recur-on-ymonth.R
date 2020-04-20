#' Recur on a month of the year
#'
#' `recur_on_ymonth()` recurs on a specific month of the year.
#'
#' @param x `[rrule]`
#'
#'    A recurrence rule.
#'
#' @param ymonth `[integer / character]`
#'
#'    Months of the year to mark as events. Integer values must be between
#'    `[1, 12]`. This can also be a full month string like `"November"`, or an
#'    abbreviation like `"Nov"`.
#'
#' @examples
#' # There is a big difference between adding this rule to a `yearly()`
#' # or `monthly()` frequency, and a `daily()` frequency.
#'
#' # Limit from every day to every day in February
#' on_feb_daily <- daily() %>% recur_on_ymonth("Feb")
#'
#' # Limit from 1 day per month to 1 day in February
#' on_feb_monthly <- monthly() %>% recur_on_ymonth("Feb")
#'
#' start <- "1999-01-01"
#' end <- "2001-01-01"
#'
#' alma_search(start, end, on_feb_daily)
#'
#' alma_search(start, end, on_feb_monthly)
#'
#' @export
recur_on_ymonth <- function(x, ymonth) {
  validate_rrule(x)
  ymonth <- month_normalize(ymonth)

  old <- get_rule(x, "ymonth")
  new <- vec_cast(ymonth, integer(), x_arg = "ymonth")

  if (any(new > 12 | new < 1)) {
    abort("`ymonth` can only take values in [1, 12].")
  }

  new <- union(old, new)
  new <- unique(new)
  new <- sort(new)

  tweak_rrule(x, ymonth = new)
}

# ------------------------------------------------------------------------------

month_normalize <- function(x) {
  if (!is.character(x)) {
    return(x)
  }

  x <- tolower(x)

  where <- month_match(x)

  misses <- is.na(where)

  if (any(misses)) {
    abort("A character `x` must be a month name or abbreviation.")
  }

  out <- month_int()[where]

  out <- unique(out)

  out
}

month_match <- function(x) {
  vec_match(x, month_name())
}

month_name <- function() {
  c(
    tolower(month.name),
    tolower(month.abb),
    "sept" # special case
  )
}

month_int <- function() {
  c(
    1:12,
    1:12,
    9L
  )
}
