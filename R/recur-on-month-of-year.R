#' Recur on a month of the year
#'
#' `recur_on_month_of_year()` recurs on a specific month of the year.
#'
#' @param x `[rrule]`
#'
#'    A recurrence rule.
#'
#' @param month `[integer / character]`
#'
#'    Months of the year to mark as events. Integer values must be between
#'    `[1, 12]`. This can also be a full month string like `"November"`, or an
#'    abbreviation like `"Nov"`.
#'
#' @return
#' An updated rrule.
#'
#' @export
#' @examples
#' # There is a big difference between adding this rule to a `yearly()`
#' # or `monthly()` frequency, and a `daily()` frequency.
#'
#' # Limit from every day to every day in February
#' on_feb_daily <- daily() %>% recur_on_month_of_year("Feb")
#'
#' # Limit from 1 day per month to 1 day in February
#' on_feb_monthly <- monthly() %>% recur_on_month_of_year("Feb")
#'
#' start <- "1999-01-01"
#' end <- "2001-01-01"
#'
#' alma_search(start, end, on_feb_daily)
#'
#' alma_search(start, end, on_feb_monthly)
recur_on_month_of_year <- function(x, month) {
  check_rrule(x)

  month <- month_normalize(month)
  month <- vec_cast(month, to = integer())

  if (any(month > 12 | month < 1)) {
    abort("`month` can only take values in [1, 12].")
  }

  old <- get_rule(x, "month_of_year")
  if (!is_null(old)) {
    month <- vec_set_union(old, month)
  }

  month <- vec_unique(month)
  month <- vec_sort(month)

  tweak_rrule(x, month_of_year = month)
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
