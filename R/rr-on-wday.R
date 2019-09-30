#' Recur on a day of the week
#'
#' `rr_on_wday()` recurs on a specific day of the week.
#'
#' @details
#'
#' Multiple week day values are allowed, and `nth` will be applied to
#' all of them. If you want to apply different `nth` values to different
#' days of the week, call `rr_on_wday()` twice with different `wday` values.
#'
#' It is particularly important to pay attention to the `since` date when using
#' weekly rules. The day of the week to use comes from the `since` date, which,
#' by default, is a Thursday (`1970-01-01`).
#'
#' @param x `[rrule]`
#'
#'    A recurrence rule.
#'
#' @param wday `[integer / character]`
#'
#'    Days of the week to recur on. Integer values must be from `1` to `7`, with
#'    `1 = Monday` and `7 = Sunday`. This is also allowed to be a full weekday
#'    string like `"Tuesday"`, or an abbreviation like `"Tues"`.
#'
#' @param nth `[integer / NULL]`
#'
#'    Limit to the n-th occurence of the `wday` in the base frequency. For
#'    example, in a monthly frequency, using `nth = -1` would limit to the
#'    last `wday` in the month. The default of `NULL` chooses all occurences.
#'
#' @examples
#' library(magrittr)
#'
#' # Using default `since` (1970-01-01, a Thursday)
#' on_weekly_thursdays <- weekly()
#'
#' start <- "1999-01-01" # <- a Friday
#' end <- "1999-03-01"
#'
#' # This finds the first Thursday, and then continues from there
#' sch_seq(start, end, on_weekly_thursdays)
#'
#' # We start counting from a Friday here, so our `start` date counts
#' on_weekly_fridays <- weekly(since = start)
#' sch_seq(start, end, on_weekly_fridays)
#'
#' # Alternatively, we could use `rr_on_wday()` and force a recurrence on Friday
#' on_wday_friday <- on_weekly_thursdays %>% rr_on_wday("Friday")
#' sch_seq(start, end, on_wday_friday)
#'
#' # At monthly frequencies, you can use n-th values to look for particular
#' # week day occurrences
#' on_first_friday_in_month <- monthly() %>% rr_on_wday("Fri", 1)
#' sch_seq(start, end, on_first_friday_in_month)
#'
#' # Negative values let you look from the back
#' on_last_friday_in_month <- monthly() %>% rr_on_wday("Fri", -1)
#' sch_seq(start, end, on_last_friday_in_month)
#'
#' # At yearly frequencies, this looks for the first sunday of the year
#' on_first_sunday_in_year <- yearly() %>% rr_on_wday("Sunday", 1)
#' sch_seq(start, end, on_first_sunday_in_year)
#'
#' # Last week day of the month
#' last_weekday_of_month <- monthly() %>%
#'   # Last occurence of each weekday in the month
#'   rr_on_wday(c("Mon", "Tue", "Wed", "Thu", "Fri"), -1) %>%
#'   # Now choose the last one of those in each month
#'   rr_position(-1)
#'
#' sch_seq(start, end, last_weekday_of_month)
#'
#' @export
rr_on_wday <- function(x, wday, nth = NULL) {
  validate_rrule(x)

  old <- get_rule(x, "wday")

  if (is.null(old)) {
    old <- new_list(n = 7L)
  }

  wday <- wday_normalize(wday)
  wday <- vec_cast(wday, integer(), x_arg = "wday")

  if (any(wday < 1L | wday > 7L)) {
    abort("`wday` must be in [1, 7].")
  }

  # Early exit for all weekdays
  if (is.null(nth)) {
    for (day in wday) {
      old[[day]] <- "all"
    }
    x <- tweak_rrule(x, wday = old)
    return(x)
  }

  new_nth <- vec_cast(nth, integer(), x_arg = "nth")

  abs_nth <- abs(new_nth)
  if (any(abs_nth > 5 | abs_nth < 1)) {
    abort("`nth` can only take values in [-5, -1] and [1, 5].")
  }

  for (day in wday) {
    old_nth <- old[[day]]

    # The union of "all" and any other nth is "all"
    if (identical(old_nth, "all")) {
      return(x)
    }

    new_nth <- union(old_nth, new_nth)
    new_nth <- unique(new_nth)

    old[[day]] <- new_nth
  }

  tweak_rrule(x, wday = old)
}
