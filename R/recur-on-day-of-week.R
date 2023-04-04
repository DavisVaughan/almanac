#' Recur on a day of the week
#'
#' @description
#'
#' - `recur_on_day_of_week()` recurs on a specific day of the week.
#'
#' - `recur_on_weekends()` and `recur_on_weekdays()` are helpers for
#'   recurring on weekends and weekdays.
#'
#' @details
#' Multiple week day values are allowed, and `nth` will be applied to
#' all of them. If you want to apply different `nth` values to different
#' days of the week, call `recur_on_day_of_week()` twice with different `day`
#' values.
#'
#' It is particularly important to pay attention to the `since` date when using
#' weekly rules. The day of the week to use comes from the `since` date, which,
#' by default, is a Monday (`1900-01-01`).
#'
#' @inheritParams rlang::args_dots_empty
#'
#' @param x `[rrule]`
#'
#'    A recurrence rule.
#'
#' @param day `[integer / character]`
#'
#'    Days of the week to recur on. Integer values must be from `1` to `7`, with
#'    `1 = Monday` and `7 = Sunday`. This is also allowed to be a full weekday
#'    string like `"Tuesday"`, or an abbreviation like `"Tues"`.
#'
#' @param nth `[integer / NULL]`
#'
#'    Limit to the n-th occurrence of the `day` in the base frequency. For
#'    example, in a monthly frequency, using `nth = -1` would limit to the
#'    last `day` in the month. The default of `NULL` chooses all occurrences.
#'
#' @return
#' An updated rrule.
#'
#' @examples
#' # Using default `since` (1900-01-01, a Monday)
#' on_weekly_mondays <- weekly()
#'
#' start <- "1999-01-01" # <- a Friday
#' end <- "1999-03-01"
#'
#' # This finds the first Thursday, and then continues from there
#' alma_search(start, end, on_weekly_mondays)
#'
#' # We start counting from a Friday here
#' on_weekly_fridays <- weekly(since = start)
#' alma_search(start, end, on_weekly_fridays)
#'
#' # Alternatively, we could use `recur_on_day_of_week()` and force a recurrence
#' # rule on Friday
#' on_forced_friday <- on_weekly_mondays %>% recur_on_day_of_week("Friday")
#' alma_search(start, end, on_forced_friday)
#'
#' # At monthly frequencies, you can use n-th values to look for particular
#' # week day events
#' on_first_friday_in_month <- monthly() %>% recur_on_day_of_week("Fri", nth = 1)
#' alma_search(start, end, on_first_friday_in_month)
#'
#' # Negative values let you look from the back
#' on_last_friday_in_month <- monthly() %>% recur_on_day_of_week("Fri", nth = -1)
#' alma_search(start, end, on_last_friday_in_month)
#'
#' # At yearly frequencies, this looks for the first sunday of the year
#' on_first_sunday_in_year <- yearly() %>% recur_on_day_of_week("Sunday", nth = 1)
#' alma_search(start, end, on_first_sunday_in_year)
#'
#' # Last week day of the month
#' last_weekday_of_month <- monthly() %>%
#'   # Last occurrence of each weekday in the month
#'   recur_on_day_of_week(c("Mon", "Tue", "Wed", "Thu", "Fri"), nth = -1) %>%
#'   # Now choose the last one of those in each month
#'   recur_on_position(-1)
#'
#' alma_search(start, end, last_weekday_of_month)
#'
#' @export
recur_on_day_of_week <- function(x, day, ..., nth = NULL) {
  check_dots_empty0(...)
  check_rrule(x)

  old <- get_rule(x, "day_of_week")
  if (is_null(old)) {
    old <- new_list(n = 7L)
  }

  day <- day_of_week_normalize(day)
  day <- vec_cast(day, to = integer())

  if (any(day < 1L | day > 7L)) {
    abort("`day` must be in [1, 7].")
  }

  # Early exit for all weekdays
  if (is_null(nth)) {
    for (elt in day) {
      old[[elt]] <- "all"
    }
    x <- tweak_rrule(x, day_of_week = old)
    return(x)
  }

  nth <- vec_cast(nth, to = integer())

  is_yearly <- x$rules$frequency == "yearly"
  abs_nth <- abs(nth)

  if (is_yearly) {
    if (any(abs_nth > 53 | abs_nth < 1)) {
      abort("`nth` can only take values in [-53, -1] and [1, 53] when the frequency is yearly.")
    }
  } else {
    if (any(abs_nth > 5 | abs_nth < 1)) {
      abort("`nth` can only take values in [-5, -1] and [1, 5].")
    }
  }

  for (elt in day) {
    old_nth <- old[[elt]]

    # The union of "all" and any other nth is "all"
    if (identical(old_nth, "all")) {
      return(x)
    }

    if (is_null(old_nth)) {
      new_nth <- nth
    } else {
      new_nth <- vec_set_union(old_nth, nth)
    }

    new_nth <- vec_unique(new_nth)
    new_nth <- vec_sort(new_nth)

    old[[elt]] <- new_nth
  }

  tweak_rrule(x, day_of_week = old)
}

#' @rdname recur_on_day_of_week
#' @export
recur_on_weekdays <- function(x) {
  recur_on_day_of_week(x, day = 1:5)
}

#' @rdname recur_on_day_of_week
#' @export
recur_on_weekends <- function(x) {
  recur_on_day_of_week(x, day = 6:7)
}
