#' Add to an rbundle
#'
#' @description
#' - `add_cacher()` adds a rrule to an rbundle.
#'
#' - `add_rdate()` adds a rdate to an rbundle. rdates are singular
#'   special cased dates that are forcibly included in the recurrence set.
#'
#' - `add_exdate()` adds an exdate to a rbundle. exdates are singular
#'   special cased dates that are forcibly excluded from the recurrence set.
#'
#' @details
#' In terms of priority:
#'
#' - An exdate will never be included.
#'
#' - A rdate will always be included if it is not also an exdate.
#'
#' - An event generated from a rrule will always be included if it is not
#'   also an exdate.
#'
#' Combining two rrules into the same rbundle is a way of joining them using
#' an "or" condition. For example, joining the following recurrence rules
#' would translate to "the 5th day of the month or any Tuesday".
#'
#' ```
#' on_5th_of_the_month <- monthly() %>%
#'   on_mday(5)
#'
#' on_tuesday <- weekly() %>%
#'   on_wday("Tuesday")
#'
#' rbundle() %>%
#'   add_cacher(on_5th_of_the_month) %>%
#'   add_cacher(on_tuesday)
#' ```
#'
#' @param x `[rbundle]`
#'
#'   An rbundle to add to.
#'
#' @param cacher `[cacher]`
#'
#'   A cacher to add to the rbundle.
#'
#' @param rdate `[Date]`
#'
#'   Dates to forcibly include in the rbundle.
#'
#' @param exdate `[Date]`
#'
#'   Dates to forcibly exclude from the rbundle.
#'
#' @examples
#' on_thanksgiving <- yearly() %>%
#'   recur_on_wday("Thurs", 4) %>%
#'   recur_on_ymonth("Nov")
#'
#' on_christmas <- yearly() %>%
#'   recur_on_mday(25) %>%
#'   recur_on_ymonth("Dec")
#'
#' on_labor_day <- monthly() %>%
#'   recur_on_ymonth("Sep") %>%
#'   recur_on_wday("Mon", 1)
#'
#' rb <- rbundle() %>%
#'   add_cacher(on_thanksgiving) %>%
#'   add_cacher(on_christmas) %>%
#'   add_cacher(on_labor_day)
#'
#' # Thanksgiving, Christmas, or Labor Day
#' alma_search("2019-01-01", "2021-01-01", rb)
#'
#' # Except Labor Day in 2019
#' rb2 <- add_exdate(rb, "2019-09-02")
#'
#' alma_search("2019-01-01", "2021-01-01", rb2)
#'
#' @name rbundle-add
NULL

#' @rdname rbundle-add
#' @export
add_cacher <- function(x, cacher) {
  validate_rbundle(x)
  validate_cacher(cacher, x_arg = "cacher")

  cachers <- c(x$cachers, list(cacher))

  new_rbundle(
    cachers = cachers,
    rdates = x$rdates,
    exdates = x$exdates
  )
}

#' @rdname rbundle-add
#' @export
add_rdate <- function(x, rdate) {
  validate_rbundle(x)
  rdate <- vec_cast_date(rdate, "rdate")

  validate_date_bounds(rdate, x_arg = "rdate")

  rdates <- vec_c(x$rdates, rdate)
  rdates <- unique(rdates)

  new_rbundle(
    cachers = x$cachers,
    rdates = rdates,
    exdates = x$exdates
  )
}

#' @rdname rbundle-add
#' @export
add_exdate <- function(x, exdate) {
  validate_rbundle(x)
  exdate <- vec_cast_date(exdate, "exdate")

  validate_date_bounds(exdate, x_arg = "exdate")

  exdates <- vec_c(x$exdates, exdate)
  exdates <- vec_unique(exdates)

  new_rbundle(
    cachers = x$cachers,
    rdates = x$rdates,
    exdates = exdates
  )
}
