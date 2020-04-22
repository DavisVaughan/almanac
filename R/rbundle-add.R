#' Add to an rbundle
#'
#' @description
#' - `add_rschedule()` adds an rschedule to an rbundle. This can be another
#'   rrule or another rbundle.
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
#' - An event generated from an rschedule will always be included if it is not
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
#'   add_rschedule(on_5th_of_the_month) %>%
#'   add_rschedule(on_tuesday)
#' ```
#'
#' @inheritParams adj_following
#'
#' @param x `[rbundle]`
#'
#'   An rbundle to add to.
#'
#' @param rdate `[Date]`
#'
#'   Dates to forcibly include in the rbundle.
#'
#' @param exdate `[Date]`
#'
#'   Dates to forcibly exclude from the rbundle.
#'
#' @return
#' An updated rbundle.
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
#'   add_rschedule(on_thanksgiving) %>%
#'   add_rschedule(on_christmas) %>%
#'   add_rschedule(on_labor_day)
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
add_rschedule <- function(x, rschedule) {
  validate_rbundle(x, "x")
  validate_rschedule(rschedule, x_arg = "rschedule")

  rschedules <- c(x$rschedules, list(rschedule))

  new_rbundle(
    rschedules = rschedules,
    rdates = x$rdates,
    exdates = x$exdates
  )
}

#' @rdname rbundle-add
#' @export
add_rdate <- function(x, rdate) {
  validate_rbundle(x, "x")
  rdate <- vec_cast_date(rdate, "rdate")

  rdates <- vec_c(x$rdates, rdate)
  rdates <- unique(rdates)

  new_rbundle(
    rschedules = x$rschedules,
    rdates = rdates,
    exdates = x$exdates
  )
}

#' @rdname rbundle-add
#' @export
add_exdate <- function(x, exdate) {
  validate_rbundle(x, "x")
  exdate <- vec_cast_date(exdate, "exdate")

  exdates <- vec_c(x$exdates, exdate)
  exdates <- vec_unique(exdates)

  new_rbundle(
    rschedules = x$rschedules,
    rdates = x$rdates,
    exdates = exdates
  )
}
