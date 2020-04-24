#' Add to an rbundle
#'
#' @description
#' - `add_rschedule()` adds an rschedule to an rbundle. This can be another
#'   rrule or another rbundle.
#'
#' - `add_rdates()` adds rdates to an rbundle. rdates are singular
#'   special cased dates that are forcibly included in the event set.
#'
#' - `add_exdates()` adds exdates to an rbundle. exdates are singular
#'   special cased dates that are forcibly excluded from the event set.
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
#' ```{r}
#' on_5th_of_the_month <- monthly() %>%
#'   recur_on_mday(5)
#'
#' on_tuesday <- weekly() %>%
#'   recur_on_wday("Tuesday")
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
#' @param rdates `[Date]`
#'
#'   Dates to forcibly include in the rbundle.
#'
#' @param exdates `[Date]`
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
#' rb2 <- add_exdates(rb, "2019-09-02")
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

  out <- new_rbundle(
    rschedules = rschedules,
    rdates = x$rdates,
    exdates = x$exdates
  )

  rbundle_restore(out, x)
}

#' @rdname rbundle-add
#' @export
add_rdates <- function(x, rdates) {
  validate_rbundle(x, "x")
  rdates <- vec_cast_date(rdates, "rdates")

  rdates <- vec_c(x$rdates, rdates)
  rdates <- vec_unique(rdates)
  rdates <- vec_sort(rdates)

  out <- new_rbundle(
    rschedules = x$rschedules,
    rdates = rdates,
    exdates = x$exdates
  )

  rbundle_restore(out, x)
}

#' @rdname rbundle-add
#' @export
add_exdates <- function(x, exdates) {
  validate_rbundle(x, "x")
  exdates <- vec_cast_date(exdates, "exdates")

  exdates <- vec_c(x$exdates, exdates)
  exdates <- vec_unique(exdates)
  exdates <- vec_sort(exdates)

  out <- new_rbundle(
    rschedules = x$rschedules,
    rdates = x$rdates,
    exdates = exdates
  )

  rbundle_restore(out, x)
}
