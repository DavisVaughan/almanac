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
#'   recur_on_day_of_week("Thurs", nth = 4) %>%
#'   recur_on_month_of_year("Nov")
#'
#' on_christmas <- yearly() %>%
#'   recur_on_day_of_month(25) %>%
#'   recur_on_month_of_year("Dec")
#'
#' on_labor_day <- monthly() %>%
#'   recur_on_month_of_year("Sep") %>%
#'   recur_on_day_of_week("Mon", nth = 1)
#'
#' rb <- runion() %>%
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
  check_rbundle(x)
  check_rschedule(rschedule)

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
  check_rbundle(x)
  rdates <- vec_cast_date(rdates)

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
  check_rbundle(x)
  exdates <- vec_cast_date(exdates)

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
