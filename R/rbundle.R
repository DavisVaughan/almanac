#' Constructor for an rbundle
#'
#' @description
#' `new_rbundle()` is a developer focused tool that is not required for normal
#' usage of almanac. It constructs a new rbundle directly from a list of
#' existing rschedules.
#'
#' @param rschedules `[list]`
#'
#'   A list of rschedules.
#'
#' @param rdates `[Date]`
#'
#'   A vector of dates to forcibly include in the event set.
#'
#' @param exdates `[Date]`
#'
#'   A vector of dates to forcibly exclude from the event set.
#'
#' @param ... `[named dots]`
#'
#'   Additional named elements added to the rbundle object.
#'
#' @param class `[character]`
#'
#'   An optional subclass.
#'
#' @return
#' `new_rbundle()` returns a new rbundle.
#'
#' @export
#' @examples
#' new_rbundle()
#'
#' x <- daily()
#' y <- weekly()
#'
#' rschedules <- list(x, y)
#'
#' new_rbundle(rschedules)
new_rbundle <- function(rschedules = list(),
                        rdates = new_date(),
                        exdates = new_date(),
                        ...,
                        class = character()) {
  vec_check_list(rschedules)

  check_date(rdates)
  check_no_missing(rdates)
  check_finite(rdates)
  check_date_within_bounds(rdates)

  check_date(exdates)
  check_no_missing(exdates)
  check_finite(exdates)
  check_date_within_bounds(exdates)

  new_rschedule(
    rschedules = rschedules,
    rdates = rdates,
    exdates = exdates,
    ...,
    class = c(class, "rbundle")
  )
}
