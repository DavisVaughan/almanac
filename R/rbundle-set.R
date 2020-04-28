#' Create a new set-based recurrence bundle
#'
#' @description
#' Often, a single rrule will be sufficient. However, more complex
#' recurrence objects can be constructed by combining multiple rschedules into
#' a _recurrence bundle_.
#'
#' There are three types of recurrence bundles provided in almanac, each of
#' which construct their event sets by performing a set operation on the
#' underlying event sets of the rschedules in the bundle.
#'
#' - `runion()` takes the union.
#'
#' - `rintersect()` takes the intersection.
#'
#' - `rsetdiff()` takes the set difference.
#'
#' Once you have created a recurrence bundle, you can:
#'
#' - Add recurrence rules or other recurrence bundles with [add_rschedule()].
#'
#' - Forcibly include dates in its event set with [add_rdates()].
#'
#' - Forcibly exclude dates from its event set with [add_exdates()].
#'
#' @details
#' For `rsetdiff()`, the event set is created "from left to right" and depends
#' on the order that the rschedules were added to the bundle.
#'
#' @return
#' An empty rbundle.
#'
#' @name rbundle-set
#' @seealso [add_rschedule()]
#' @examples
#' runion()
#'
#' on_weekends <- weekly() %>%
#'   recur_on_weekends()
#'
#' runion() %>%
#'   add_rschedule(on_weekends)
NULL


#' Constructor for a set-based recurrence bundle
#'
#' @description
#' These constructors are developer focused tools that are not required for
#' normal usage of almanac. They construct new rbundle subclasses directly from
#' a list of existing rschedules.
#'
#' - `new_runion()` creates an runion.
#'
#' - `new_rintersect()` creates an rintersect.
#'
#' - `new_rsetdiff()` creates a rsetdiff.
#'
#' @inheritParams new_rbundle
#'
#' @return
#' A new rbundle subclass.
#'
#' @name new-rbundle-set
#' @examples
#' new_runion()
#'
#' x <- daily()
#' y <- weekly()
#'
#' rschedules <- list(x, y)
#'
#' new_runion(rschedules)
NULL
