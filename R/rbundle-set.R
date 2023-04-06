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
#' @details
#' For `rsetdiff()`, the event set is created "from left to right" and depends
#' on the order that the rschedules were added to the bundle.
#'
#' @param ... `[rschedules]`
#'
#'   rschedule objects to add to the bundle.
#'
#' @return
#' A runion, rintersect, or rsetdiff.
#'
#' @name rbundle-set
#' @examples
#' since <- "2019-04-01"
#' until <- "2019-05-31"
#'
#' on_weekends <- weekly(since = since, until = until) %>%
#'   recur_on_weekends()
#'
#' on_25th <- monthly(since = since, until = until) %>%
#'   recur_on_day_of_month(25)
#'
#' # On weekends OR the 25th of the month
#' ru <- runion(on_weekends, on_25th)
#' alma_events(ru)
#'
#' # On weekends AND the 25th of the month
#' ri <- rintersect(on_weekends, on_25th)
#' alma_events(ri)
#'
#' # On weekends AND NOT the 25th of the month
#' rsd1 <- rsetdiff(on_weekends, on_25th)
#' alma_events(rsd1)
#'
#' # On the 25th of the month AND NOT the weekend
#' rsd2 <- rsetdiff(on_25th, on_weekends)
#' alma_events(rsd2)
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
