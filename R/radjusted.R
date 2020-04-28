#' Create an adjusted rschedule
#'
#' @description
#' `radjusted()` creates a new adjusted rschedule on top of an existing one. The
#' new rschedule contains the same event dates as the existing rschedule,
#' except when they intersect with the dates in the event set of the
#' rschedule, `adjust_on`. In those cases, an `adjustment` is applied to the
#' problematic dates to shift them to valid event dates.
#'
#' This is most useful when creating corporate holiday rschedules. For example,
#' Christmas always falls on December 25th, but if it falls on a Saturday,
#' your company might observe Christmas on the previous Friday. If it falls
#' on a Sunday, you might observe it on the following Monday. In this case,
#' you could construct an rschedule for a recurring event of December 25th,
#' and a second rschedule for weekends. When Christmas falls on a weekend,
#' you would apply an adjustment of [adj_nearest()] to get the observance date.
#'
#' @inheritParams adj_following
#'
#' @param adjust_on `[rschedule]`
#'
#'   An rschedule that determines when the `adjustment` is to be applied.
#'
#' @param adjustment `[function]`
#'
#'   An adjustment function to apply to problematic dates. Typically one
#'   of the pre-existing adjustment functions, like [adj_nearest()].
#'
#'   A custom adjustment function must have two arguments `x` and `rschedule`.
#'   `x` is the complete vector of dates that possibly need adjustment.
#'   `rschedule` is the rschedule who's event set determines when an
#'   adjustment needs to be applied. The function should adjust `x` as required
#'   and return the adjusted Date vector.
#'
#' @return
#' An adjusted rschedule.
#'
#' @export
#' @examples
#' since <- "2000-01-01"
#' until <- "2010-01-01"
#'
#' on_christmas <- yearly(since = since, until = until) %>%
#'   recur_on_ymonth("Dec") %>%
#'   recur_on_mday(25)
#'
#' # All Christmas dates, with no adjustments
#' alma_events(on_christmas)
#'
#' on_weekends <- weekly(since = since, until = until) %>%
#'   recur_on_weekends()
#'
#' # Now all Christmas dates that fell on a weekend are
#' # adjusted either forwards or backwards, depending on which
#' # non-event date was closer
#' on_adj_christmas <- radjusted(on_christmas, on_weekends, adj_nearest)
#'
#' alma_events(on_adj_christmas)
radjusted <- function(rschedule, adjust_on, adjustment) {
  new_radjusted(rschedule, adjust_on, adjustment)
}

# ------------------------------------------------------------------------------

#' @export
print.radjusted <- function(x, ...) {
  print(format(x))
  invisible(x)
}

#' @export
format.radjusted <- function(x, ...) {
  rschedule <- format(x$rschedule)
  adjust_on <- format(x$adjust_on)

  out <- c("<radjusted>\n", "Adjust:", rschedule, "\nAdjust on:", adjust_on)
  out <- glue::glue_collapse(out, sep = "\n")

  out
}

# ------------------------------------------------------------------------------

#' @export
rschedule_events.radjusted <- function(x) {
  x$cache$get_events()
}

# ------------------------------------------------------------------------------

new_radjusted <- function(rschedule, adjust_on, adjustment) {
  validate_rschedule(rschedule, "rschedule")
  validate_rschedule(adjust_on, "adjust_on")
  validate_adjustment(adjustment, "adjustment")

  cache <- cache_radjusted$new(
    rschedule = rschedule,
    adjust_on = adjust_on,
    adjustment = adjustment
  )

  new_rschedule(
    rschedule = rschedule,
    adjust_on = adjust_on,
    adjustment = adjustment,
    cache = cache,
    class = "radjusted"
  )
}

# ------------------------------------------------------------------------------

validate_adjustment <- function(x, x_arg = "") {
  if (nzchar(x_arg)) {
    x_arg <- glue(" `{x_arg}`")
  }

  if (!is_function(x)) {
    glubort("Input{x_arg} must be a function.")
  }

  fmls <- fn_fmls(x)

  if (length(fmls) != 2L) {
    glubort("Input{x_arg} must have two arguments, `x` and `rschedule`.")
  }

  invisible(x)
}
