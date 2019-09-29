# `nth` is applied to all `wday`'s

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
