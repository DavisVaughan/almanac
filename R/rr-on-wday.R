#' @export
rr_on_wday <- function(x, wday, nth = NULL) {
  validate_rrule(x)

  old <- get_rule(x, "wday")

  if (is.null(old)) {
    old <- new_list(n = 7L)
  }

  wday <- wday_normalize(wday)
  wday <- vec_cast(wday, integer(), x_arg = "wday")
  vec_assert(wday, size = 1L)

  if (wday < 1L || wday > 7L) {
    abort("`wday` must be in [1, 7].")
  }

  # Early exit for all weekdays
  if (is.null(nth)) {
    old[[wday]] <- "all"
    x <- tweak_rrule(x, wday = old)
    return(x)
  }

  new_nth <- vec_cast(nth, integer(), x_arg = "nth")

  abs_nth <- abs(new_nth)
  if (any(abs_nth > 5 | abs_nth < 1)) {
    abort("`nth` can only take values in [-5, -1] and [1, 5].")
  }

  old_nth <- old[[wday]]

  # The union of "all" and any other nth is "all"
  if (identical(old_nth, "all")) {
    return(x)
  }

  new_nth <- union(old_nth, new_nth)
  new_nth <- unique(new_nth)
  new_nth <- sort(new_nth)

  old[[wday]] <- new_nth

  tweak_rrule(x, wday = old)
}
