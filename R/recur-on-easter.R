#' Recur on easter
#'
#' `recur_on_easter()` is a special helper to recur on Easter. Easter is
#' particularly difficult to construct a recurrence rule for.
#'
#' @param x `[rrule]`
#'
#'    A recurrence rule.
#'
#' @param offset `r lifecycle::badge("deprecated")`
#'
#'    `[integer(1)]`
#'
#'    Deprecated in favor of using [roffset()] directly.
#'
#'    An offset in terms of a number of days on either side of Easter to recur
#'    on. This offset must still fall within the same year, otherwise the date
#'    will be silently ignored.
#'
#' @return
#' An updated rrule.
#'
#' @export
#' @examples
#' on_easter <- yearly() %>% recur_on_easter()
#'
#' # Rather than:
#' if (FALSE) {
#' on_easter_monday <- yearly() %>% recur_on_easter(1)
#' }
#'
#' # Please use:
#' on_easter_monday <- roffset(on_easter, 1)
#'
#' alma_search("1999-01-01", "2001-01-01", on_easter)
#'
#' both <- runion(on_easter, on_easter_monday)
#'
#' alma_search("1999-01-01", "2001-01-01", both)
recur_on_easter <- function(x, offset = NULL) {
  check_rrule(x)
  check_rule_not_set(x, "easter")

  if (is_null(offset)) {
    offset <- 0L
  } else {
    lifecycle::deprecate_warn(
      when = "1.0.0",
      what = "recur_on_easter(offset =)",
      with = "roffset()",
      always = TRUE
    )
  }

  check_number_whole(offset, min = -366, max = 366)
  offset <- vec_cast(offset, to = integer())

  tweak_rrule(x, easter = offset)
}
