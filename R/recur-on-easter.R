#' Recur on easter
#'
#' `recur_on_easter()` is a special helper to recur on Easter. Easter is
#' particularly difficult to construct a recurrence rule for. Using `offset`,
#' this can also be used to generate a recurrence rule on Easter Monday or
#' Good Friday.
#'
#' @param x `[rrule]`
#'
#'    A recurrence rule.
#'
#' @param offset `[integer(1)]`
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
#' on_easter_monday <- yearly() %>% recur_on_easter(-1)
#'
#' alma_search("1999-01-01", "2001-01-01", on_easter)
#'
#' rb <- runion(on_easter, on_easter_monday)
#'
#' alma_search("1999-01-01", "2001-01-01", rb)
#'
#'
#' # Note that `offset` must land within the same year, otherwise the date
#' # is ignored
#' on_easter_back_93_days <- yearly() %>% recur_on_easter(-93)
#' on_easter_back_94_days <- yearly() %>% recur_on_easter(-94)
#'
#' alma_search("1999-01-01", "2001-01-01", on_easter_back_93_days)
#' alma_search("1999-01-01", "2001-01-01", on_easter_back_94_days)
recur_on_easter <- function(x, offset = 0L) {
  check_rrule(x)
  check_rule_not_set(x, "easter")

  check_number_whole(offset, min = -366, max = 366)
  offset <- vec_cast(offset, to = integer())

  tweak_rrule(x, easter = offset)
}
