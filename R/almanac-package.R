#' @keywords internal
"_PACKAGE"

# We import R6::R6Class() and V8::v8() below to avoid an R CMD Check false alarm
# that states "Namespaces in Imports field not imported from". I guess this
# comes up because we don't call these from inside functions.

# The following block is used by usethis to automatically manage
# roxygen namespace tags. Modify with care!
## usethis namespace: start
#' @import rlang
#' @import vctrs
#' @importFrom glue glue
#' @importFrom R6 R6Class
#' @importFrom V8 v8
#' @useDynLib almanac, .registration = TRUE
## usethis namespace: end
NULL
