rrule_js_path <- NULL

.onLoad <- function(libname, pkgname) {
  rrule_js_path <<- system.file("js/rrule.js", package = pkgname)
}
