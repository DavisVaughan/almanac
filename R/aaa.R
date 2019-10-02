global_context <- NULL
global_empty_date <- NULL

.onLoad <- function(libname, pkgname) {
  rrule_js_path <- system.file("js/rrule.js", package = pkgname)

  global_context <<- V8::v8()
  global_context$source(rrule_js_path)

  global_empty_date <<- vctrs::new_date()

  invisible()
}
