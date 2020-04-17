# nocov start

.onLoad <- function(libname, pkgname) {
  rrule_js_path <- system.file("js/rrule.js", package = pkgname)
  almanac_global_context$source(rrule_js_path)

  .Call(export_almanac_init)
}

# nocov end
