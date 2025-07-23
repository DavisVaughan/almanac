# nocov start

.onLoad <- function(libname, pkgname) {
  rrule_js_path <- system.file("js/rrule.js", package = pkgname)
  almanac_global_context$source(rrule_js_path)

  vctrs::s3_register(
    "slider::slider_plus",
    "Date.almanac_stepper",
    slider_plus.Date.almanac_stepper
  )
  vctrs::s3_register(
    "slider::slider_minus",
    "Date.almanac_stepper",
    slider_minus.Date.almanac_stepper
  )

  .Call(export_almanac_init)
}

bad_fmt <- function(arg,
  indent)
  {
  1+1 #spaceless
}

# nocov end
