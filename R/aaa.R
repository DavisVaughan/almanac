global_context <- NULL
global_empty_date <- NULL
global_inf_date <- NULL
global_neg_inf_date <- NULL
global_na_date <- NULL

.onLoad <- function(libname, pkgname) {
  rrule_js_path <- system.file("js/rrule.js", package = pkgname)

  global_context <<- V8::v8()
  global_context$source(rrule_js_path)

  global_empty_date <<- vctrs::new_date()
  global_inf_date <<- structure(Inf, class = "Date")
  global_neg_inf_date <<- structure(-Inf, class = "Date")
  global_na_date <<- structure(NA_real_, class = "Date")

  invisible()
}
