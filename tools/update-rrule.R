# Update script for `inst/js/rrule.js`, which corresponds to the minified rrule
# JS library found at https://github.com/jakubroztocil/rrule.

url <- "https://jakubroztocil.github.io/rrule/dist/es5/rrule.min.js"

tf <- tempfile(fileext = ".js")
download.file(url, destfile = tf)

fs::file_copy(
  path = tf,
  new_path = here::here("inst", "js", "rrule.js"),
  overwrite = TRUE
)

unlink(tf)
