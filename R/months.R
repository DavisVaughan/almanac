test_month_from_days <- function(x) {
  x <- vec_cast_date(x)
  .Call(export_test_month_from_days, x)
}
