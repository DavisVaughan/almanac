test_that("can construct a hldy object", {
  hldy <- new_hldy("hldy", function(x, y) x, weekly(), adj_none)
  expect_s3_class(hldy, "hldy")
  expect_named(hldy, c("name", "generator", "adjust_on", "adjustment"))
})
