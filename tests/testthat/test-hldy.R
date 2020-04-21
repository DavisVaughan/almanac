test_that("can construct a hldy object", {
  hldy <- new_hldy("hldy", function(x, y, w, z) x, function(x, y) x)
  expect_s3_class(hldy, "hldy")
  expect_named(hldy, c("name", "generator", "adjustment"))
})
