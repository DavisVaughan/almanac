test_that("can create an empty rbundle()", {
  x <- rbundle()
  expect_s3_class(x, "rbundle")
  expect_identical(x$rrules, list())
  expect_identical(x$rdates, new_date())
  expect_identical(x$exdates, new_date())
})

test_that("rbundle() generates informative output", {
  verify_output(test_path("output", "test-rbundle.txt"), {
    "# Empty rbundle"
    rbundle()
  })
})
