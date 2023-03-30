# ------------------------------------------------------------------------------
# new_rschedule()

test_that("must supply at least one field", {
  expect_snapshot(error = TRUE, {
    new_rschedule(class = "foo")
  })
})

test_that("must supply named fields", {
  expect_snapshot(error = TRUE, {
    new_rschedule(1, "foo")
  })
})

test_that("class is required", {
  expect_error(new_rschedule(x = 1))
})

test_that("can create rschedule subclasses", {
  x <- new_rschedule(x = 1, class = "foobar")
  expect_identical(x, structure(list(x = 1), class = c("foobar", "rschedule")))
})

# ------------------------------------------------------------------------------
# rschedule_events()

test_that("default method errors", {
  expect_snapshot(error = TRUE, {
    rschedule_events(1)
  })
})

test_that("method is required for subclasses", {
  x <- new_rschedule(x = 1, class = "foobar")
  expect_snapshot(error = TRUE, {
    rschedule_events(x)
  })
})


