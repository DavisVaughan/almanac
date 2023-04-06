test_that("events are offset from the original schedule", {
  events <- as.Date(c("2019-02-05", "2019-01-10", "2019-02-05", "2019-01-01"))

  x <- rcustom(events)
  y <- roffset(x, 2)

  expect_identical(
    rschedule_events(y),
    rschedule_events(x) + 2
  )
})

test_that("print method is nice", {
  x <- yearly()
  x <- roffset(x, by = 1)

  expect_snapshot({
    x
  })

  # Nesting
  z <- runion(x, x)

  expect_snapshot({
    z
  })
})

test_that("`by` must be a single integer", {
  expect_snapshot(error = TRUE, {
    roffset(yearly(), by = "a")
  })
  expect_snapshot(error = TRUE, {
    roffset(yearly(), by = 1:2)
  })
  expect_snapshot(error = TRUE, {
    roffset(yearly(), by = NA_integer_)
  })
  expect_snapshot(error = TRUE, {
    roffset(yearly(), by = Inf)
  })
})

test_that("`rschedule` is validated", {
  expect_snapshot(error = TRUE, {
    roffset(1, by = 2)
  })
})
