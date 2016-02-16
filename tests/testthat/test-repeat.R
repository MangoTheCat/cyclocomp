
context("Repeat loops")

test_that("simple repeat loops", {

  f <- function() repeat { }
  expect_equal(cyclocomp(f), 2)
})
