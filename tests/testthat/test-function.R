
context("Functions")

test_that("function definitions work", {

  f <- function() function() "foo"
  expect_equal(cyclocomp(f), 1)

  f <- function() function(ok) ok
  expect_equal(cyclocomp(f), 1)
})
