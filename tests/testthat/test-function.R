
context("Functions")

test_that("function definitions work", {

  f <- function() function() "foo"
  expect_equal(cyclocomp(f), 1)

})
