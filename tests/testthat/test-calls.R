
context("Function calls")

test_that("function calls are OK", {

  f <- function() print()
  expect_equal(cyclocomp(f), 1)

})
