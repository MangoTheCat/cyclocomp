
context("&& and ||")

test_that("&& works well", {

  f <- function() { FALSE && TRUE }
  expect_equal(cyclocomp(f), 2)
  
})

test_that("|| works well", {

  f <- function() { FALSE || TRUE }
  expect_equal(cyclocomp(f), 2)
  
})
