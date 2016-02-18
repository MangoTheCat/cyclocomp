
context("Return")

test_that("returns work well", {

  f <- function() {
    g <- function() {
      "foo"
      return()
      "bar"
    }
  }
  expect_equal(cyclocomp(f), 2)

  f <- function() {
    g <- function() {
      "foo"
      "bar"
    }
  }
  expect_equal(cyclocomp(f), 1)

  f <- function() {
    return()
  }
  expect_equal(cyclocomp(f), 1)

  f <- function() {
    g <- function() {
      "foo"
      "bar"
    }
    return()
  }
  expect_equal(cyclocomp(f), 1)

  f <- function() {
    g <- function() {
      "foo"
      "bar"
      return("ok")
    }
  }
  expect_equal(cyclocomp(f), 1)

  f <- function() { return(); 1; }
  expect_equal(cyclocomp(f), 2)
})
