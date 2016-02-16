
context("If")

test_that("simple if statements", {

  f <- function() { if (TRUE) "foo"; "bar"; "foobar" }
  expect_equal(cyclocomp(f), 2)

  f <- function() { "bar"; if (TRUE) "foo"; "bar"; "foobar" }
  expect_equal(cyclocomp(f), 2)

  f <- function() { if (TRUE) "foo" }
  expect_equal(cyclocomp(f), 2)

  f <- function() { if (TRUE) "foo" else "bar" }
  expect_equal(cyclocomp(f), 2)

  f <- function() { if (TRUE) "foo" else "bar"; "foobar" }
  expect_equal(cyclocomp(f), 2)

  f <- function() { "bar"; if (TRUE) "foo" else "bar" }
  expect_equal(cyclocomp(f), 2)
})
