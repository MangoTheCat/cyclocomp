
context("Simple sequences")

test_that("sequences are simple", {

  f <- function() { "foo"; "bar"; "foobar" }
  expect_equal(cyclocomp(f), 1)

  f <- function() { print("foo"); print("bar"); print("foobar") }
  expect_equal(cyclocomp(f), 1)

  f <- function() { }
  expect_equal(cyclocomp(f), 1)

  f <- function(foo) { }
  expect_equal(cyclocomp(f), 1)

  f <- function(foo = 1, bar = 2) { }
  expect_equal(cyclocomp(f), 1)

  f <- function(foo = 1, bar = 2) { bar; foo + 1; print(bar / 2) }
  expect_equal(cyclocomp(f), 1)

})
