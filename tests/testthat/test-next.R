
context("next")

test_that("next works", {

  f <- function() { for (i in 1:10) { 1; next; 2 } }
  expect_equal(cyclocomp(f), 4)

  f <- function() { for (i in 1:10) { 1; next } }
  expect_equal(cyclocomp(f), 3)

  f <- function() { while (TRUE) { 1; next; 2 } }
  expect_equal(cyclocomp(f), 4)

  f <- function() { while (TRUE) { 1; next } }
  expect_equal(cyclocomp(f), 3)

  f <- function() { repeat { 1; next; 2 } }
  expect_equal(cyclocomp(f), 3)

  f <- function() { repeat { 1; next } }
  expect_equal(cyclocomp(f), 2)
})
