
context("Break in loops")

test_that("break in simple loops", {

  f <- function() { for (i in 1:10) { 1; break; 2 } }
  expect_equal(cyclocomp(f), 4)

  f <- function() { for (i in 1:10) { 1; 2; break } }
  expect_equal(cyclocomp(f), 3)

})

test_that("break in the loop condition", {

  f <- function() {
    for (i in 1:2) {
      for (j in { break; 1:10 }) {
        "foobar"
      }
      i
    }
    i
  }
  expect_equal(cyclocomp(f), 6)

  f <- function() {
    for (i in 1:2) {
      for (j in 1:10) {
        break
        "foobar"
      }
      i
    }
    i
  }
  expect_equal(cyclocomp(f), 6)
  
})
