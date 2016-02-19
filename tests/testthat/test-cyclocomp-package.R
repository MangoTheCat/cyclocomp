
context("CC of a package")

test_that("cyclocomp_package", {
  res <- cyclocomp_package("cyclocomp")
  expect_true(inherits(res, "data.frame"))
  expect_true("cyclocomp" %in% res$name)
  expect_equal(colnames(res), c("name", "cyclocomp"))
})
