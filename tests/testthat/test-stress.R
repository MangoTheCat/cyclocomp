
context("Stress test on base R functions")

do_pkg <- function(pkg) {
  alln <- ls(asNamespace(pkg))
  for (n in alln) {
    expect_silent(
      cyclocomp(get(n, asNamespace(pkg))),
      info = paste0(pkg, "::", n)
    )
  }
}

expect_silent <- function(expr, info = NULL, label = NULL) {
  out <- tryCatch(
    evaluate_promise(expr),
    error = function(e) stop("info: ", info, ", label: ", label)
  )
  expect_equal(out$output, "", info = info, label = label)
  expect_equal(length(out$warnings), 0, info = info, label = label)
  expect_equal(length(out$messages), 0, info = info, label = label)
}

test_that("some base packages", {

  skip("Takes too long to run currently")

  do_pkg("base")
  do_pkg("stats")
  do_pkg("utils")
  do_pkg("methods")
  do_pkg("graphics")
})
