
#' Cyclomatic complexity of a local package
#'
#' Automatically builds the package and installs it to a temporary
#' directory.
#'
#' @param path Path to the root directory of the R package.
#' @return Data frame with two columns: \code{name} and \code{cyclocomp}.
#'
#' @family cyclomatic complexity
#' @importFrom remotes install_local
#' @importFrom desc desc_get
#' @export
cyclocomp_package_dir <- function(path = ".") {
  tmp <- withr::local_tempdir()
  pkgname <- desc_get("Package", file = file.path(path, "DESCRIPTION"))

  install_local(path, lib = tmp)
  withr::local_libpaths(c(tmp, .libPaths()))
  withr::local_namespace(pkg)
  cyclocomp::cyclocomp_package(pkgname)
}

#' @importFrom crayon yellow red underline

report_system_error <- function(msg, status) {

  if (status$status == 0) return()

  if (status$stderr == "") {
    stop(
      msg, ", unknown error, standard output:\n",
      yellow(status$stdout),
      call. = FALSE
    )

  } else {
    stop(
      underline(yellow(paste0("\n", msg, ", standard output:\n\n"))),
      yellow(status$stdout), "\n",
      underline(red("Standard error:\n\n")), red(status$stderr),
      call. = FALSE
    )
  }
}
