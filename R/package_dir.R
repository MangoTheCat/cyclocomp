
#' Cyclomatic complexity of a local package
#'
#' Automatically builds the package and installs it to a temporary
#' directory.
#'
#' @param path Path to the root directory of the R package.
#' @return Data frame with two columns: \code{name} and \code{cyclocomp}.
#'
#' @family cyclomatic complexity
#' @importFrom devtools install_local
#' @importFrom callr r
#' @importFrom desc desc_get
#' @export

cyclocomp_package_dir <- function(path = ".") {
  tmp <- tempfile()
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  pkgname <- desc_get("Package", file = file.path(path, "DESCRIPTION"))

  targz <- build_package(path)

  install_local(targz, lib = tmp)

  r(libpath = c(tmp, .libPaths()),
    function(pkg) {
      loadNamespace(pkg)
      cyclocomp::cyclocomp_package(pkg)
    },
    args = list(pkgname)
  )
}

#' @importFrom withr with_dir
#' @importFrom callr rcmd_safe

build_package <- function(path) {

  path <- normalizePath(path)

  tmpdir <- tempfile()
  dir.create(tmpdir)
  on.exit(unlink(tmpdir, recursive = TRUE))

  file.copy(path, tmpdir, recursive = TRUE)

  ## If not a tar.gz, build it. Otherwise just leave it as it is.
  if (file.info(path)$isdir) {
    build_status <- with_dir(
      tmpdir,
      rcmd_safe("build", basename(path))
    )
    unlink(file.path(tmpdir, basename(path)), recursive = TRUE)
  }

  report_system_error("Build failed", build_status)

  ## replace previous handler, no need to clean up any more
  on.exit(NULL)

  file.path(
    tmpdir,
    list.files(tmpdir, pattern = "\\.tar\\.gz$")
  )
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
