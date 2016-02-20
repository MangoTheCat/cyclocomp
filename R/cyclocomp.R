
#' Cyclomatic Complexity of R Code
#'
#' Cyclomatic complexity is a software metric (measurement), used to indicate
#' the complexity of a program. It is a quantitative measure of the number of
#' linearly independent paths through a program's source code. It was developed
#' by Thomas J. McCabe, Sr. in 1976.
#'
#' @docType package
#' @name cyclocomp
NULL

#' Cyclomatic Complexity of R Code
#'
#' Cyclomatic complexity is a software metric (measurement), used to indicate
#' the complexity of a program. It is a quantitative measure of the number of
#' linearly independent paths through a program's source code. It was developed
#' by Thomas J. McCabe, Sr. in 1976.
#'
#' @param expr An R function or expression.
#' @return Integer scalar, the cyclomatic complexity of the
#'   expression.
#' @export
#' @family cyclomatic complexity
#'
#' @examples
#' ## Supply a function
#' cyclocomp(
#'   function(arg) { calulate(this); and(that) }
#' )
#' cyclocomp(ls)
#' cyclocomp(cyclocomp)
#' 
#' ## Or a quoted expression
#' cyclocomp(quote( if (condition) "foo" else "bar" ))
#' cyclocomp(quote( while (condition) { loop } ))
#'
#' ## Complexity of individual control flow constructs
#' cyclocomp(quote({
#'   if (condition) this
#' }))
#'
#' cyclocomp(quote({
#'   if (condition) this else that
#' }))
#'
#' cyclocomp(quote({
#'   for (var in seq) expr
#' }))
#'
#' cyclocomp(quote({
#'   while (cond) expr
#' }))
#'
#' cyclocomp(quote({
#'   repeat expr
#' }))
#'
#' cyclocomp(quote({
#'   for (var in seq) {
#'     this
#'     break
#'     that
#'   }
#' }))
#'
#' cyclocomp(quote({
#'   for (var in seq) {
#'     this
#'     next
#'     that
#'   }
#' }))

cyclocomp <- function(expr) {
  fg <- flowgraph(expr)
  nrow(fg$edges) - nrow(fg$nodes) + 2L
}

#' Cyclomatic complexity of the objects in an installed package
#'
#' Note that the package must be installed.
#'
#' @param package Package name, character scalar.
#' @return Data frame with two columns: \code{name} and \code{cyclocomp}.
#'
#' @family cyclomatic complexity
#' @export
#' @examples
#' ## They might take a while to run
#' \donttest{
#' cyclocomp_package("grDevices")
#' cyclocomp_package("methods")
#' }

cyclocomp_package <- function(package) {
  names <- ls(asNamespace(package))
  cc <- vapply(names, function(n) cyclocomp(get(n, asNamespace(package))), 1L)
  data.frame(
    stringsAsFactors = FALSE,
    name = unname(names),
    cyclocomp = unname(cc)
  )
}
