
#' Cyclomatic Complecity of R Code
#'
#' Cyclomatic complexity is a software metric (measurement), used to indicate
#' the complexity of a program. It is a quantitative measure of the number of
#' linearly independent paths through a program's source code. It was developed
#' by Thomas J. McCabe, Sr. in 1976.
#'
#' @docType package
#' @name cyclocomp
NULL

#' Cyclomatic Complecity of R Code
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
