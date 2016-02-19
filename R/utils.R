
what_expr <- function(expr) {
  if (is.call(expr)) {
    paste0(as.character(expr[[1]])[1], "()")

  } else {
    typeof(expr)
  }
}

## We need the tryCatch, because the string might contain invalid
## multi-byte characters. E.g.
## substring('\x93', 1, 10) and nchar('\x93') both fail

what_atomic <- function(expr) {
  if (is.character(expr)) {
    tryCatch(
      paste0("\"", substring(expr[1], 1, 10), "\""),
      error = function(e) "\"<string>\""
    )
  } else if (is.name(expr)) {
    as.character(expr)
  } else {
    "atomic"
  }
}
