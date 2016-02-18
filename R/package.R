
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

#' @export

cyclocomp <- function(expr) {
  fg <- flowgraph(expr)
  nrow(fg$edges) - nrow(fg$nodes) + 2
}

flowgraph <- function(expr) {

  ## 'last' is a list (composite?) column, and we need to
  ## add it separately, because data.frame() cannot handle it.
  nodes <- data.frame(
    stringsAsFactors = FALSE,
    id = "2",
    type = "exit"
  )
  nodes$last <- list(character())

  ## The structure of the graph is stored here
  edges <- data.frame(
    stringsAsFactors = FALSE,
    from = "1",
    to = "2"
  )

  add_node <- function(x, id, type, last = character()) {
    nr <- data.frame(
      stringsAsFactors = FALSE,
      id = id,
      type = type
    )

    nr$last <- list(last)
    nodes <<- rbind(nodes, nr)
  }

  add_edges <- function(...) {
    args <- list(...)
    for (a in seq_along(args)[-1]) {
      edges <<- rbind(
        edges,
        data.frame(
          stringsAsFactors = FALSE,
          from = args[[a - 1]],
          to = args[[a]]
        )
      )
    }
  }

  walk_lang(expr, id = "1", add_node, add_edges)

  edges <- post_process(nodes, edges)
  nodes <- nodes[, names(nodes) != "last"]

  list(nodes = nodes, edges = edges)
}

id.1 <- function(x) paste0(x, ".1")
id.2 <- function(x) paste0(x, ".2")
id.3 <- function(x) paste0(x, ".3")

## If we have an edge that goes to another block (instead of
## going inside the same block), and the source block of the edge
## has a "last" subblock, then we rewire this edge, such that it
## leaves from the "last" subblock.
##
## The "last" subblock might be a list of ids, in this case we
## need to add extra edges. (This only really happens for
## if-else.
##
## Because we might extend edges as we go along, the loop is
## trciky, so we implement it manually instead of a 'for'.
## The potential new edges are added at the loop cursor, and
## are processed again.

post_process <- function(nodes, edges) {
  e <- 1
  while (e <= nrow(edges)) {
    from <- edges$from[e]
    to   <- edges$to[e]
    rec <- nodes[ nodes$id == from, ]

    ## If there is no last sub-block, or the edge edge is going to
    ## a sub-block, then we are all good. Otherwise rewire.
    if (length(rec$last[[1]]) && ! is_child(to, from)) {
      edges$from[e] <- rec$last[[1]][1]
      for (l in rec$last[[1]][-1]) {
        new_edge <- data.frame(
          stringsAsFactors = FALSE,
          from = l,
          to = to
        )
        edges <- rbind(edges[1:e,], new_edge, edges[-(1:e), ])
      }

    } else {
      e <- e + 1
    }
  }
  edges
}

is_child <- function(child, parent) {
  substring(child, 1, nchar(parent)) == parent && child != parent
}

what_expr <- function(expr) {
  if (is.function(expr)) {
    "function"
  } else if (is.call(expr)) {
    paste0(as.character(expr[[1]]), "()")

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
