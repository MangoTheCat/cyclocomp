
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

  walk_lang <- function(x, id) {

    if (is.call(x) && identical(x[[1]], quote(`for`))) {
      walk_for(x, id)

    } else if (is.call(x) && identical(x[[1]], quote(`while`))) {
      walk_while(x, id)

    } else if (is.call(x) && identical(x[[1]], quote(`repeat`))) {
      walk_repeat(x, id)

    } else if (is.call(x) && identical(x[[1]], quote(`break`))) {
      walk_break(x, id)

    } else if (is.call(x) && identical(x[[1]], quote(`next`))) {
      walk_next(x, id)

    } else if (is.call(x) && identical(x[[1]], quote(`if`)) && length(x) == 3) {
      walk_if(x, id)

    } else if (is.call(x) && identical(x[[1]], quote(`if`)) && length(x) == 4) {
      walk_ifelse(x, id)

    } else if (is.function(x)) {
      walk_function(x, id)

    } else if (is.call(x) || is.pairlist(x) || is.expression(x) || is.list(x)) {
      walk_list(x, id)

    } else {
      add_node(x, id, what_atomic(x))
    }

    invisible()
  }

  walk_for <- function(x, id) {
    add_node(x, id, "for", last = c(id.1(id), id.2(id)))
    add_edges(id, id.1(id), id.2(id), id.2(id))
    walk_lang(x[[3]], id.1(id))
    walk_lang(x[[4]], id.2(id))
  }

  walk_while <- function(x, id) {
    add_node(x, id, "while", last = c(id.1(id), id.2(id)))
    add_edges(id, id.1(id), id.2(id), id.1(id))
    walk_lang(x[[2]], id.1(id))
    walk_lang(x[[3]], id.2(id))
  }

  walk_repeat <- function(x, id) {
    add_node(x, id, "repeat", last = id.1(id))
    add_edges(id, id.1(id), id.1(id))
    walk_lang(x[[2]], id.1(id))
  }

  walk_break <- function(x, id) {
    add_node(x, id, "break")
  }

  walk_next <- function(x, id) {
    add_node(x, id, "next")
  }

  walk_if <- function(x, id) {
    add_node(x, id, "if", last = c(id.1(id), id.2(id)))
    add_edges(id, id.1(id), id.2(id))
    walk_lang(x[[2]], id.1(id))
    walk_lang(x[[3]], id.2(id))
  }

  walk_ifelse <- function(x, id) {
    add_node(x, id, "ifelse", last = c(id.2(id), id.3(id)))
    add_edges(id, id.1(id), id.2(id))
    add_edges(id.1(id), id.3(id))
    walk_lang(x[[2]], id.1(id))
    walk_lang(x[[3]], id.2(id))
    walk_lang(x[[4]], id.3(id))
  }

  walk_function <- function(x, id) {
    ## Function without an argument
    if (is.null(formals(x))) {
      add_node(x, id, "function", last = id.1(id))
      add_edges(id, id.1(id))
      walk_lang(body(x), id.1(id))

    } else {
    ## Function with arguments
      add_node(x, id, "function", last = id.2(id))
      add_edges(id, id.1(id), id.2(id))
      walk_lang(formals(x), id.1(id))
      walk_lang(body(x), id.2(id))
    }
  }

  walk_list <- function(x, id) {
    x <- as.list(x)
    what <- what_expr(x)
    last <- if (length(x) == 0) character() else paste0(id, ".", length(x))
    add_node(x, id, what, last = last)
    add_edges(id, id.1(id))
    for (i in seq_along(x)) {
      if (i != 1) add_edges(paste0(id, ".", i - 1), paste0(id, ".", i))
      walk_lang(x[[i]], paste0(id, ".", i))
    }
  }

  walk_lang(expr, id = "1")

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
