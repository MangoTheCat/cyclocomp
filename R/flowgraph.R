
#' @importFrom utils head tail

flowgraph <- function(expr) {

  prealloc <- 4000

  num_nodes <- 0
  nodes <- list(
    id = rep("", prealloc),
    type = rep("", prealloc)
  )
  nodeslast <- replicate(prealloc, character())

  ## The structure of the graph is stored here
  num_edges <- 0
  edges <- list(
    from = rep("", prealloc),
    to = rep("", prealloc)
  )

  add_node <- function(x, id, type, last = character()) {
    num_nodes <<- num_nodes + 1
    nodes$id[num_nodes] <<- id
    nodes$type[num_nodes] <<- type
    nodeslast[[num_nodes]] <<- last
  }

  add_to_last <- function(elem, id) {
    w <- which(nodes$id == elem)
    nodeslast[[w]] <<- c(nodeslast[[w]], id)
  }

  add_edges <- function(...) {
    args <- unlist(list(...))
    n <- length(args) - 1
    edges$from[num_edges + (1:n)] <<- head(args, -1)
    edges$to[num_edges + (1:n)]   <<- tail(args, -1)
    num_edges <<- num_edges + n
  }

  add_node(NULL, "2", "exit")
  add_edges("1", "2")

  breaks <- character()

  breaks_push <- function(id) {
    breaks <<- c(breaks, id)
  }

  breaks_pop <- function() {
    breaks <<- head(breaks, -1)
  }

  breaks_tail <- function() {
    tail(breaks, 1)
  }

  nexts <- character()

  nexts_push <- function(id) {
    nexts <<- c(nexts, id)
  }

  nexts_pop <- function() {
    nexts <<- head(nexts, -1)
  }

  nexts_tail <- function() {
    tail(nexts, 1)
  }

  functions <- character()

  functions_push <- function(id) {
    functions <<- c(functions, id)
  }

  functions_pop <- function() {
    functions <<- head(functions, -1)
  }

  functions_tail <- function() {
    tail(functions, 1)
  }

  walk_lang <- function(x, id) {

    if (is.call(x) && identical(x[[1]], quote(return)) &&
        is.primitive(eval(x[[1]]))) {
      walk_return(x, id)

    } else if (is.call(x) && identical(x[[1]], quote(`for`))) {
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

    } else if (is.call(x) &&
               (identical(x[[1]], quote(`&&`)) || identical(x[[1]], quote(`||`)))) {
      walk_andor(x, id)

    } else if (is.function(x)) {
      walk_function(x, id)

    } else if (is.call(x) && identical(x[[1]], quote(`function`))) {
      walk_function_call(x, id)

    } else if (is.call(x) || is.pairlist(x) || is.expression(x) || is.list(x)) {
      walk_list(x, id)

    } else {
      add_node(x, id, what_atomic(x))
    }

  }

  walk_return <- function(x, id) {
    add_to_last(functions_tail(), id)
    walk_list(x, id)
  }

  walk_for <- function(x, id) {
    add_node(x, id, "for", last = c(id.1(id), id.2(id)))
    add_edges(id, id.1(id), id.2(id), id.2(id))

    ## Don't add the loop here because we might break
    ## in the vector expression. So only after that.
    walk_lang(x[[3]], id.1(id))
    breaks_push(id)
    nexts_push(id.2(id))
    walk_lang(x[[4]], id.2(id))
    breaks_pop()
    nexts_pop()
  }

  walk_while <- function(x, id) {
    add_node(x, id, "while", last = c(id.1(id), id.2(id)))
    add_edges(id, id.1(id), id.2(id), id.1(id))

    ## Don't add the loop here because we might break
    ## loop condition. So only after that.
    walk_lang(x[[2]], id.1(id))
    breaks_push(id)
    nexts_push(id.1(id))
    walk_lang(x[[3]], id.2(id))
    breaks_pop()
    nexts_pop()
  }

  walk_repeat <- function(x, id) {
    add_node(x, id, "repeat", last = id.1(id))
    add_edges(id, id.1(id), id.1(id))

    breaks_push(id)
    nexts_push(id.1(id))
    walk_lang(x[[2]], id.1(id))
    breaks_pop()
    nexts_pop()
  }

  walk_break <- function(x, id) {
    add_node(x, id, "break")
    add_to_last(breaks_tail(), id)
  }

  walk_next <- function(x, id) {
    add_node(x, id, "next")
    add_edges(id, nexts_tail())
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

  walk_andor <- function(x, id) {
    name <- as.character(x[[1]])
    add_node(x, id, name, last = c(id.1(id), id.2(id)))
    add_edges(id, id.1(id), id.2(id))
    walk_lang(x[[2]], id.1(id))
    walk_lang(x[[3]], id.2(id))
  }

  walk_function <- function(x, id) {
    ## Function without an argument
    if (is.null(formals(x))) {
      add_node(x, id, "function", last = id.1(id))
      add_edges(id, id.1(id))
      functions_push(id)
      walk_lang(body(x), id.1(id))
      functions_pop()

    } else {
      ## Function with arguments
      add_node(x, id, "function", last = id.2(id))
      add_edges(id, id.1(id), id.2(id))
      walk_lang(formals(x), id.1(id))
      functions_push(id)
      walk_lang(body(x), id.2(id))
      functions_pop()
    }
  }

  walk_function_call <- function(x, id) {
    if (is.null(x[[2]])) {
      add_node(x, id, "function", last = id.1(id))
      add_edges(id, id.1(id))
      functions_push(id)
      walk_lang(x[[3]], id.1(id))
      functions_pop()

    } else {
      ## Function with arguments
      add_node(x, id, "function", last = id.2(id))
      add_edges(id, id.1(id), id.2(id))
      walk_lang(x[[2]], id.1(id))
      functions_push(id)
      walk_lang(x[[3]], id.2(id))
      functions_pop()
    }
  }

  walk_list <- function(x, id) {
    what <- what_expr(x)
    x <- as.list(x)
    last <- if (length(x) == 0) character() else paste0(id, ".", length(x))
    add_node(x, id, what, last = last)
    if (length(x) != 0) {
      add_edges(id, id.1(id))
      for (i in seq_along(x)) {
        if (i != 1) add_edges(paste0(id, ".", i - 1), paste0(id, ".", i))
        walk_lang(x[[i]], paste0(id, ".", i))
      }
    }
  }

  walk_lang(expr, id = "1")

  nodes <- lapply(nodes, head, num_nodes)
  edges <- lapply(edges, head, num_edges)

  nodes <- as.data.frame(stringsAsFactors = FALSE, nodes)
  nodes$last <- head(nodeslast, num_nodes)
  edges <- as.data.frame(stringsAsFactors = FALSE, edges)

  edges <- post_process(nodes, edges)
  nodes <- nodes[, names(nodes) != "last"]

  list(nodes = nodes, edges = edges)
}

id.1 <- function(x) paste0(x, ".1")
id.2 <- function(x) paste0(x, ".2")
id.3 <- function(x) paste0(x, ".3")
