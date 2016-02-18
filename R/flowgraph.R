
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

  add_to_last <- function(elem, id) {
    w <- which(nodes$id == elem)
    nodes$last[[w]] <<- c(nodes$last[[w]], id)
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
  
  loops <- character()

  loops_push <- function(id) {
    loops <<- c(loops, id)
  }

  loops_pop <- function() {
    loops <<- head(loops, -1)
  }

  loops_tail <- function() {
    tail(loops, 1)
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

    } else if (is.call(x) &&
               (identical(x[[1]], quote(`&&`)) || identical(x[[1]], quote(`||`)))) {
      walk_andor(x, id)

    } else if (is.function(x)) {
      walk_function(x, id)

    } else if (is.call(x) || is.pairlist(x) || is.expression(x) || is.list(x)) {
      walk_list(x, id)

    } else {
      add_node(x, id, what_atomic(x))
    }

  }

  walk_for <- function(x, id) {
    add_node(x, id, "for", last = c(id.1(id), id.2(id)))
    add_edges(id, id.1(id), id.2(id), id.2(id))

    loops_push(id)
    walk_lang(x[[3]], id.1(id))
    walk_lang(x[[4]], id.2(id))
    loops_pop()
  }

  walk_while <- function(x, id) {
    add_node(x, id, "while", last = c(id.1(id), id.2(id)))
    add_edges(id, id.1(id), id.2(id), id.1(id))

    loops_push(id)
    walk_lang(x[[2]], id.1(id))
    walk_lang(x[[3]], id.2(id))
    loops_pop()
  }

  walk_repeat <- function(x, id) {
    add_node(x, id, "repeat", last = id.1(id))
    add_edges(id, id.1(id), id.1(id))

    loops_push(id)
    walk_lang(x[[2]], id.1(id))
    loops_pop()
  }

  walk_break <- function(x, id) {
    add_node(x, id, "break")
    add_to_last(loops_tail(), id)
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
