
walk_lang <- function(x, id, add_node, add_edges) {

  if (is.call(x) && identical(x[[1]], quote(`for`))) {
    walk_for(x, id, add_node, add_edges)

  } else if (is.call(x) && identical(x[[1]], quote(`while`))) {
    walk_while(x, id, add_node, add_edges)

  } else if (is.call(x) && identical(x[[1]], quote(`repeat`))) {
    walk_repeat(x, id, add_node, add_edges)

  } else if (is.call(x) && identical(x[[1]], quote(`break`))) {
    walk_break(x, id, add_node, add_edges)

  } else if (is.call(x) && identical(x[[1]], quote(`next`))) {
    walk_next(x, id, add_node, add_edges)

  } else if (is.call(x) && identical(x[[1]], quote(`if`)) && length(x) == 3) {
    walk_if(x, id, add_node, add_edges)

  } else if (is.call(x) && identical(x[[1]], quote(`if`)) && length(x) == 4) {
    walk_ifelse(x, id, add_node, add_edges)

  } else if (is.call(x) &&
             (identical(x[[1]], quote(`&&`)) || identical(x[[1]], quote(`||`)))) {
    walk_andor(x, id, add_node, add_edges)

  } else if (is.function(x)) {
    walk_function(x, id, add_node, add_edges)

  } else if (is.call(x) || is.pairlist(x) || is.expression(x) || is.list(x)) {
    walk_list(x, id, add_node, add_edges)

  } else {
    add_node(x, id, what_atomic(x))
  }

  invisible()
}

walk_for <- function(x, id, add_node, add_edges) {
  add_node(x, id, "for", last = c(id.1(id), id.2(id)))
  add_edges(id, id.1(id), id.2(id), id.2(id))
  walk_lang(x[[3]], id.1(id), add_node, add_edges)
  walk_lang(x[[4]], id.2(id), add_node, add_edges)
}

walk_while <- function(x, id, add_node, add_edges) {
  add_node(x, id, "while", last = c(id.1(id), id.2(id)))
  add_edges(id, id.1(id), id.2(id), id.1(id))
  walk_lang(x[[2]], id.1(id), add_node, add_edges)
  walk_lang(x[[3]], id.2(id), add_node, add_edges)
}

walk_repeat <- function(x, id, add_node, add_edges) {
  add_node(x, id, "repeat", last = id.1(id))
  add_edges(id, id.1(id), id.1(id))
  walk_lang(x[[2]], id.1(id), add_node, add_edges)
}

walk_break <- function(x, id, add_node, add_edges) {
  add_node(x, id, "break")
}

walk_next <- function(x, id, add_node, add_edges) {
  add_node(x, id, "next")
}

walk_if <- function(x, id, add_node, add_edges) {
  add_node(x, id, "if", last = c(id.1(id), id.2(id)))
  add_edges(id, id.1(id), id.2(id))
  walk_lang(x[[2]], id.1(id), add_node, add_edges)
  walk_lang(x[[3]], id.2(id), add_node, add_edges)
}

walk_ifelse <- function(x, id, add_node, add_edges) {
  add_node(x, id, "ifelse", last = c(id.2(id), id.3(id)))
  add_edges(id, id.1(id), id.2(id))
  add_edges(id.1(id), id.3(id))
  walk_lang(x[[2]], id.1(id), add_node, add_edges)
  walk_lang(x[[3]], id.2(id), add_node, add_edges)
  walk_lang(x[[4]], id.3(id), add_node, add_edges)
}

walk_andor <- function(x, id, add_node, add_edges) {
  name <- as.character(x[[1]])
  add_node(x, id, name, last = c(id.1(id), id.2(id)))
  add_edges(id, id.1(id), id.2(id))
  walk_lang(x[[2]], id.1(id), add_node, add_edges)
  walk_lang(x[[3]], id.2(id), add_node, add_edges)
}

walk_function <- function(x, id, add_node, add_edges) {
  ## Function without an argument
  if (is.null(formals(x))) {
    add_node(x, id, "function", last = id.1(id))
    add_edges(id, id.1(id))
    walk_lang(body(x), id.1(id), add_node, add_edges)

  } else {
    ## Function with arguments
    add_node(x, id, "function", last = id.2(id))
    add_edges(id, id.1(id), id.2(id))
    walk_lang(formals(x), id.1(id), add_node, add_edges)
    walk_lang(body(x), id.2(id), add_node, add_edges)
  }
}

walk_list <- function(x, id, add_node, add_edges) {
  x <- as.list(x)
  what <- what_expr(x)
  last <- if (length(x) == 0) character() else paste0(id, ".", length(x))
  add_node(x, id, what, last = last)
  add_edges(id, id.1(id))
  for (i in seq_along(x)) {
    if (i != 1) add_edges(paste0(id, ".", i - 1), paste0(id, ".", i))
    walk_lang(x[[i]], paste0(id, ".", i), add_node, add_edges)
  }
}
