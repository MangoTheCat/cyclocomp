
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
