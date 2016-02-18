
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
  unique(edges)
}

is_child <- function(child, parent) {
  substring(child, 1, nchar(parent)) == parent && child != parent
}
