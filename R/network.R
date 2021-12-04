#' Find net bridges
#'
#' @param net network
#'
#' @export
#'
bridges <- function(net){
  bridges <- c() # empty vector to store bridge names in
  number_components <- length(igraph::decompose.graph(net)) # grab the number of components in the original raph
  for (i in 1:length(igraph::E(net))) { # begin a loop through all of the edges
    net_sub <- igraph::delete.edges(net, i) # delete the edge in question
    if(length(igraph::decompose.graph(net_sub) ) > number_components){ # if the number of components has increased
      bridges <- c(i, bridges)  # save this edge as a bridge
    }
  }
  return(bridges) # return the set of bridges
}

#' Find tie ranges
#' @param net network
#' @export
tie_range <- function(net){
  tie_ranges <- c() # empty vector to save ranges
  for (i in 1:length(igraph::E(net))) { # loop through edges
    incident_vertices <- igraph::ends(net, i) # which nodes are incident to the edge in quetion
    net_sub <- igraph::delete.edges(net, i) # delete the edge
    updated_distance <-
      igraph::distances(net_sub,
                v = incident_vertices[1, 1],
                to = incident_vertices[1, 2],
                mode = "all") # evaluate the distance for the previously connected nodes
    tie_ranges <- c(tie_ranges, updated_distance) # save the result
  }
  return(tie_ranges) # return the resulting tie ranges
}
