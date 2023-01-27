# LASSO -------------------------------------------------------------------

#' Create and test correlation-based prediction models
#' Training and validation predictors can be different (x1, x2)
#'
#' @param x1 training predictors (dataframe or tibble)
#' @param y response variable
#' @param x2 validation predictors, will be the same as training ones if not provided
#' @param prop the proportion of the sample to use for training (vs holdout)
#'
#' @export
#'
cor_cor <- function(x1, y, x2 = NULL, prop = .67) {
  if(is.null(x2)) x2 <- x1
  s <- sample(nrow(x1), prop * nrow(x1))
  x1 <- sapply(x1, scale)
  x2 <- sapply(x2, scale)
  (x2[-s, ] %*% stats::cor(x1[s, ], y[s])) %>%
    stats::cor(y[-s])
}

#' Function to create and validate elastic net prediction models
#'
#' @param x1 training predictors (dataframe or tibble)
#' @param y response variable
#' @param x2 validation predictors, will be the same as training ones if not provided
#' @param prop the proportion of the sample to use for training (vs holdout)
#' @param alpha passed to cv.glmnet; default is .05
#' @param lambda.type passed to cv.glmnet; default is "lambda.min"
#'
#' @export
#'
glmnet_cor <- function(x1, y, x2 = NULL, prop = .67, alpha = .05, lambda.type = "lambda.min") {
  if (is.null(x2)) x2 <- x1
  s <- sample(nrow(x1), prop * nrow(x1))
  x1 <- sapply(x1, scale)
  x2 <- sapply(x2, scale)
  glmnet::cv.glmnet(x1[s, ], y[s], alpha = alpha) %>%
    stats::predict(x2[-s, ], s = lambda.type) %>%
    stats::cor(y[-s])
}


# Network -----------------------------------------------------------------
#' Find Net Bridges
#' @param net network graph (class ‘igraph’)
#' @export
bridges <- function(net) {
  # TODO: create a purrr version
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

#' Find Tie Ranges
#' @param net network graph (class ‘igraph’)
#' @export
tie_range <- function(net) {
  # TODO: create a purrr version
  tie_ranges <- c() # empty vector to save ranges
  for (i in 1:length(igraph::E(net))) { # loop through edges
    incident_vertices <- igraph::ends(net, i) # which nodes are incident to the edge in quetion
    net_sub <- igraph::delete.edges(net, i) # delete the edge
    updated_distance <- igraph::distances( # evaluate the distance for the previously connected nodes
      net_sub,
      v = incident_vertices[1, 1],
      to = incident_vertices[1, 2],
      mode = "all"
    )
    tie_ranges <- c(tie_ranges, updated_distance) # save the result
  }
  return(tie_ranges) # return the resulting tie ranges
}
