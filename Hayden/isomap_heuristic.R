library(vegan)

isomap_heuristic <- function(dist_matrix, k = 0) {
  if (k == 0) {
    n = dim(dist_matrix)[[1]]
    k = floor(log(n))
  }
  
  connected = FALSE
  
  try(
    # Check if connected at k.
    {
    iso_dist = as.matrix(isomapdist(dist_matrix, k=k))
    connected = TRUE
    }
  )
  
  while (connected) {
    k <- floor(k/2)
    tryCatch(
      {
        iso_dist = as.matrix(isomapdist(dist_matrix, k = k))
      }, error = function(e) {
        connected <<- FALSE
      }
    )
  } 
  
  while (!connected) {
    k <- k + 1
    try(
      {
        iso_dist = as.matrix(isomapdist(dist_matrix, k = k))
        connected <- TRUE
      }
    )
  }
  return(as.matrix(iso_dist))
}

