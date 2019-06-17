rescaling <- function(X) {
  
  for(i in 1:ncol(X)) {
    minx <- min(X[,i])
    maxx <- max(X[,i])
    
    range = maxx - minx
    
    X[,i] <- (X[,i]-minx)/range
  }
  
  return(X)
  
}

mean_norm <- function(X) {
  
  for(i in 1:ncol(X)) {
    minx <- min(X[,i])
    maxx <- max(X[,i])
    
    range = maxx - minx
    
    X[,i] <- (X[,i]-mean(X[,i]))/range
  }
  
  return(X)
}

std_dev <- function(X) {
  for(i in 1:ncol(X)) {
    minx <- min(X[,i])
    maxx <- max(X[,i])
    

    X[,i] <- (X[,i]-mean(X[,i]))/sd(X[,i])
  }
  
  return(X)
}
