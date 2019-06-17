normalise <- function(X, choice = 1) {
  

  n <- switch(choice, 
         "1" = rescaling(X),
         "2" = mean_norm(X),
         "3" = std_dev(X))
  
  return(n)
}
