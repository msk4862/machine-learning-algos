inbuilt_regression <- function(filename) {



data_set = read.csv(filename)
data_set <- data_set[,2:ncol(data_set)]

data_set <- normalise(data_set, choice = 1)


fix(data_set)    # display Boston dataset


names(data_set)  # display names of Boston dataset attribute

attach(data_set) # variable corresponding to each attribute 
lm.fit = lm(sales~., data = data_set)  # medv = beta0 + beta1*lstat
lm.fit

}


##Function to choose a normalizing method##
normalise <- function(X, choice = 1) {
  
  
  n <- switch(choice, 
              "1" = rescaling(X),
              "2" = mean_norm(X),
              "3" = std_dev(X))
  
  return(n)
}


##Normalizing methods##
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

filename <- "C:\\Users\\msk\\Documents\\Academics\\ML\\ML Programs/Advertising.csv"
inbuilt_regression(filename)



















