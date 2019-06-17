data_preprocessing <- function(filename, alpha, iterations) {
  
  data <- read.csv(filename)
  data
  # x <- c(0,1,2,3)
  # y <- c(4,7,7,8)
  # data <- data.frame(x,y)
  no_of_features <- ncol(data) - 1
  no_of_samples <- nrow(data)
  
  #Normalizing the values of predictors
  data <- normalise(data[,2:ncol(data)], choice = 1)
  
  Y <- data[,ncol(data)]
  X <- matrix(c(1), nrow = no_of_samples)
  
  X <- cbind(X, data[,1:ncol(data)-1])
  

  
  BETA <- matrix(c(1:no_of_features), nrow = 1)
  
  #grad_descent(X, Y, BETA, no_of_features, no_of_samples, alpha, iterations)
  normal_method(X, Y)
}


data_preprocessing <- function(filename) {
  
  data <- read.csv(filename)
  data
  # x <- c(0,1,2,3)
  # y <- c(4,7,7,8)
  # data <- data.frame(x,y)
  no_of_features <- ncol(data) - 1
  no_of_samples <- nrow(data)
  
  #Normalizing the values of predictors
  data <- normalise(data[,2:ncol(data)], choice = 1)
  
  Y <- data[,ncol(data)]
  X <- matrix(c(1), nrow = no_of_samples)
  
  X <- cbind(X, data[,1:ncol(data)-1])
  
  normal_method(X, Y)
}
