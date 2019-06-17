##Function to perform Gradient Descent##
grad_descent <-function(X, Y, BETA, no_of_features, no_of_samples, alpha=0.1, iterations=5) {
  result <- data.frame()
  
  #alpha = 0.1
  #iterations = 5
  for(i in (1:iterations)) {
    E = ((BETA %*% t(X))-Y)
    
    J <- (1/(2*no_of_samples))*(E%*%t(E))

    BETA <- BETA -(alpha/no_of_samples)*(E %*% as.matrix(X))

    result <- rbind(result, c(J,BETA))
  }
  col_name <- c("COST FUNCTION(J)")
  col_name <- c(col_name, c(1:ncol(BETA)))
  
  names(result) <- col_name
  plot(c(1:iterations), result[,1], xlab = "Iterations", ylab = "Cost Function(J)", type = "l")
  
  write.csv(result, "result.csv")
}



##Function for Data Extraction & Normalising Data##
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
  
  grad_descent(X, Y, BETA, no_of_features, no_of_samples, alpha, iterations)
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

setwd("C:\\Users\\msk\\Documents\\Academics\\ML\\ML Programs")

filename <- "C:\\Users\\msk\\Documents\\Academics\\ML\\ML Programs/Advertising.csv"
data_preprocessing(filename, 0.1, 10000)
getwd()

