##Function to perform Normal Method##
normal_method <- function(X, Y) {
  
  X = data.matrix(X)    #Coverting Data frame to matrix 
  X_t = t.default(X)
  
  temp = X_t %*% X
  
  if(det(temp) != 0) {
    temp = solve(temp)      #Inverse
    temp1 = X_t %*% Y  
    
    B = temp %*% temp1
    
    B
  }
  else {
    print("Invalid Values!!!")
  }
}


##Function for Data Extraction & Normalising Data##
data_preprocessing <- function(filename) {
  
  data <- read.csv(filename)
  no_of_features <- ncol(data) - 1
  no_of_samples <- nrow(data)
  
  #Normalizing the values of predictors
  data <- normalise(data[,2:ncol(data)], choice = 1)
  
  Y <- data[,ncol(data)]
  X <- matrix(c(1), nrow = no_of_samples)
  
  X <- cbind(X, data[,1:ncol(data)-1])
  
  normal_method(X, Y)
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
data_preprocessing(filename)
