
Cal_B0 = function(x,y,B1){

  return(mean(y)-mean(x)*B1)
}

Cal_B1 = function(x,y){
  B1 = sum((x - mean(x))*(y-mean(y)))/sum((x - mean(x))^2)
  return(B1)
  
}

LinearRegression = function(X,Y){

  #Finding value for B0 and B1
  Beta1 = Cal_B1(X,Y)
  Beta0 = Cal_B0(X,Y,Beta1)
  
  
  #Matrix Adjustment
  Beta = matrix(c(Beta0,Beta1),nrow=1,byrow = TRUE)
  x = matrix(c(1), nrow = length(X))
  x = cbind(x, X)
  
  Ycap = Beta%*%t(x)
  
  Error = Ycap - Y
  Residual_Error = sum(Error^2)

  cat("","Beta Values :\n",Beta,"\n\n")
  
  #Finding Best Fit Line
  BestFit(x,y,Beta0,Beta1)
  
  #printing Y^ (estimated Y) values
  cat("\n\n Y^ : \n")
  print(Ycap)
  
  eq = Beta0 + Beta1*X
  
  plot(X, eq)
  
  cat("\n\n Residual Error = ",Residual_Error)
  
  
  
}

X <- Boston[,"lstat"]

Y <- Boston[,"medv"]

LinearRegression(X,Y)


