filename <- "C:\\Users\\HR-Lab-03\\Desktop\\1225\\ML/Advertising.csv"  


data_set = read.csv(filename)
data_set <- data_set[,2:ncol(data_set)]

data_set <- normalise(data_set, choice = 1)


fix(data_set)    # display Boston dataset


names(data_set)  # display names of Boston dataset attribute

attach(data_set) # variable corresponding to each attribute 
lm.fit = lm(sales~., data = data_set)  # medv = beta0 + beta1*lstat
lm.fit

# OR

lm.fit = lm(medv~lstat,data=Boston)
lm.fit

summary(lm.fit)  # summary of the linear regression model
names(lm.fit)    # attribute names of the fitted model
lm.fit["residuals"]
lm.fit["model"]
plot(lm.fit)

coef(lm.fit)   # coefficient of the variables
confint(lm.fit) #confidence intervals for one or more parameters in a fitted model

predict(lm.fit,data.frame(lstat=c(5,10,15)),interval="confidence")
predict(lm.fit,data.frame(lstat=c(5,10,15)),interval="prediction")
plot(lstat,medv) #plot medv vs lstat

abline(lm.fit)   # regression model line
abline(lm.fit,lwd=3) # regression model line with width 3
abline(lm.fit,lwd=1,col="red")


plot(lstat,medv,col="red")
plot(lstat,medv,pch=20)
plot(lstat,medv,pch="+")
plot(1:20,1:20,pch=10:30)
min(Boston["14"])
max(Boston["medv"])
par(mfrow=c(2,2))
plot(lm.fit)

plot(predict(lm.fit),residuals(lm.fit))
plot(predict(lm.fit),rstudent(lm.fit))
plot(hatvalues(lm.fit))
which.max(hatvalues(lm.fit))


# Multiple linear regression 
lm.fit = lm(medv~lstat+age,data=Boston)  # medv = beta0 + beta1*lstat + beta2*age
summary(lm.fit)
plot(lm.fit)

lm.fit = lm(medv~.,data=Boston)# medv = beta0 + beta1*lstat+...
summary(lm.fit)
plot(lm.fit)

names(lm.fit)
lm.fit["residuals"]
lm.fit["model"]
