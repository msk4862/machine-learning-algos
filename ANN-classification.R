# neural net - classification
library(nnet)
data("iris")
library(caret)
iris_new = cbind(iris[,-5],class.ind(iris[,5]))

training_size = floor(0.7*nrow(iris_new))
train_index = sample(1:nrow(iris_new), size = training_size) 

train_set = iris_new[train_index,]
test_set = iris_new[-train_index,]

names(train_set) = names(iris_new)
names(test_set) = names(iris_new)

model = neuralnet(setosa+versicolor+virginica~.,data=train_set,hidden = 3)
print(model)
plot(model)

predicted_value = predict(model, test_set)
predicted_value

#which.max = returns index of maximum
idx <- apply(predicted_value, 1, which.max)
predicted <- c('setosa', 'versicolor', 'virginica')[idx]

confusionMatrix(table(predicted, iris[-train_index,5]))

mean(predicted == iris[-train_index,5])
