#Logistic regression
library(caret)
iris = read.csv("C:\\Users\\msk\\Documents\\Academics\\ML\\ML Practical\\iris-data.csv", header = FALSE)
names(iris) = c("v1","v2","v3","v4", "class")

iris <- iris[1:100,]
training_size = floor(0.7*nrow(iris))
train_index = sample(1:nrow(iris), size = training_size) 

train_set = iris[train_index,]
test_set = iris[-train_index,]
logistic_model <- glm (class~., data = train_set, family = binomial)
summary(logistic_model)

predicted_model <- predict(logistic_model, test_set)
predicted_model <- ifelse(predicted_model > 0.5, "Versicolor","Setosa")

confusionMatrix(table(as.vector(predicted_model),as.vector(test_set[,"class"])))

mean(predicted_model==test_set[,5])

