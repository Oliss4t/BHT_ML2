library(caret)
library(rpart)
library(rpart.plot)

data <- read.csv("/Users/conor/Downloads/insurance.csv")

names(data)

summary(data$charges)

boxplot(data$charges)

boxplot(data$charges ~ data$age)

median(data$bmi)


set.seed(1)
train <- sample(1:nrow(data), 4*(nrow(data)/5))
tree.data<-rpart(charges~.,data,subset=train)
print(tree.data)


tree.data.full<-rpart(charges~.,data,subset=train,cp=0)
print(tree.data.full)
#rpart.plot(tree.data.full)
cpmatrix<-printcp(tree.data.full)
plotcp(tree.data.full)

prune.data=prune(tree.data,cp=0.019)
prune.data
rpart.plot(prune.data)


#on training data
#full tree
pred.train.full<-predict(tree.data.full,newdata=data[train,])
mean((data$charges[train]-pred.train.full)^2)
##default tree
pred.train.default<-predict(tree.data,newdata=data[train,])
mean((data$charges[train]-pred.train.default)^2)
##pruned tree
pred.train.pruned<-predict(prune.data,newdata=data[train,])
mean((data$medv[train]-pred.train.pruned)^2)

###on the test data
#full tree
pred.test.full<-predict(tree.data.full,newdata=data[-train,])
mean((data$charges[-train]-pred.test.full)^2)
##default tree
pred.test.default<-predict(tree.data,newdata=data[-train,])
mean((data$charges[-train]-pred.test.default)^2)
##pruned tree
pred.test.pruned<-predict(prune.data,newdata=data[-train,])
mean((data$charges[-train]-pred.test.pruned)^2)

###on the test data
#full tree
pred.test.full<-predict(tree.data.full,newdata=data[-train,])
mean((data$charges[-train]-pred.test.full)^2)
##default tree
pred.test.default<-predict(tree.data,newdata=data[-train,])
mean((data$charges[-train]-pred.test.default)^2)
##pruned tree
pred.test.pruned<-predict(prune.data,newdata=data[-train,])
mean((data$charges[-train]-pred.test.pruned)^2)


data.test=data[-train,"charges"]
plot(pred.test.pruned,data.test)
abline(c(0,1))

# Hpyerparamter optimisation



train_index <- createDataPartition(data$charges, p = 0.8, list = FALSE)
train <- data[train_index, ]
test <- data[-train_index, ]

param_grid <- expand.grid(cp = seq(0.01, 0.5, 0.01))

regtree <- train(
  charges ~ .,
  data = train,
  method = "rpart",
  trControl = trainControl(method = "cv", number = 10),
  tuneGrid = param_grid
)

print(regtree$bestTune)

predictions <- predict(regtree, newdata = test)

mse <- mean((test$charges - predictions) ^ 2)
print(mse)
