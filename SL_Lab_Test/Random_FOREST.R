install.packages("randomForest")
library(randomForest)
#regression
data(Boston)
library(MASS)
#Boston
data(Boston)
dataset<-as.data.frame(Boston)
dataset
#splitting of data
sample<-sample(c(TRUE,FALSE),nrow(dataset),replace = TRUE,prob = c(0.7,0.5))
train<-dataset[sample,]
test<-dataset[!sample,]

rf.boston<-randomForest(medv~.,data = train,mtry=13,importance=TRUE)
rf.boston
#plot
ypred<-predict(rf.boston,test)
plot(ypred,test$medv)
abline(0,1)
mean((ypred-test$medv)^2)
#metrics
MAE(ypred,test$medv)
MSE(ypred,test$medv)
RMSE(ypred,test$medv)
MAPE(ypred,test$medv)


rf.boston<-randomForest(medv~.,data = train,mtry=13,ntree=25,importance=TRUE)
rf.boston
#plot
ypred<-predict(rf.boston,test)
plot(ypred,test$medv)
abline(0,1)
mean((ypred-test$medv)^2)
#metrics
MAE(ypred,test$medv)
MSE(ypred,test$medv)
RMSE(ypred,test$medv)
MAPE(ypred,test$medv)

rf.boston<-randomForest(medv~.,data = train,mtry=6,ntree=25,importance=TRUE)
rf.boston
#plot
ypred<-predict(rf.boston,test)
plot(ypred,test$medv)
abline(0,1)
mean((ypred-test$medv)^2)
#metrics
MAE(ypred,test$medv)
MSE(ypred,test$medv)
RMSE(ypred,test$medv)
MAPE(ypred,test$medv)
#variable importance
importance(rf.boston)
varImpPlot(rf.boston)


#classification
data("heart")
Heart_data<-as.data.frame(heart)
Heart_data
#splitting of data
sample<-sample(c(TRUE,FALSE),nrow(Heart_data),replace = TRUE,prob = c(0.7,0.5))
train<-Heart_data[sample,]
test<-Heart_data[!sample,]
#training

tree.heart<-randomForest(factor(y)~.,data = train)
summary(tree.heart)
tree.heart


#confusion Matrix
ypred<-predict(tree.heart,test,type='class')
ypred
table(predict=ypred,truth=test$y)
#Metrics
ConfusionMatrix(ypred,test$y)
Accuracy(ypred,test$y)
Precision(ypred,test$y)
Recall(ypred,test$y)
F1_Score(ypred,test$y)

#pruning
plotcp(tree.heart)
tree.heart$cptable
index<-which.min(tree.heart$cptable[,"xerror"])
cpopt<-tree.heart$cptable[index,"CP"]
cpopt
opttree.heart<-prune(tree.heart,cp=cpopt)
rpart.plot(opttree.heart)

#confusion Matrix
ypred<-predict(opttree.heart,test,type='class')
ypred
table(predict=ypred,truth=test$y)
#Metrics
ConfusionMatrix(ypred,test$y)
Accuracy(ypred,test$y)
Precision(ypred,test$y)
Recall(ypred,test$y)
F1_Score(ypred,test$y)

##REGRESSION
library(MASS)
#Boston
data(Boston)
dataset<-as.data.frame(Boston)
dataset
#splitting of data
sample<-sample(c(TRUE,FALSE),nrow(dataset),replace = TRUE,prob = c(0.7,0.5))
train<-dataset[sample,]
test<-dataset[!sample,]
#training
tree.boston<-rpart(medv~.,data = train,method = )
tree.heart<-rpart(y~sbp+tobacco+ldl+adiposity+factor(famhist)+typea+obesity+alcohol+age,data = train,method = 'class')
summary(tree.heart)
tree.heart
rpart.plot(tree.heart)

#confusion Matrix
ypred<-predict(tree.heart,test,type='class')
ypred
table(predict=ypred,truth=test$y)
#Metrics
ConfusionMatrix(ypred,test$y)
Accuracy(ypred,test$y)
Precision(ypred,test$y)
Recall(ypred,test$y)
F1_Score(ypred,test$y)

#pruning
plotcp(tree.heart)
tree.heart$cptable
index<-which.min(tree.heart$cptable[,"xerror"])
cpopt<-tree.heart$cptable[index,"CP"]
cpopt
opttree.heart<-prune(tree.heart,cp=cpopt)
rpart.plot(opttree.heart)

#confusion Matrix
ypred<-predict(opttree.heart,test,type='class')
ypred
table(predict=ypred,truth=test$y)
#Metrics
ConfusionMatrix(ypred,test$y)
Accuracy(ypred,test$y)
Precision(ypred,test$y)
Recall(ypred,test$y)
F1_Score(ypred,test$y)


#Gbm
install.packages("gbm")
library(gbm)
library(MASS)
library(MLmetrics)
data(Boston)
library(MASS)
#Boston
data(Boston)
dataset<-as.data.frame(Boston)
dataset
#splitting of data
sample<-sample(c(TRUE,FALSE),nrow(dataset),replace = TRUE,prob = c(0.7,0.5))
train<-dataset[sample,]
test<-dataset[!sample,]
#train model
boost.boston<-glm(medv~.,data=train,distribution="gaussian",n.trees=50,interaction.depth=4)
boost.boston
summary(boost.boston)

#partial importance
par(mfrow=c(1,2))
plot(boost.boston,i="rm")
plot(boost.boston,i="lstat")

ypred<-predict(boost.boston,test)
plot(ypred,test$medv)
mean((ypred-test$medv)^2)
#metrics
MAE(ypred,test$medv)
MSE(ypred,test$medv)
RMSE(ypred,test$medv)
MAPE(ypred,test$medv)


