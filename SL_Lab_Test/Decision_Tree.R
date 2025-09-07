install.packages("rpart")
install.packages("rpart.plot")
library(catdata)
library(rpart)
library(rpart.plot)
library(MLmetrics)
#classification
data("heart")
Heart_data<-as.data.frame(heart)
Heart_data
 #splitting of data
sample<-sample(c(TRUE,FALSE),nrow(Heart_data),replace = TRUE,prob = c(0.7,0.5))
train<-Heart_data[sample,]
test<-Heart_data[!sample,]
#training

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

