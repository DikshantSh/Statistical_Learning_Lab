install.packages("rpart")
install.packages("rpart.plot")
library(catdata)
library(rpart)
library(rpart.plot)
library(MLmetrics)
#classification
data <- read.csv("C:/Users/SHISHUPAL/Desktop/ISE/SEM 6/Statistical Learning/LAB/Dataset/Iris.csv", header = TRUE, sep = ",")
View(data)
head(data)
summary(data)
data <- data[, !names(data) %in% c("Id")]
head(data)
#splitting of data
sample<-sample(c(TRUE,FALSE),nrow(data),replace = TRUE,prob = c(0.7,0.5))
train<-data[sample,]
test<-data[!sample,]
#training

tree.data<-rpart(Species~SepalLengthCm +SepalWidthCm+PetalLengthCm+PetalWidthCm ,data = train,method = 'class')
summary(tree.data)
tree.data
rpart.plot(tree.data)

#confusion Matrix
ypred<-predict(tree.data,test,type='class')
table(predict=ypred,truth=test$Species)
#Metrics
ConfusionMatrix(ypred,test$Species)
Accuracy(ypred,test$Species)
Precision(ypred,test$Species)
Recall(ypred,test$Species)
F1_Score(ypred,test$Species)

#pruning
plotcp(tree.data)
tree.data$cptable
index<-which.min(tree.data$cptable[,"xerror"])
cpopt<-tree.data$cptable[index,"CP"]
cpopt
opttree.data<-prune(tree.data,cp=cpopt)
rpart.plot(opttree.data)

#confusion Matrix
ypred<-predict(opttree.data,test,type='class')
table(predict=ypred,truth=test$Species)

#Metrics
ConfusionMatrix(ypred,test$Species)
Accuracy(ypred,test$Species)
Precision(ypred,test$Species)
Recall(ypred,test$Species)
F1_Score(ypred,test$Species)

#Bagging
install.packages("ipred")
install.packages("tree")

library(ipred)
library(tree)


# Bagging with decision tree
#fit the bagged model 
X<-data[, !names(data) %in% c("Species")]
head(data)
head(train)
bag <- bagging( 
  formula = Species ~ ., 
  data =train, 
  nbagg = 50,    
  coob = TRUE, 
  control = rpart.control(minsplit = 2, cp = 0,  
                          min_depth=2) 
) 
bag
#confusion Matrix
ypred<-predict(bag,test,type='class')

table(predict=ypred,truth=test$Species)

#Metrics
ConfusionMatrix(ypred,test$Species)
Accuracy(ypred,test$Species)
Precision(ypred,test$Species)
Recall(ypred,test$Species)
F1_Score(ypred,test$Species)

#Random forest
install.packages("randomForest")
library(randomForest)

# Now we are Training the random forest model 
rf_model <- randomForest(Species ~ .,  
                         data = train,ntree = 50) 
rf_model

#confusion Matrix
ypred<-predict(rf_model,test,type='class')
table(predict=ypred,truth=test$Species)

#Metrics
ConfusionMatrix(ypred,test$Species)
Accuracy(ypred,test$Species)
Precision(ypred,test$Species)
Recall(ypred,test$Species)
F1_Score(ypred,test$Species)





