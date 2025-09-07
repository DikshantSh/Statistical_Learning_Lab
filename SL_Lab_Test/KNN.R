library(ISLR)
install.packages("caTools")
library(caTools)
library(dplyr)
library(class)
library(MASS)
View(Default)
Default$student=ifelse(Default$student=="Yes",1,0)
head(Default)
set.seed(10000)
split<-sample.split(Default,SplitRatio=0.8)
train_cl<-subset(Default,split=="TRUE")
test_cl<-subset(Default,split=="FALSE")
#feature scaling

train_scale<-scale(train_cl[,2:4])
test_scale<-scale(test_cl[,2:4])

head(train_scale)
head(test_scale)
#if you want to remove default column from dataset then
train_scale<-scale(train_cl[-1])
test_scale<-scale(test_cl[-1])

head