LungCapData2<-read.table(file='C:/Users/diksh/OneDrive/Desktop/IIT KGP/Sem Sixth/SL Lab/LungCapData2.csv', sep = ',', header = T)
head(LungCapData2)
summary(LungCapData2)

#make a plot of Lungcap vs. Height
plot(Height, LungCap, main="Polynomial Regression", las=1)

attach(LungCapData2)

plot(Height, LungCap, main="Height vs LungCap", las=1)

plot(Height, LungCap, main="Height vs LungCap", las=2)

#now, let's fit a linear regression
model1<-lm(LungCap~Height)
summary(model1)

#See how attach function is basically used, on deatching the dataset





# and add the line to the plot...make it thic
abline(model1,lwd=3,col="red")

#Fitting a polynomial regression model with
model2<-lm(LungCap ~ Height + I(Height^2))
summary(model2)
lines(smooth.spline(Height, predict(model2)), col="blue", lwd=3)

#Different way to fit a polynomial regression model
#create Height^2, and then include this in model...it's the same!
HeightSquare<-Height^2
model2again<-lm(LungCap~Height+HeightSquare)
summary(model2again)

#or, use the "poly" command...it's the same!
model2againagain<-lm(LungCap~poly(Height,degree=2,raw=T))
summary(model2againagain)

#test if the model including Height^2 is signig better than one with
#using the partial F-test
anova(model1,model2)

#try fitting a model that includes Height^3 as well
model3<-lm(LungCap~Height+I(Height^2)+I(Height^3))
summary(model3)

#no, let's add this model to 
lines(smooth.spline(Height,predict(model3)), col="green",lwd=3)

#and let add a leged
legend(46,15,legend=c("model1 : linear","model2 : poly x^2", "model3: polyx^3"),
       col = c("red","blue","green"),lty=c(1,1,3), lwd=3, bty="n", cex=0.9)

#now lets test
anova(model2,model3)
