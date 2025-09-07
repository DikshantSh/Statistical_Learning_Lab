# LungCapData2
LungCapData2 <- read.table("C:\\Users\\user\\Desktop\\StatLearning Lab Spring '24\\LungCapData2.csv", sep = ",", header = T)
LungCapData2 <- read.table("C:\\Users\\diksh\\Downloads\\LungCapData2.csv", sep = ",", header = T)
head(LungCapData2)
summary(LungCapData2)

# Make a plot of LungCap vs. Height
plot(Height, LungCap, main = 'Height vs. LungCap', las = 1) # It will raise an error!

attach(LungCapData2)

plot(Height, LungCap, main = 'Height vs. LungCap', las = 1)

plot(Height, LungCap, main = 'Height vs. LungCap', las = 2)

# See how attach function is basically used. On de-attaching the dataset we need to explicitly mention the dataset everytime

# Now, let's fit a linear regression
model1 <- lm(LungCap ~ Height)
summary(model1)

# Add the line to the plot, make it thick and red
abline(model1, lwd = 3, col = 'red')

# Fitting a polynomial regression model with degree 2
model2 <- lm(LungCap ~ Height + I(Height^2))
summary(model2)

lines(smooth.spline(Height, predict(model2)), col = 'blue', lwd = 3)

# Different way to fit a polynomial regression model
# create Height^2, and include this in model... it's the same!
HeightSquare <- Height^2
model2again <- lm(LungCap ~ Height + HeightSquare)
summary(model2again)

# or use the poly command... it's the same!
model2returns <- lm(LungCap ~ poly(Height, degree = 2, raw = T))
summary(model2returns)

# Test if the model including Height^2 is significantly better than one with just Height
# using the partial F-test
anova(model1, model2)

# Try fitting a model that includes Height^3 as well
model3 <- lm(LungCap ~ poly(Height, degree = 3, raw = T))
summary(model3)

# Now, let's add this model to the plot, using a thick dashed green line
lines(smooth.spline(Height, predict(model3)), col = 'green', lwd = 3, lty = 3)

# and, let's add a legend to clarify the lines
legend(46, 15, legend = c("model1: linear", "model2: poly x^2", "model3: poly x^3"),
       col = c('red','blue','green'),
       lty = c(1,1,3), lwd = 3, bty = 'n', cex = 0.9)

# Let's test if the model with Height^3 is significantly better than one without it
anova(model2, model3)
