install.packages("ISLR")
library(ISLR)
install.packages("dplyr")
library(dplyr)
library(ggplot2)
library(splines)

head(Wage)
dim(Wage)

agelims=Wage %>%
  select(age) %>%
  range
head(Wage)

#
age_grid=seq(from=min(agelims), to=max(agelims))
#
fit=lm(wage~bs(age,knots = c(25,40,60)),data=Wage)
summary(fit)
#
#
#
pred=predict(fit,newdata=list(age=age_grid),se=TRUE)

#
se_bands=with(pred, cbind("upper"=fit+2*se.fit,
                          "lower"=fit-2*se.fit))

#plot the spline and error bands
ggplot() +
  geom_point(data=Wage,aes(x=age,y=age))+
  geom_point(aes(x=age_grid,y=pred$fit),)+
  geom_ribbon(aes(x=age_grid,
                  ymin=se_bands[,"lower"],
                  ymax=se_bands[,"upper"]),
              alpha=0.3)+
  xlim(agelims)


#Fit 2 smoothing splines
fit_smooth = with(Wage, smooth.spline(age, wage, df=16))
fit_smooth_cv = with(Wage, smooth.spline(age, wage, cv=TRUE))

#Plot the smoothing splines
ggplot() +
  geom_point(data = Wage, aes(x = age, y = wage)) +
  geom_line(aes(x = fit_smooth$x, y = fit_smooth$y,
                color = "16 degrees of freedom")) +
  geom_line(aes(x = fit_smooth_cv$x, y = fit_smooth_cv$y,
                color = "6.8 effective degrees of freedom")) +
  theme(legend.position = 'bottom')+
  labs(title = "Smoothing Splines", color = "")


install.packages("gam")
library(gam)
gam1 = gam(wage ~ s(year, 4) + s(age, 5) + education, data = Wage)
summary(gam1)

par(mfrow = c(1,3))
plot(gam1, se = TRUE, col = "blue")
