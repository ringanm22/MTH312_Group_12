#locfit
#install.packages("locfit")

#polynomial mean

library(locfit)
data(ethanol, package="locfit")
data = ethanol


fit <- locfit.raw(data$E , data$NOx , deg = 2 , ev = data$E)
y_hat = predict(fit)
df = data.frame(x = data$E, y = data$NOx, y_hat = y_hat)
library(ggplot2)
ggplot(df)+
  geom_point(aes(x =data$E, y = data$NOx))+
  geom_line(aes(x = data$E, y = y_hat))



fit1 <- locfit.raw(data$E , data$NOx , deg = 2 , deriv = 1, ev = data$E)
deriv_1 = predict(fit1)
df1 = data.frame(x = data$E, y = data$NOx, deriv_1 = deriv_1)
library(ggplot2)
ggplot(df)+
  geom_point(aes(x =data$E, y = data$NOx))+
  geom_line(aes(x = data$E, y = deriv_1))




fit2 <- locfit.raw(data$E , data$NOx , deg = 2 , ev = data$E , deriv = c(1,1))
deriv_2 = predict(fit2)
df2 = data.frame(x = data$E, y = data$NOx, deriv_2 = deriv_2)
library(ggplot2)
ggplot(df)+
  geom_point(aes(x =x, y = y))+
  geom_line(aes(x = x, y= deriv_2))


#install.packages("Metrics")
library(Metrics)
rmse = rmse(data$NOx , y_hat)

#######################################################################################################


#polynomial median

rm(list = ls())
#locfit
#install.packages("locfit")
library(locfit)
data(ethanol, package="locfit")
data = ethanol


fit <- locfit.robust(x = data$E , y = data$NOx)
y_hat = predict(fit , data$E)
df = data.frame(x = data$E, y = data$NOx, y_hat = y_hat)
library(ggplot2)
ggplot(df)+
  geom_point(aes(x =data$E, y = data$NOx))+
  geom_line(aes(x = data$E, y = y_hat))


fit1 <- locfit.raw(data$E , data$NOx , deg = 2 , deriv = 1)
deriv_1 = predict(fit1 , data$E)
df1 = data.frame(x = data$E, y = data$NOx, deriv_1 = deriv_1)
library(ggplot2)
ggplot(df)+
  geom_point(aes(x =data$E, y = data$NOx))+
  geom_line(aes(x = data$E, y = deriv_1))



fit2 <- locfit.raw(data$E , data$NOx , deg = 2 , deriv = c(1,1))
deriv_2 = predict(fit2, data$E)
df1 = data.frame(x = data$E, y = data$NOx, deriv_2 = deriv_2)
library(ggplot2)
ggplot(df)+
  geom_point(aes(x =data$E, y = data$NOx))+
  geom_line(aes(x = data$E, y = deriv_2))

rmse = rmse(data$NOx , y_hat)