
# Data
# https://archive.ics.uci.edu/ml/datasets/Seoul+Bike+Sharing+Demand 

Bike <- readr:: read_csv('/Users/reygovea/Documents/Fall 2020/STA5166/Final Project/Bike Rentals /SeoulBikeData.csv')

# Packages
library(dplyr)
library(gapminder)
library(leaps)
library(glmnet)
library(ggplot2)
library(MASS)
library(usdm)
library(car)
library(corrplot)
library(texreg)
library(MPV)
library(AER)
library(DHARMa)
library(boot)
library(tidyverse)
library(broom)
library(glmmTMB)
library(pscl)


# Methedology

model <- lm(Rented_Bike_Count ~ Hour + Temperature + Humidity + Wind_Speed+Visibility + Dew_Point_Temp + Solar_Radiation + Rainfall + Snowfall, data = Bike)

stepwise <- step(model, scope = Rented_Bike_Count ~ Hour + Temperature + Humidity + Wind_Speed+Visibility + Dew_Point_Temp + Solar_Radiation + Rainfall + Snowfall, direction = "both", k=log(n))

step.model <- lm(Rented_Bike_Count ~ Hour + Temperature + Humidity + Solar_Radiation + Rainfall , data = Bike) 

step.model.vif <- vif(step.model)
step.model.vif

# Multiple Linear Regression

layout(matrix(c(1,2,3,4),2,2))
plot(step.model)

par(mfrow = c(1,1))
YJ <- boxCox(model, family="yjPower", plotit = T)

trans.step.model1 <- lm((Rented_Bike_Count)^.4 ~ Hour + Temperature + Humidity + Solar_Radiation + Rainfall , data = Bike)

layout(matrix(c(1,2,3,4),2,2))
plot(trans.step.model1, main = 'lambda = .4')

# Poisson Model 

#looking at mean and variance of the dependent var. -- this is for assumption testing
#with the var being much greater than the mean we expect that there is over-dispersion of the model 
mean(Bike$Rented_Bike_Count, )
var(Bike$Rented_Bike_Count)

par(mfrow = c(1,1))
# Hist of dist for dependent var  
hist(Bike$Rented_Bike_Count, main = "Rented Bike Count Frequency", xlab= "Rented Bike Count")

poisson.model <- glm(Rented_Bike_Count ~ Hour + Temperature + Humidity + Solar_Radiation + Rainfall, data = Bike, family = poisson(link = "log"))
summary(poisson.model)

# Zero Inflated Negative Binomial Model

zeroNB.model <- zeroinfl(Rented_Bike_Count ~ Hour + Temperature + Humidity + Solar_Radiation + Rainfall, data = Bike, dist = "negbin")
summary(zeroNB.model)

# Model Comparison 

AIC(model)
AIC(poisson.model)
AIC(zeroNB.model)

#using the vuong model to compare the best model with a significant p-value indicating that the zero inflated negative binomial is the better model --- the significant p-value indicates that the zero inflated model is better. 
comparison <- vuong(poisson.model, zeroNB.model)







