
install.packages("broom")
install.packages("ggpubr")
library(ggplot2)
library(dplyr)
library(broom)
library(ggpubr)

#simple regression
summary(income.data)
hist(income.data$happiness) #check to see if data is normal dist
plot (happiness ~ income, data = income.data) #check to see if data is linear

income.happiness.lm <- lm(happiness ~ income, data = income.data) #makes the linear model
summary(income.happiness.lm) #summary of model, small p value says that model fits the data well. 
#there is a significiant positive relationship btw income and happiness

#test for homoscedasticity
par(mfrow= c(2,2))
plot(income.happiness.lm) #residuals red line is horz and around 0, means that there are no outliers or biases in ther data that would make a linear regression invalid
par(mfrow = c(1,1))
#normal Q-Qplot is 1:1 line => residuals from a perfect model. This plus the residuals meets the assumption of homoscedasticity

#visualize
income.graph <- ggplot(income.data, aes(x=income, y = happiness)) + geom_point()
income.graph <- income.graph + geom_smooth (method = "lm", col = "black") #add linear regression line
income.graph <- income.graph + stat_regline_equation (label.x = 3, label.y = 7) #add equation of line

income.graph <- income.graph + theme_bw() +
  labs(title = "Reported happiness as a function of income",
       x = "Income (x$10,000)",
       y = "Happiness score (0 to 10)")
income.graph




#multiple regression
summary(heart.data)
cor(heart.data$biking, heart.data$smoking) #see if varibles are highly correlated, ans= 0.015 (not highly correlated)
hist(heart.data$heart.disease) #check to see if data is normal dist
plot(heart.disease ~ biking, data=heart.data) #linear
plot(heart.disease ~ smoking, data = heart.data) #kinda linear?

#fit a linear model with heart disease as the dependent variable and biking and smoking as indep.
heart.disease.lm <- lm(heart.disease ~ biking +smoking, data = heart.data)
summary(heart.disease.lm)

#test for homoscedasticity
par(mfrow = c(2,2))
plot(heart.disease.lm)
#the residuals show no bias, so the model fits the assumption of homoscedasticity

#visualize is complicated and instead, usually a table is used

