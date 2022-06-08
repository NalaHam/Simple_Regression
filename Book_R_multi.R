#multiple linear regression allows you to model your response variable in terms 
#of more than one predictor so you can measure the joint effect of several variables

#make matrix
demo.data <- data.frame(y = c(1.55, 0.42, 1.29, 0.73, 0.76, -1.09, 1.41, -0.32),
                        x1 = c(1.13, -0.73, 0.12,0.52,-0.54,-1.15,0.20,-1.09), 
                        x2 = c(1,0,1,1,0,1,0,1))
demo.fit <- lm(y~x1+x2, data = demo.data) #to right of ~ specify the predictors and separate them with a +
coef(demo.fit)

#back to survey data
library(MASS)
survmult <- lm(Height~Wr.Hnd+Sex, data = survey)
summary(survmult)

survmult2 <- lm(Height~Wr.Hnd+Sex+Smoke, data = survey)
summary(survmult2)

#plot survmult

survcoefs <- coef(survmult)
survcoefs #gives intercept(height of female), hand span, increase associated with males

plot(survey$Height~survey$Wr.Hnd, 
     col = c("gray", "black")[as.numeric(survey$Sex)],
     pch = 16, xlab = "Writing handspan", ylab = "Height")
abline(a = survcoefs[1], b = survcoefs[2], col = "gray", lwd = 2) #female, includes intercept and hand span
abline(a = survcoefs[1]+survcoefs[3], b = survcoefs[2], col = "black", lwd=2) #male
legend("topleft", legend = levels(survey$Sex), col = c("gray", "black"), pch =16)

#confidence interval for survmult2
confint(survmult2) #if confidence interval includes 0 then it is non significant






