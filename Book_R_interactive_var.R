library(faraway)
#model total cholesterol(continuous) by age(continuous) and body frame(categorical k=3)

dia.fit <- lm(chol~age+frame+age:frame, data = diabetes) #use : to specify an interactive effect of two predictors
                                        #a short hand way of doing this is just chol~age*frame, this says include 
                                        #an intercept, all main effects, and the interaction

summary(dia.fit) #if atleast one of the coefficents is significant, the entire effect should be deemed significant
                #in this case the small frame is significant and the others are not, but frame as a whole is significant so we include everything

dia.coef <- coef(dia.fit)
dia.coef

#plot each of the frames and the output
dia.small <- c(dia.coef[1], dia.coef[2])
dia.small
dia.medium <- c(dia.coef[1]+dia.coef[3], dia.coef[2]+dia.coef[5])
dia.large <- c(dia.coef[1]+dia.coef[4], dia.coef[2]+dia.coef[6])
cols <- c("black", "darkgray", "lightgray")

plot(diabetes$chol~diabetes$age, col = cols[diabetes$frame], 
     cex = 0.5, xlab = "age", ylab = "cholesterol")
abline(coef = dia.small, lwd = 2)
abline(coef = dia.medium, lwd = 2, col = "darkgray")
abline(coef = dia.large, lwd = 2, col = "lightgray")
legend("topright", legend = c("small frame", "medium frame", "large frame"),
       lty = 1, lwd = 2, col = cols)




