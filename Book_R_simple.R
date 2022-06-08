#example from the book of R
library(MASS)
survey
?survey

plot(survey$Height~survey$Wr.Hnd, xlab = "Writing handspan (cm)", ylab= "Height (cm)") #see the plot
        #The result looks linear with a positive slope

cor(survey$Wr.Hnd, survey$Height, use = "complete.obs") #0.6009909

#R will remove NA values when plotting and to find out how many observations were removed we do:
incomplete.obs <- which(is.na(survey$Height)|is.na(survey$Wr.Hnd)) #Find out how many observations are NA for Height and Wr.Hnd

length(incomplete.obs)#count how many observations

survfit <- lm(Height~Wr.Hnd, data = survey)

survfit #gives repeat of the code above with estimates if the intercept and the slope. 
        #Wr.Hnd is th slope, and this says that there is 3.117 cm increase 
        #in height for every 1 cm increas in handspan. 

abline (survfit, lwd = 2)

obsA <- c(survey$Wr.Hnd[197], survey$Height[197])
obsB <- c(survey$Wr.Hnd[154], survey$Height[154])

names(survfit) #the elements that make up a lm function

mycoef <- coef(survfit) #direct-access function to get coefficients. resid and fitted are others. 
                        #resid gives residuals and fitted gives fitted.values
beta0.hat <- mycoef[1]
beta1.hat <- mycoef[2]

#show the difference btw line and observations
segments(x0 = c(obsA[1], obsB[1]), y0 = beta0.hat + beta1.hat*c(obsA[1], obsB[1]), 
                x1 = c(obsA[1], obsB[1]), y1 = c(obsA[2], obsB[2]), lty = 2) #adds lines for the difference 
                                                                     #btw the line of fit and actual points

summary(survfit)
confint(survfit, level = 0.95) #should be 95% confident that the true value of Beta 1
                              # lies btw 2.55 and 3.69

#R-squared info is provided in the summary and they describe the proportion of the variation
#in the response that can be attributed to the predictor

#for simple reg: unadjusted or multiple R-squared is found by the square of the 
#estimated correlation coefficient
rho.xy <- cor(survey$Wr.Hnd, survey$Height, use = "complete.obs")
rho.xy^2 #0.3611901 # multiple R-squared value, this tells that 36.1% of the variation 
                    #in height can be attributed to handspan

names(summary(survfit)) #names that are used in the summary and can be used to just get one thing
summary(survfit)$sigma #gives the sigma value in the summary of survfit






