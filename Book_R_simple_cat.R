#binary categorical predictors
#Beta0 provides a baseline or reference when X = 0 and Beta1 represents the 
#additive effect on the mean response if X = 1
library(MASS)

class(survey$Sex)
table(survey$Sex)

plot(survey$Height~survey$Sex) #gives two boxplots
points(survey$Height~as.numeric(survey$Sex), cex = 0.5) #gives the raw heights in the graph

means.sex <- tapply(survey$Height, INDEX = survey$Sex, FUN= mean, na.rm = TRUE)
means.sex
points(1:2, means.sex, pch = 4, cex = 3)

survfit2 <- lm(Height~Sex, data = survey)
summary(survfit2) #the Beta0 or Intercept is the estimate of the mean height of female.


# K>2 categorical predictors
#dummy coding is used to create several binary variables from a categorical varible 
#use k-1 dummy variables where one acts as a reference which is incorporated into the intercept

#see the effect of height on the different levels of smoking(heavy,never,occasional,regular)
is.factor(survey$Smoke) #true, this is a factor
table(survey$Smoke) #gives the number of students in each category
levels(survey$Smoke) #gives the levels attributed of any R factor via 'levels'
                    #R makes the leves of a factor appear in alphabetical order 
                    #by default, and will set the first one as the reference level

boxplot(Height~Smoke, data = survey)
points(1:4, tapply(survey$Height, survey$Smoke, mean, na.rm = TRUE), pch = 4)

survfit3 <- lm(Height~Smoke, data = survey)
summary(survfit3) #used heavy as the intercept
                  #p values are large, meaning that having a smoking freq different
                  #from the reference affects the mean students heights

#change reference level
SmokeReordered <- relevel(survey$Smoke, ref = "Never") #choose which level is the first
levels(SmokeReordered)
survfit4 <- lm(Height~SmokeReordered, data = survey)
summary(survfit4)











