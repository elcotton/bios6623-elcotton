####Initial Modeling###
#10/22/18

#Library
library(boot)

#import the data
va <- read.csv("C:/Users/cottonel/Documents/BIOS6623_AdvancedData/Project_Two/cleanedData.csv")

#
model <- glm(death30~asa + bmi + proced,family = binomial(link = "logit"),
             data = va)

modelAlb <- glm(death30~asa + bmi + proced + albumin,family = binomial(link = "logit"),
                data = va)
summary(model)
summary(modelAlb)

coeff <- model$coefficients

#Subset to complete values
vaSub <- va[,c(2,3,4,5,8,10)]
vaSub <- vaSub[complete.cases(vaSub),]

logit <- coeff[1] + vaSub$asa*coeff[2] + vaSub$bmi*coeff[3] +  vaSub$proced*coeff[4]

vaSub$propPred <- inv.logit(logit)
ptest <- exp(logit)/(1+exp(logit))

aggregate(vaSub$propPred,list(vaSub$hospcode),mean)*100

#Another day problem
#figure out the bootstrapping code
#POOP!
boot(vaSub$propPred[vaSub$hospcode == 1], statistic = mean, R = 1000)
