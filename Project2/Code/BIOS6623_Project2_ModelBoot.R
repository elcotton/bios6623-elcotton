####Initial Modeling###
#10/22/18

#Library
library(boot)

#import the data
va <- read.csv("C:/Users/cottonel/Documents/BIOS6623_AdvancedData/Project_Two/cleanedData.csv")

#ASA as FACTOR???
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


boot.proportions<-matrix(NA, nrow = 10000, ncol = 44)

###FIX AND ONLY GET PREDICTION ON 39
###Build the model using everyone
###Get predicted values for everyone
###Subset to only the 39 month people
###MAKE ASA a categorical data!!!! 1-3 and 4-5
for(i in 1:1000){
  #sample new data
  boot.vec <- sample(nrow(vaSub), replace = T)
  boot.sp <- vaSub[boot.vec,]
  
  #run the logistic regression
  model <- glm(death30~asa + bmi + proced,family = binomial(link = "logit"),
               data = boot.sp)
  coeff <- model$coefficients
  
  #get the fitted values
  logit <- coeff[1] + vaSub$asa*coeff[2] + vaSub$bmi*coeff[3] +  vaSub$proced*coeff[4]
  boot.sp$proportions <- inv.logit(logit)
  
  #save aggregated values for each hospital in boot proportions
  boot.proportions[i,] <- aggregate(boot.sp$proportions,list(boot.sp$hospcode),mean)[,2]*100
  
  print(i)
}


