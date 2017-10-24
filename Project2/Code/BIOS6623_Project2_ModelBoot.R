####Initial Modeling###
#10/22/18

#Library
library(boot)

#import the data
va <- read.csv("C:/Users/cottonel/Documents/BIOS6623_AdvancedData/Project_Two/cleanedData.csv")


###Run the logistic model
#without albumin
model <- glm(death30~asa + bmiCalc + proced,family = binomial(link = "logit"),
             data = va)

#with albumin
modelAlb <- glm(death30~asa + bmiCalc + proced + albumin,family = binomial(link = "logit"),
                data = va)
summary(model)
summary(modelAlb)

coeff <- model$coefficients

#Subset to complete cases with only the variables we care about
vaSub <- va[,c(2,3,4,5,10,11)]
vaSub <- vaSub[complete.cases(vaSub),]

#Find the predicted proportions for every person
logit <- coeff[1] + vaSub$asa*coeff[2] + vaSub$bmiCalc*coeff[3] +  vaSub$proced*coeff[4]

vaSub$propPred <- inv.logit(logit)

#get only the 39 period people
va39 <- vaSub[vaSub$sixmonth == 39,]

#the predicted death rate for each hospital!
expProp <- aggregate(va39$propPred,list(va39$hospcode),mean)*100


###Do the bootstrapping
#create empty vector to fill with proportions
boot.proportions<-matrix(NA, nrow = 10000, ncol = 43)

for(i in 1:10000){
  #sample new data
  boot.vec <- sample(nrow(vaSub), replace = T)
  boot.sp <- vaSub[boot.vec,]
  
  #run the logistic regression
  model <- glm(death30~asa + bmiCalc + proced,family = binomial(link = "logit"),
               data = boot.sp)
  coeff <- model$coefficients
  
  #get the fitted values
  logit <- coeff[1] + vaSub$asa*coeff[2] + vaSub$bmiCalc*coeff[3] +  vaSub$proced*coeff[4]
  boot.sp$proportions <- inv.logit(logit)
  
  #save aggregated values for each hospital in boot proportions
  boot.sp39 <- boot.sp[boot.sp$sixmonth == 39,]
  boot.proportions[i,] <- aggregate(boot.sp39$proportions,list(boot.sp39$hospcode),mean)[,2]*100
  
  print(i)
}

#Save the bootstrap so I don't have to run it again
write.csv(boot.proportions,"C:/Repositories/bios6623-elcotton/Project2/Reports/proportions.csv")

boot.proportions <- read.csv("C:/Repositories/bios6623-elcotton/Project2/Reports/proportions.csv")
boot.proportions$X <- NULL

#Get the 2.5% and 97.5% percentiles
quantProp <- matrix(NA, nrow = 43, ncol = 1)
for(i in 1:43){
  quantProp[i] <- paste("(", round(quantile(boot.proportions[,i], prob = c(0.025,0.975))[1],2), ",",
                        round(quantile(boot.proportions[,i], prob = c(0.025,0.975))[2],2), ")", sep = "") 
}

#Save these percentiles and add them to table 2
expCI <- cbind(expProp, quantProp)
write.csv(expCI, "C:/Repositories/bios6623-elcotton/Project2/Reports/quantiles.csv")




