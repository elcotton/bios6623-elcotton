####Initial Modeling###
#10/22/18

#Library
library(boot)
library(ggplot2)

#import the data
va <- read.csv("C:/Users/cottonel/Documents/BIOS6623_AdvancedData/Project_Two/cleanedData.csv")


###Run the logistic model
#without albumin
model <- glm(death30~asa + bmiCalc + proced,family = binomial(link = "logit"),
             data = va)

#with albumin
#I did not include ASA because everyone was 4 or 5
modelAlb <- glm(death30~bmiCalc + proced + albumin,family = binomial(link = "logit"),
                data = va)
summary(model)
summary(modelAlb)

coeff <- model$coefficients

#Subset to complete cases with only the variables we care about
vaSub <- va[,c(2,3,4,5,10,11)]
vaSub <- vaSub[complete.cases(vaSub),]

#Find the predicted/expected proportions for every person with complete data
logit <- coeff[1] + vaSub$asa*coeff[2] + vaSub$bmiCalc*coeff[3] +  vaSub$proced*coeff[4]

vaSub$propPred <- inv.logit(logit)

#get only the 39 period people with complete cases
va39 <- vaSub[vaSub$sixmonth == 39,]

#the predicted death rate for each hospital based on the last six months
expProp <- aggregate(va39$propPred,list(va39$hospcode),mean)*100


####Below is the code for bootstrapping
###It was ran once and the results were saved as proportions.csv
###Those results are uploaded and used for the rest of the code
###since the bootstrapping takes a long time to run
# ###Do the bootstrapping
# #create empty vector to fill with proportions
# boot.proportions<-matrix(NA, nrow = 10000, ncol = 43)
# 
# #run the bootstrap
# #look at ratio of observed/exp
# for(i in 1:10000){
#   set.seed(i)
#   #sample new data
#   boot.vec <- sample(nrow(vaSub), replace = T)
#   boot.sp <- vaSub[boot.vec,]
#   
#   #run the logistic regression
#   model <- glm(death30~asa + bmiCalc + proced,family = binomial(link = "logit"),
#                data = boot.sp)
#   coeff <- model$coefficients
#   
#   #get the fitted values
#   logit <- coeff[1] + vaSub$asa*coeff[2] + vaSub$bmiCalc*coeff[3] +  vaSub$proced*coeff[4]
#   vaSub$proportions <- inv.logit(logit)
#   
#   #save aggregated values for each hospital in boot proportions
#   #Ponder if this hospital code is correct
#   boot.sp39 <- vaSub[vaSub$sixmonth == 39,]
#   boot.proportions[i,] <- aggregate(boot.sp39$proportions,list(boot.sp39$hospcode),mean)[,2]*100
#   
#   print(i)
# }
# 
# #Save the bootstrap so I don't have to run it again
# write.csv(boot.proportions,"C:/Repositories/bios6623-elcotton/Project2/Reports/proportions.csv")

###Get results from the bootstrap
#upload the bootstrap
boot.proportions <- read.csv("C:/Repositories/bios6623-elcotton/Project2/Reports/proportions.csv")
boot.proportions$X <- NULL

#Get the 2.5% and 97.5% percentiles
quantProp <- matrix(NA, nrow = 43, ncol = 1)
for(i in 1:43){
  quantProp[i] <- paste("(", round(quantile(boot.proportions[,i], prob = c(0.025,0.975))[1],2), ",",
                        round(quantile(boot.proportions[,i], prob = c(0.025,0.975))[2],2), ")", sep = "") 
}

#Save these percentiles, will be add them to table 2
expCI <- cbind(expProp, quantProp)


###Make table two
#actual and expected death rates, CI, observed/expected, way above or below expected
#Find observed/expected

#Upload tableTwo made at the end of the cleaning/table code
tableTwo <- read.csv("C:/Repositories/bios6623-elcotton/Project2/Reports/tableTwo.csv")
tableTwo$X <- NULL

#Add the expected death rate found above
tableTwo$ExpectedDR <- round(c(expProp$x[1:29], NA, expProp$x[30:43]),2)

#add the CI
tableTwo$CI <- c(quantProp[1:29], NA, quantProp[30:43])

#add the observed/expected ratio
tableTwo$ObsoverExp <- round(tableTwo$Death.Rate/tableTwo$ExpectedDR,2)

#add a column that says if that ratio is above 1.2 or below 0.8
tableTwo$SuperDifferent <- NULL
tableTwo$SuperDifferent[tableTwo$ObsoverExp >= 1.3] <- "High"
tableTwo$SuperDifferent[tableTwo$ObsoverExp < 1.3 & tableTwo$ObsoverExp > 0.7] <- "Very close"
tableTwo$SuperDifferent[tableTwo$ObsoverExp <= 0.7] <- "Low"
tableTwo$SuperDifferent[is.na(tableTwo$ObsoverExp)] <- NA

#save final table
write.csv(tableTwo, "C:/Repositories/bios6623-elcotton/Project2/Reports/tableTwoPlus.csv")


###plot the Obsoverexp by hospital
tableTwo$Hospital <- as.factor(tableTwo$Hospital)
ggplot(tableTwo, aes(x = Hospital, y = ObsoverExp)) + 
  geom_point(size = 3, color = "dodgerblue")+
  theme_bw() +
  scale_x_discrete(name = "Hospital") +
  scale_y_continuous(name = "Observed/Predicted Mortality Rate Ratio") +
  labs(title = "Graph 1: Observed/Predicted Mortality Rates by Hospital")+
  theme(text = element_text(size=12)) +
  theme(plot.title = element_text(size = rel(2), hjust = 0.5))+ 
  theme(axis.title = element_text(face = "bold"))+
  geom_hline(yintercept = 1.2, col = "red") +
  geom_hline(yintercept = 0.8, col = "blue")
