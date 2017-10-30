####ADA Project 2#####
#10/10/17
#Initial Data cleaning


#load libraries
library(dplyr)
library(haven)

#import the data
va <- read_sas("~/BIOS6623_AdvancedData/Project_Two/vadata1.sas7bdat")


###Clean the data
#Delete the proced = 2 people
va <- va[grep(2, va$proced, invert = TRUE),]

#fix the funny Weight values
va[va$hospcode >=1 & va$hospcode <=16 & va$sixmonth == 39,5] <- va[va$hospcode >=1 & va$hospcode <=16 & va$sixmonth == 39,5]*2.2

#Calculate the BMI by hand
va$bmiCalc <- va$weight/(va$height^2)*703

#Switch ASA to 0 (if 1,2,3) or 1 (if 4,5)
va$asa[va$asa == 1|va$asa == 2|va$asa == 3] <- 0
va$asa[va$asa == 4|va$asa == 5] <- 1

###Investigate the missing data
#Subset to those who don't have albumin
vaAMiss <- va[is.na(va$albumin),]
vaANotMiss <- va[is.na(va$albumin) == F,]
#Missing at random! Based on ASA score

###Add a column that is for current data and old data
va$Current <- NA
va$Current[va$sixmonth == 39] <- 1
va$Current[va$sixmonth != 39] <- 0

#Export the cleaned datafile
write.csv(va, "C:/Users/cottonel/Documents/BIOS6623_AdvancedData/Project_Two/cleanedData.csv")

###Prepare the data for table making
#Change NA to missing for table making
va$proced[is.na(va$proced)] <- "missing"
va$asa[which(is.na(va$asa) == TRUE)] <- "missing"

###Subset data into both datasets
vaCurr <- va[va$Current == 1,]
vaNotCurr <- va[va$Current == 0,]

###Make tableOne, 1 column for old and one for new
tableOne <- matrix(NA, ncol = 4, nrow = 14)
colnames(tableOne) <- c("Variable","All Months", "Current Month", "Past Months")
tableOne[,1] <- c("N", "Procedure", "Valve Surgery", "CABG Surgery", "Missing",
                  "ASA", "0 (1,2,3)", "1 (4 or 5)", "Missing", "Weight", "Height",
                  "BMI", "Albumin", "30 day mortality")

contFunc <- function(x){
  return(paste(round(mean(x, na.rm = TRUE),2), "±", round(sd(x, na.rm = TRUE),2), "(Missing =", sum(is.na(x)), ")"))
}

tableOne[1,2] <- nrow(va)
tableOne[1,3] <- nrow(vaCurr)
tableOne[1,4] <- nrow(vaNotCurr)


tableOne[3:5,2] <- paste(round(prop.table(table(va$proced))[c(1,2,3)]*100,2), "(",table(va$proced)[c(1,2,3)],")")
tableOne[3:5,3] <- paste(round(prop.table(table(vaCurr$proced))[c(1,2,3)]*100,2), "(",table(vaCurr$proced)[c(1,2,3)],")")
tableOne[3:5,4] <- paste(round(prop.table(table(vaNotCurr$proced))[c(1,2,3)]*100,2), "(",table(vaNotCurr$proced)[c(1,2,3)],")")

tableOne[7:9,2] <- paste(round(prop.table(table(va$asa))[c(1,2,3)]*100,2), "(",table(va$asa)[c(1,2,3)],")")
tableOne[7:9,3] <- paste(round(prop.table(table(vaCurr$asa))[c(1,2,3)]*100,2), "(",table(vaCurr$asa)[c(1,2,3)],")")
tableOne[7:9,4] <- paste(round(prop.table(table(vaNotCurr$asa))[c(1,2,3)]*100,2), "(",table(vaNotCurr$asa)[c(1,2,3)],")")

tableOne[10,2] <- contFunc(va$weight)
tableOne[10,3] <- contFunc(vaCurr$weight)
tableOne[10,4] <- contFunc(vaNotCurr$weight)

tableOne[11,2] <- contFunc(va$height)
tableOne[11,3] <- contFunc(vaCurr$height)
tableOne[11,4] <- contFunc(vaNotCurr$height)

tableOne[12,2] <- contFunc(va$bmiCalc)
tableOne[12,3] <- contFunc(vaCurr$bmiCalc)
tableOne[12,4] <- contFunc(vaNotCurr$bmiCalc)

tableOne[13,2] <- contFunc(va$albumin)
tableOne[13,3] <- contFunc(vaCurr$albumin)
tableOne[13,4] <- contFunc(vaNotCurr$albumin)

tableOne[14,2] <- paste(round(prop.table(table(va$death30))[2]*100,2), "(",table(va$death30)[2],")")
tableOne[14,3] <- paste(round(prop.table(table(vaCurr$death30))[2]*100,2), "(",table(vaCurr$death30)[2],")")
tableOne[14,4] <- paste(round(prop.table(table(vaNotCurr$death30))[2]*100,2), "(",table(vaNotCurr$death30)[2],")")

#Save tableOne
write.csv(tableOne, "C:/Repositories/bios6623-elcotton/Project2/Reports/tableOne.csv")

###Make table 2, the deaths by the hospital
#Only for the last 6 months
tableTwo <- matrix(NA, nrow = 44, ncol = 4)
colnames(tableTwo) <- c("Hospital", "Number Died", "Number Seen", "Death Rate")
tableTwo[,1] <- seq(1,44, by =1)
tableTwo[,2] <- table(vaCurr$hospcode,vaCurr$death30)[,2]
tableTwo[,3] <- table(vaCurr$hospcode)
tableTwo[,4] <- round(table(vaCurr$hospcode,vaCurr$death30)[,2]/table(vaCurr$hospcode)*100, 2)

#Save tabletwo
write.csv(tableTwo, "C:/Repositories/bios6623-elcotton/Project2/Reports/tableTwo.csv")

###Should actual death rates be calculated for everyone in the last
#six months or just those with complete cases


