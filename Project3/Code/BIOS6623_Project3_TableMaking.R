###################################################
# BIOS 6623: Project 3                            #
# Date: 11/11/17                                  #
# Purpose: Make Table1-Demo, Make Table2-Outcome  #
###################################################

#import the data
ment <- read.csv("C:/Users/cottonel/Documents/BIOS6623_AdvancedData/Project_Three/Project3Data_Cleaned.csv")

#Get only the first measurement on each person
ment <- ment[!duplicated(ment$id),]

#Make mini datasets
mentDem <- ment[ment$demind == 1,]
mentNoDem <- ment[ment$demind == 0,]

#Make tableOne: Demographics
tableOne <- matrix(NA, ncol = 4, nrow = 7)
colnames(tableOne) <- c("Variable", "All Patients", "Dementia Diagnosis", "No Dementia Diagnosis")
tableOne[,1] <- c("N", "Sex (n (%))", "Male", "Female", "SES (mean ± sd)", 
                  "Age (mean ± sd)", "Age at onset (mean ± sd)")

contFunc <- function(x){
  return(paste(round(mean(x, na.rm = TRUE),2), "±", round(sd(x, na.rm = TRUE),2), "(Missing =", sum(is.na(x)), ")"))
}

tableOne[1,2] <- nrow(ment)
tableOne[1,3] <- nrow(mentDem)
tableOne[1,4] <- nrow(mentNoDem)

tableOne[3:4,2] <- paste(round(prop.table(table(ment$gender))[c(1,2)]*100,2), "(",table(ment$gender)[c(1,2)],")")
tableOne[3:4,3] <- paste(round(prop.table(table(mentDem$gender))[c(1,2)]*100,2), "(",table(mentDem$gender)[c(1,2)],")")
tableOne[3:4,4] <- paste(round(prop.table(table(mentNoDem$gender))[c(1,2)]*100,2), "(",table(mentNoDem$gender)[c(1,2)],")")

tableOne[5,2] <- contFunc(ment$SES)
tableOne[5,3] <- contFunc(mentDem$SES)
tableOne[5,4] <- contFunc(mentNoDem$SES)

tableOne[6,2] <- contFunc(ment$age)
tableOne[6,3] <- contFunc(mentDem$age)
tableOne[6,4] <- contFunc(mentNoDem$age)


tableOne[7,2] <- contFunc(ment$ageonset)
tableOne[7,3] <- contFunc(mentDem$ageonset)
tableOne[7,4] <- contFunc(mentNoDem$ageonset)

write.csv(tableOne, "C:/Repositories/bios6623-elcotton/Project3/Reports/tableOne.csv")

#make TableTwo: Outcome variables
tableTwo <- matrix(NA, nrow = 5, ncol = 4)
colnames(tableTwo) <- c("Variable", "All Patients", "Dementia Diagnosis", "No Dementia Diagnosis")
tableTwo[,1] <- c("Baseline Values", "Block Design Test Score",
                  "Category Fluency for Animals Score", 
                  "Logical Memory I Story A Score",
                  "Logical Memory II Story A Score")

tableTwo[2,2] <- contFunc(ment$blockR)
tableTwo[2,3] <- contFunc(mentDem$blockR)
tableTwo[2,4] <- contFunc(mentNoDem$blockR)

tableTwo[3,2] <- contFunc(ment$animals)
tableTwo[3,3] <- contFunc(mentDem$animals)
tableTwo[3,4] <- contFunc(mentNoDem$animals)

tableTwo[4,2] <- contFunc(ment$logmemI)
tableTwo[4,3] <- contFunc(mentDem$logmemI)
tableTwo[4,4] <- contFunc(mentNoDem$logmemI)

tableTwo[5,2] <- contFunc(ment$logmemII)
tableTwo[5,3] <- contFunc(mentDem$logmemII)
tableTwo[5,4] <- contFunc(mentNoDem$logmemII)

write.csv(tableTwo, "C:/Repositories/bios6623-elcotton/Project3/Reports/tableTwo.csv")










