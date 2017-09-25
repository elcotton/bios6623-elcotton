####BIOS 6623 Advanced data analysis####
#Project 1
#9/23/17

#import the data
hiv <- read.csv("C:/Users/cottonel/Documents/BIOS6623_AdvancedData/Project_One/BIOS6623_Project1_Data.csv")

#Subset to the years of interest
hiv <- hiv[which(hiv$years == 0|hiv$year == 2),]

#What to include
#baseiine value of outcome, baseline age, baseline bmi 

hivBaseline <- hiv[which(hiv$years == 0),]
hivYear2 <- hiv[which(hiv$years == 2),]

#Change Race (good balance-no missing)
hivBaseline$RACE[which(hivBaseline$RACE == 1)] <- "White, Non-Hispanic"
hivBaseline$RACE[which(hivBaseline$RACE == "2"|hivBaseline$RACE == "3"|hivBaseline$RACE == "4"|
                         hivBaseline$RACE == "7"|hivBaseline$RACE == "8")] <- "Non White"

#Alcohol Use (poor balance--no missing)
hivBaseline$DKGRP[which(hivBaseline$DKGRP == 3| hivBaseline$DKGRP == 2)] <- "4 or more/week"
hivBaseline$DKGRP[which(hivBaseline$DKGRP == "0"|hivBaseline$DKGRP == "1")] <- "3 or less/week"

#Income(34 missing---good balance)
hivBaseline$income[which(hivBaseline$income == 9)] <- NA
hivBaseline$income[which(hivBaseline$income == "1")] <- "<10,000"
hivBaseline$income[which(hivBaseline$income == "2"|hivBaseline$income == "3"|
                     hivBaseline$income == "4")] <- "10,000-39,999"
hivBaseline$income[which(hivBaseline$income == "5"|hivBaseline$income == "6"|
                     hivBaseline$income == "7")] <- ">40,000"

#Education(no missing---new grouping)
hivBaseline$EDUCBAS[which(hivBaseline$EDUCBAS == 1|hivBaseline$EDUCBAS == 2|
                            hivBaseline$EDUCBAS == 3|hivBaseline$EDUCBAS == 4)] <- "1 year of college or less"
hivBaseline$EDUCBAS[which(hivBaseline$EDUCBAS == "5"|hivBaseline$EDUCBAS == "6"|
                            hivBaseline$EDUCBAS == "7")] <- "4 years of college or more"

#Marijuana Use (no missing, good balance)
hivBaseline$HASHV[which(hivBaseline$HASHV == 1)] <- "No"
hivBaseline$HASHV[which(hivBaseline$HASHV == "2")] <- "Yes"

#Smoking (balanced, no missing)
hivBaseline$SMOKE[which(hivBaseline$SMOKE == 1|hivBaseline$SMOKE == 2)] <- "Never/Former Smoker"
hivBaseline$SMOKE[which(hivBaseline$SMOKE == "3")] <- "Current Smoker"

#Adherence---with Year 2
hivYear2$ADH[which(hivYear2$ADH == 1)] <- "100%"
hivYear2$ADH[which(hivYear2$ADH == "2"|hivYear2$ADH == "3"|
                     hivYear2$ADH == "4")] <- "99% or less"


#Subset baseline to only have the variables we want
hivBaseline<- hivBaseline[,c(2,3,4,5,7,8,20,21,24,25,27,28,
                             29,30,33,34)]
colnames(hivBaseline) <- c("ID", "QOL_MentBase", "QOL_PhysBase", "HashV",
                           "Income", "BMI", "Smoker", "Drinks",
                           "CD4CountBase", "VloadBase", "Race", "Education",
                           "HIVpos", "Age", "Years", "HardDrugs")

#Subset year 2 to only have the variables we want
hivYear2 <- hivYear2[,c(2,3,4,24,25,26)]
colnames(hivYear2) <- c("ID", "QOL_MentYear2", "QOL_PhysYear2",
                       "CD4CountYear2", "VloadYear2", "Adherence")

#Merge baseline and year 2
#506 people total who had year 2 measurements
hiv <- merge(hivBaseline,hivYear2, by = "ID")

#For now---omit the crazy values
hiv$BMI[which(hiv$BMI > 400|hiv$BMI < 0)] <- NA

#Calculate the difference variables
hiv$QOL_MentDiff <- hiv$QOL_MentYear2-hiv$QOL_MentBase
hiv$QOL_PhysDiff <- hiv$QOL_PhysYear2-hiv$QOL_PhysBase
hiv$CD4CountDiff <- hiv$CD4CountYear2-hiv$CD4CountBase
hiv$VloadDiff <- hiv$VloadYear2-hiv$VloadBase

hist(hiv$CD4CountDiff)
hist(hiv$QOL_MentDiff)

summary(hiv$QOL_MentDiff)
summary(hiv$QOL_PhysDiff)
summary(hiv$CD4CountDiff)
summary(hiv$VloadDiff)

hivDrug <- hiv[which(hiv$HardDrugs == 1),]
hivNoDrug <- hiv[which(hiv$HardDrugs == 0),]

##Make Table One---Demographics
tableOne <- matrix(NA, nrow = 21, ncol = 4)
colnames(tableOne) <- c("Variable", "All Patients", "Drug Users", "Not Drug Users")
tableOne[,1] <- c("N","Age", "BMI", "Race", "White","Non-White", "Marijuana Use", "Income", "<10000", "10000-39999",
                  ">40000", "Smoker", "Alcohol consumption", "3 or less/week", "4 or more/week", "Education", 
                  "1 year college or less", "4 year college or more","Adherence", "100%", "<100%")

contFunc <- function(x){
  return(paste(round(mean(x, na.rm = TRUE),2), "±", round(sd(x, na.rm = TRUE),2)))
}

tableOne[1,2:4] <- nrow(hiv)
tableOne[1,3] <- nrow(hivDrug)
tableOne[1,4] <- nrow(hivNoDrug)

tableOne[2,2] <- contFunc(hiv$Age)
tableOne[2,3] <- contFunc(hivDrug$Age)
tableOne[2,4] <- contFunc(hivNoDrug$Age)

tableOne[3,2] <- contFunc(hiv$BMI)
tableOne[3,3] <- contFunc(hivDrug$BMI)
tableOne[3,4] <- contFunc(hivNoDrug$BMI)

tableOne[5,2] <- paste(round(prop.table(table(hiv$Race))[2]*100,2), "(",table(hiv$Race)[2],")")
tableOne[5,3] <- paste(round(prop.table(table(hivDrug$Race))[2]*100,2), "(",table(hivDrug$Race)[2],")")
tableOne[5,4] <- paste(round(prop.table(table(hivNoDrug$Race))[2]*100,2), "(",table(hivNoDrug$Race)[2],")")

tableOne[6,2] <- paste(round(prop.table(table(hiv$Race))[1]*100,2), "(",table(hiv$Race)[1],")")
tableOne[6,3] <- paste(round(prop.table(table(hivDrug$Race))[1]*100,2), "(",table(hivDrug$Race)[1],")")
tableOne[6,4] <- paste(round(prop.table(table(hivNoDrug$Race))[1]*100,2), "(",table(hivNoDrug$Race)[1],")")

tableOne[7,2] <- paste(round(prop.table(table(hiv$HashV))[2]*100,2), "(",table(hiv$HashV)[2],")")
tableOne[7,3] <- paste(round(prop.table(table(hivDrug$HashV))[2]*100,2), "(",table(hivDrug$HashV)[2],")")
tableOne[7,4] <- paste(round(prop.table(table(hivNoDrug$HashV))[2]*100,2), "(",table(hivNoDrug$HashV)[2],")")

tableOne[9:11,2] <- paste(round(prop.table(table(hiv$Income))[c(1,3,2)]*100,2), "(",table(hiv$Income)[c(1,3,2)],")")
tableOne[9:11,3] <- paste(round(prop.table(table(hivDrug$Income))[c(1,3,2)]*100,2), "(",table(hivDrug$Income)[c(1,3,2)],")")
tableOne[9:11,4] <- paste(round(prop.table(table(hivNoDrug$Income))[c(1,3,2)]*100,2), "(",table(hivNoDrug$Income)[c(1,3,2)],")")

tableOne[12,2] <- paste(round(prop.table(table(hiv$Smoker))[1]*100,2), "(",table(hiv$Smoker)[1],")")
tableOne[12,3] <- paste(round(prop.table(table(hivDrug$Smoker))[1]*100,2), "(",table(hivDrug$Smoker)[1],")")
tableOne[12,4] <- paste(round(prop.table(table(hivNoDrug$Smoker))[1]*100,2), "(",table(hivNoDrug$Smoker)[1],")")

tableOne[14:15,2] <- paste(round(prop.table(table(hiv$Drinks))*100,2), "(",table(hiv$Drinks),")")
tableOne[14:15,3] <- paste(round(prop.table(table(hivDrug$Drinks))*100,2), "(",table(hivDrug$Drinks),")")
tableOne[14:15,4] <- paste(round(prop.table(table(hivNoDrug$Drinks))*100,2), "(",table(hivNoDrug$Drinks),")")

tableOne[17:18,2] <- paste(round(prop.table(table(hiv$Education))*100,2), "(",table(hiv$Education),")")
tableOne[17:18,3] <- paste(round(prop.table(table(hivDrug$Education))*100,2), "(",table(hivDrug$Education),")")
tableOne[17:18,4] <- paste(round(prop.table(table(hivNoDrug$Education))*100,2), "(",table(hivNoDrug$Education),")")

tableOne[20:21,2] <- paste(round(prop.table(table(hiv$Adherence))*100,2), "(",table(hiv$Adherence),")")
tableOne[20:21,3] <- paste(round(prop.table(table(hivDrug$Adherence))*100,2), "(",table(hivDrug$Adherence),")")
tableOne[20:21,4] <- paste(round(prop.table(table(hivNoDrug$Adherence))*100,2), "(",table(hivNoDrug$Adherence),")")


##Make Table Two---Outcomes
tableTwo <- matrix(NA, nrow = 8, ncol = 4)
colnames(tableTwo) <- c("Variable", "All Patients", "Drug Users", "Not Drug Users")

tableTwo[,1] <- c("SF-26 Mental QOL Score (Baseline)", "SF 26 Physical QOL Score(Baseline)",
                  "Viral Load (Baseline)", "CD4+ T cell count (Baseline)",
                  "SF-26 Mental QOL Score (2 year)", "SF 26 Physical QOL Score(2 year)",
                  "Viral Load (2 year)", "CD4+ T cell count (2 year)")

tableTwo[1:8,2] <- as.character(lapply(hiv[,c(2,3,10,9,17,18,20,19)], contFunc))
tableTwo[1:8,3] <- as.character(lapply(hivDrug[,c(2,3,10,9,17,18,20,19)], contFunc))
tableTwo[1:8,4] <- as.character(lapply(hivNoDrug[,c(2,3,10,9,17,18,20,19)], contFunc))

tableTwo <- tableTwo[c(1,5,2,6,3,7,4,8),]

#Make a graph of CD4 count
library(ggplot2)

hiv$HardDrugs <- as.factor(hiv$HardDrugs)
ggplot(hiv, aes(x = HardDrugs, y = CD4CountDiff)) +
  geom_boxplot(alpha = 0.7, fill = "darkslategray3") + 
  scale_y_continuous(name = "Difference in CD4+ Count (Year 2-Baseline)") +
  scale_x_discrete(name = "Hard Drug Use") +
  ggtitle("Graph 1:\n Change in CD4+ Count by Hard Drug Use") +
  theme_bw() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        axis.title = element_text(face = "bold"),
        legend.title = element_blank())

#save tables
write.csv(tableOne, "C:/Repositories/bios6623-elcotton/Project1/Reports/tableOne.csv")
write.csv(tableTwo, "C:/Repositories/bios6623-elcotton/Project1/Reports/tableTwo.csv")
