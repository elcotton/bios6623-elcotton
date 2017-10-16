####BIOS 6623 Advanced data analysis####
#Project 1
#9/23/17

#import the data
hiv <- read.csv("C:/Users/cottonel/Documents/BIOS6623_AdvancedData/Project_One/BIOS6623_Project1_Data.csv")

#Subset to the years of interest
hiv <- hiv[which(hiv$years == 0|hiv$year == 2),]

#Make our mini datasets
hivBaseline <- hiv[which(hiv$years == 0),]
hivYear2 <- hiv[which(hiv$years == 2),]

###Get the variables into the correct groupings based on Camille's email
#Use only baseline values
#White vs Non White
hivBaseline$RACE[which(hivBaseline$RACE == 1)] <- "White, Non-Hispanic"
hivBaseline$RACE[which(hivBaseline$RACE == "2"|hivBaseline$RACE == "3"|hivBaseline$RACE == "4"|
                         hivBaseline$RACE == "7"|hivBaseline$RACE == "8")] <- "Non White"

#Alcohol Use (poor balance--no missing)
hivBaseline$DKGRP[which(hivBaseline$DKGRP == 3)] <- "14 or more/week"
hivBaseline$DKGRP[which(hivBaseline$DKGRP == "0"|hivBaseline$DKGRP == "1"| 
                          hivBaseline$DKGRP == "2")] <- "13 or less/week"

#Income(34 missing---good balance)
hivBaseline$income[which(hivBaseline$income == 9)] <- NA
hivBaseline$income[which(hivBaseline$income == "1")] <- "<10,000"
hivBaseline$income[which(hivBaseline$income == "2"|hivBaseline$income == "3"|
                     hivBaseline$income == "4")] <- "10,000-39,999"
hivBaseline$income[which(hivBaseline$income == "5"|hivBaseline$income == "6"|
                     hivBaseline$income == "7")] <- ">40,000"

#Education(no missing---new grouping)
hivBaseline$EDUCBAS[which(hivBaseline$EDUCBAS == 1|hivBaseline$EDUCBAS == 2|
                            hivBaseline$EDUCBAS == 3)] <- "HS or less"
hivBaseline$EDUCBAS[which(hivBaseline$EDUCBAS == "4"|hivBaseline$EDUCBAS == "5"|
                            hivBaseline$EDUCBAS == "6"|hivBaseline$EDUCBAS == "7")] <- "1 year of college or more"

#Marijuana Use (no missing, good balance)
hivBaseline$HASHV[which(hivBaseline$HASHV == 1)] <- "No"
hivBaseline$HASHV[which(hivBaseline$HASHV == "2")] <- "Yes"

#Smoking (balanced, no missing)
hivBaseline$SMOKE[which(hivBaseline$SMOKE == 1|hivBaseline$SMOKE == 2)] <- "Never/Former Smoker"
hivBaseline$SMOKE[which(hivBaseline$SMOKE == "3")] <- "Current Smoker"

#Adherence---with Year 2
hivYear2$ADH[which(hivYear2$ADH == 1|hivYear2$ADH == 2)] <- "95% or more"
hivYear2$ADH[which(hivYear2$ADH == "3"|hivYear2$ADH == "4")] <- "94% or less"


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

#delete the impossible values of BMI
hiv$BMI[which(hiv$BMI > 400|hiv$BMI < 0)] <- NA

#Clean up VLoad
#Put it on the log scale
hiv$logVLoadBase <- log(hiv$VloadBase)
hiv$logVLoad2year <- log(hiv$VloadYear2)

#Calculate the difference variables
hiv$QOL_MentDiff <- hiv$QOL_MentYear2-hiv$QOL_MentBase
hiv$QOL_PhysDiff <- hiv$QOL_PhysYear2-hiv$QOL_PhysBase
hiv$CD4CountDiff <- hiv$CD4CountYear2-hiv$CD4CountBase
hiv$logVloadDiff <- hiv$logVLoad2year/hiv$logVLoadBase

#Delete individuals who don't have all four outcome measurements
hiv <- hiv[which(is.na(hiv$QOL_MentDiff) == F & is.na(hiv$QOL_PhysDiff) == F & 
                          is.na(hiv$CD4CountDiff) == F & is.na(hiv$logVloadDiff) == F),]

#make smaller datasets for table use
hivDrug <- hiv[which(hiv$HardDrugs == 1),]
hivNoDrug <- hiv[which(hiv$HardDrugs == 0),]

##Make Table One---Demographics
tableOne <- matrix(NA, nrow = 21, ncol = 4)
colnames(tableOne) <- c("Variable", "All Patients", "Drug Users", "Not Drug Users")
tableOne[,1] <- c("N","Age", "BMI", "Race", "White","Non-White", "Marijuana Use", "Income", "<10000", "10000-39999",
                  ">40000", "Smoker", "Alcohol consumption", "13 or less/week", "14 or more/week", "Education", 
                  "HS or less", "1 year of college or more","Adherence", "94% or less", "95% or more")

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

tableOne[17:18,2] <- paste(round(prop.table(table(hiv$Education))[c(2,1)]*100,2), "(",table(hiv$Education)[c(2,1)],")")
tableOne[17:18,3] <- paste(round(prop.table(table(hivDrug$Education))[c(2,1)]*100,2), "(",table(hivDrug$Education)[c(2,1)],")")
tableOne[17:18,4] <- paste(round(prop.table(table(hivNoDrug$Education))[c(2,1)]*100,2), "(",table(hivNoDrug$Education)[c(2,1)],")")

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

tableTwo[1:8,2] <- as.character(lapply(hiv[,c(2,3,22,9,17,18,23,19)], contFunc))
tableTwo[1:8,3] <- as.character(lapply(hivDrug[,c(2,3,22,9,17,18,23,19)], contFunc))
tableTwo[1:8,4] <- as.character(lapply(hivNoDrug[,c(2,3,22,9,17,18,23,19)], contFunc))


#Table with the differences
tableThree <- matrix(NA, nrow = 12, ncol = 4)
colnames(tableThree) <- c("Variable", "All Patients", "Drug Users", "Not Drug Users")

tableThree[,1] <- c("SF-26 Mental QOL Score (Baseline)", "SF 26 Physical QOL Score(Baseline)",
                    "Viral Load (Baseline)", "CD4+ T cell count (Baseline)",
                    "SF-26 Mental QOL Score (2 year)", "SF 26 Physical QOL Score(2 year)",
                    "Viral Load (2 year)", "CD4+ T cell count (2 year)", "Mental Diff",
                    "Phys Diff", "Viral Load Diff", "CD4+ Diff")

tableThree[1:12,2] <- as.character(lapply(hiv[,c(2,3,22,9,17,18,23,19,24,25,27,26)], contFunc))
tableThree[1:12,3] <- as.character(lapply(hivDrug[,c(2,3,22,9,17,18,23,19,24,25,27,26)], contFunc))
tableThree[1:12,4] <- as.character(lapply(hivNoDrug[,c(2,3,22,9,17,18,23,19,24,25,27,26)], contFunc))

#Make graphs
library(ggplot2)

hiv$HardDrugs[hiv$HardDrugs == 1] <- "Hard drug users"
hiv$HardDrugs[hiv$HardDrugs == 0] <- "Not hard drug users"

phys <- ggplot(hiv, aes(x = HardDrugs, y = QOL_PhysDiff)) +
  geom_boxplot(alpha = 0.7, fill = "olivedrab3") + 
  scale_y_continuous(name = "Difference in Physical QOL Score (Year 2-Baseline)") +
  scale_x_discrete(name = "Hard Drug Use") +
  ggtitle("Graph 2:\n Change in Physical Score QOL by Hard Drug Use") +
  theme_bw() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        axis.title = element_text(face = "bold"),
        legend.title = element_blank())

ment <- ggplot(hiv, aes(x = HardDrugs, y = QOL_MentDiff)) +
  geom_boxplot(alpha = 0.7, fill = "darkgoldenrod1") + 
  scale_y_continuous(name = "Difference in Mental QOL Score (Year 2-Baseline)") +
  scale_x_discrete(name = "Hard Drug Use") +
  ggtitle("Graph 1:\n Change in Mental QOL Score by Hard Drug Use") +
  theme_bw() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        axis.title = element_text(face = "bold"),
        legend.title = element_blank())

cd4 <- ggplot(hiv, aes(x = HardDrugs, y = CD4CountDiff)) +
  geom_boxplot(alpha = 0.7, fill = "darkslategray3") + 
  scale_y_continuous(name = "Difference in CD4+ Count (Year 2-Baseline)") +
  scale_x_discrete(name = "Hard Drug Use") +
  ggtitle("Graph 4:\n Change in CD4+ Count by Hard Drug Use") +
  theme_bw() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        axis.title = element_text(face = "bold"),
        legend.title = element_blank())

vload <- ggplot(hiv, aes(x = HardDrugs, y = logVloadDiff)) +
  geom_boxplot(alpha = 0.7, fill = "palevioletred3") + 
  scale_y_continuous(name = "Difference in log(Viral Load) (Year 2-Baseline)") +
  scale_x_discrete(name = "Hard Drug Use") +
  ggtitle("Graph 3:\n Change in log(Viral Load) by Hard Drug Use") +
  theme_bw() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        axis.title = element_text(face = "bold"),
        legend.title = element_blank())

library(cowplot)
plot_grid(ment,phys,vload,cd4)

hiv$HardDrugs[hiv$HardDrugs == "Hard drug users"] <- 1
hiv$HardDrugs[hiv$HardDrugs == "Not hard drug users"] <- 0


#save tables
write.csv(tableOne, "C:/Repositories/bios6623-elcotton/Project1/Reports/tableOne.csv")
write.csv(tableTwo, "C:/Repositories/bios6623-elcotton/Project1/Reports/tableTwo.csv")
write.csv(tableThree, "C:/Repositories/bios6623-elcotton/Project1/Reports/tableThree.csv")

#Save data
write.csv(hiv, "C:/Users/cottonel/Documents/BIOS6623_AdvancedData/Project_One/project1_CleanedDataR.csv")

#Change to indicator variables
#smoking
hiv$SmokerInd[hiv$Smoker == "Current Smoker"] <- 1
hiv$SmokerInd[hiv$Smoker == "Never/Former Smoker"] <- 0

#drinks
hiv$DrinksInd[hiv$Drinks == "14 or more/week"] <- 1
hiv$DrinksInd[hiv$Drinks == "13 or less/week"] <- 0

#race
hiv$RaceInd[hiv$Race == "White, Non-Hispanic"] <- 1
hiv$RaceInd[hiv$Race == "Non White"] <- 0

#education
hiv$EducationInd[hiv$Education == "1 year of college or more"] <- 1
hiv$EducationInd[hiv$Education == "HS or less"] <- 0

#adherence
hiv$AdherenceInd[hiv$Adherence == "95% or more"] <- 1
hiv$AdherenceInd[hiv$Adherence == "94% or less"] <- 0

#income
hiv$IncomeMedInd[hiv$Income == "10,000-39,999"] <- 1
hiv$IncomeMedInd[hiv$Income == "<10,000"| hiv$Income == ">40,000"] <- 0

hiv$IncomeHighInd[hiv$Income == ">40,000"] <- 1
hiv$IncomeHighInd[hiv$Income == "<10,000"| hiv$Income == "10,000-39,999"] <- 0

#hashv
hiv$HashVInd[hiv$HashV == "Yes"] <- 1
hiv$HashVInd[hiv$HashV == "No"] <- 0

#Save the cleaned data
write.csv(hiv, "C:/Users/cottonel/Documents/BIOS6623_AdvancedData/Project_One/project1_CleanedDataSAS.csv")





