########################################
#       BIOS 6623: Project Zero        #
########################################

#import data
teeth <- read.csv("C:/Users/cottonel/Documents/BIOS6623_AdvancedData/Project_Zero/ProjectZeroData.csv")

#Check out the data
table(teeth$trtgroup)
table(teeth$sex)
table(teeth$race)
hist(teeth$age)
table(teeth$smoker)
hist(teeth$sites)
hist(teeth$attachbase)
hist(teeth$attach1year)
hist(teeth$pdbase)
hist(teeth$pd1year)

#Make Table One
group1 <- teeth[teeth$trtgroup == 1,]
group2 <- teeth[teeth$trtgroup == 2,]
group3 <- teeth[teeth$trtgroup == 3,]
group4 <- teeth[teeth$trtgroup == 4,]
group5 <- teeth[teeth$trtgroup == 5,]

tableone <- matrix(NA,nrow = 15, ncol = 6)
colnames(tableone) <- c("Variable", "Placebo", "No Treatment", "Low", "Medium", "High")
tableone[,1] <- c("Number","Sex", "Male", "Female", "Race", "Native American", "African American",
                  "Asian", "White", "Smoker", "Sites", "Attach (base)", "Attach (1 year)",
                  "PD (base)", "PD (1 year)")
tableone[1,2:6] <- table(teeth$trtgroup)
tableone[3:4,2] <- round(prop.table(table(group1$sex))*100,2)
tableone[3:4,3] <- round(prop.table(table(group2$sex))*100,2)
tableone[3:4,4] <- round(prop.table(table(group3$sex))*100,2)
tableone[3:4,5] <- round(prop.table(table(group4$sex))*100,2)
tableone[3:4,6] <- round(prop.table(table(group5$sex))*100,2)

tableone[7:9,2] <- round(prop.table(table(group1$race))*100,2)
tableone[6:9,3] <- round(prop.table(table(group2$race))*100,2)
tableone[c(6,7,9),4] <- round(prop.table(table(group3$race))*100,2)
tableone[8:9,5] <- round(prop.table(table(group4$race))*100,2)
tableone[c(6,7,9),6] <- round(prop.table(table(group5$race))*100,2)

tableone[10,2] <- round(prop.table(table(group1$smoker))*100,2)[2]
tableone[10,3] <- round(prop.table(table(group2$smoker))*100,2)[2]
tableone[10,4] <- round(prop.table(table(group3$smoker))*100,2)[2]
tableone[10,5] <- round(prop.table(table(group4$smoker))*100,2)[2]
tableone[10,6] <- round(prop.table(table(group5$smoker))*100,2)[2]

for(i in 7:11){
  tableone[i+4,2] <- paste(round(mean(group1[,i], na.rm = TRUE),2), "±", round(sd(group1[,i], na.rm = TRUE),2))
  tableone[i+4,3] <- paste(round(mean(group2[,i], na.rm = TRUE),2), "±", round(sd(group2[,i], na.rm = TRUE),2))
  tableone[i+4,4] <- paste(round(mean(group3[,i], na.rm = TRUE),2), "±", round(sd(group3[,i], na.rm = TRUE),2))
  tableone[i+4,5] <- paste(round(mean(group4[,i], na.rm = TRUE),2), "±", round(sd(group4[,i], na.rm = TRUE),2))
  tableone[i+4,6] <- paste(round(mean(group5[,i], na.rm = TRUE),2), "±", round(sd(group5[,i], na.rm = TRUE),2))
}

















