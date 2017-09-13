########################################
#       BIOS 6623: Project Zero        #
########################################

#load packages
library(ggplot2)

#import data
teeth <- read.csv("C:/Users/cottonel/Documents/BIOS6623_AdvancedData/Project_Zero/ProjectZeroData.csv")

#clean up the values
teeth$attach1year[teeth$attach1year == "."] <- NA
teeth$pd1year[teeth$pd1year == "."] <- NA
teeth$pd1year <- as.numeric(as.character(teeth$pd1year))
teeth$attach1year <- as.numeric(as.character(teeth$attach1year))

####Make Table One####
#Subset to only include the non-missing individuals
teethSmall <- teeth[is.na(teeth$attach1year) == F |is.na(teeth$pd1year) == F,]

group1 <- teethSmall[teethSmall$trtgroup == 1,]
group2 <- teethSmall[teethSmall$trtgroup == 2,]
group3 <- teethSmall[teethSmall$trtgroup == 3,]
group4 <- teethSmall[teethSmall$trtgroup == 4,]
group5 <- teethSmall[teethSmall$trtgroup == 5,]

tableone <- matrix(NA,nrow = 15, ncol = 6)
colnames(tableone) <- c("Variable", "Placebo", "No Treatment", "Low", "Medium", "High")
tableone[,1] <- c("Number","Sex", "Male", "Female", "Race", "Native American", "African American",
                  "Asian", "White", "Smoker", "Sites", "Attach (base)", "Attach (1 year)",
                  "PD (base)", "PD (1 year)")
tableone[1,2:6] <- table(teethSmall$trtgroup)
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

#export the file
write.csv(tableone, "C:/Repositories/bios6623-elcotton/Project0/Reports/tableOne.csv")

####Make graphs####
teeth$pdDiff <- teeth$pd1year-teeth$pdbase
teeth$attachDiff <- teeth$attach1year-teeth$attachbase
teeth$trtgroup <- as.numeric(teeth$trtgroup)
teeth$trtgroup[teeth$trtgroup == 2] <- "Control"
teeth$trtgroup[teeth$trtgroup == 1] <- "Placebo"
teeth$trtgroup[teeth$trtgroup == 3] <- "Low"
teeth$trtgroup[teeth$trtgroup == 4] <- "Medium"
teeth$trtgroup[teeth$trtgroup == 5] <- "High"
teeth$trtgroup <- factor(teeth$trtgroup, levels = c("Control", "Placebo", "Low", "Medium", "High"))


ggplot(teeth, aes(x = trtgroup, y = attachDiff)) +
  geom_boxplot(alpha = 0.7, fill = "darkslategray3") + 
  scale_y_continuous(name = "Difference in Attachment loss (Year 1-Baseline)") +
  scale_x_discrete(name = "Treatment Group") +
  ggtitle("Graph 1:\n Change in Attachemnt Loss by Treatment Group") +
  theme_bw() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        axis.title = element_text(face = "bold"),
        legend.title = element_blank())


ggplot(teeth, aes(x = trtgroup, y = pdDiff)) +
  geom_boxplot(alpha = 0.7, fill = "goldenrod1") + 
  scale_y_continuous(name = "Difference in Pocket Depth (Year 1-Baseline)") +
  scale_x_discrete(name = "Treatment Group") +
  ggtitle("Graph 2:\n Change in Pocket Depth by Treatment Group") +
  theme_bw() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        axis.title = element_text(face = "bold"),
        legend.title = element_blank())

###Should covariates be included?###
#None are significant, so nope!
teeth$age <- as.numeric(as.character(teeth$age))
summary(aov(teeth$age~teeth$trtgroup))
summary(aov(teeth$sites~teeth$trtgroup))
chisq.test(teeth$trtgroup,teeth$sex)
fisher.test(teeth$race, teeth$trtgroup)
fisher.test(teeth$smoker,teeth$trtgroup)

####Modeling####
#RUN MODEL!
modelAttach <- summary(lm(teeth$attachDiff~relevel(as.factor(teeth$trtgroup), "Control") + teeth$attachbase))
modelPD <- summary(lm(teeth$pdDiff~relevel(as.factor(teeth$trtgroup), "Control") + teeth$pdbase))

#check assumptions
par(mfrow = c(2,2))
modelAttachPlot <- lm(teeth$attachDiff~relevel(as.factor(teeth$trtgroup), "Control") + teeth$attachbase)
plot(modelAttachPlot, which = 1:4)

par(mfrow = c(2,2))
modelPDPlot <- lm(teeth$pdDiff~relevel(as.factor(teeth$trtgroup), "Control") + teeth$pdbase)
plot(modelPDPlot, which = 1:4)

#Make table based on model
tabletwo <- matrix(NA, nrow = 6, ncol = 4)
colnames(tabletwo) <- c("","Coefficients", "SE", "P-Value")
tabletwo[,1] <- c("Control", "Placebo", "Low", "Medium", "High", "Attachment Baseline")
tabletwo[,2] <- round(modelAttach$coefficients[,1], 4)
tabletwo[,3] <- round(modelAttach$coefficients[,2], 4)
tabletwo[,4] <- round(modelAttach$coefficients[,4], 4)

tablethree <- matrix(NA, nrow = 6, ncol = 4)
colnames(tablethree) <- c("","Coefficients", "SE", "P-Value")
tablethree[,1] <- c("Control", "Placebo", "Low", "Medium", "High", "Pocket Depth Baseline")
tablethree[,2] <- round(modelPD$coefficients[,1], 4)
tablethree[,3] <- round(modelPD$coefficients[,2], 4)
tablethree[,4] <- round(modelPD$coefficients[,4], 4)

write.csv(tabletwo, "C:/Repositories/bios6623-elcotton/Project0/Reports/tableTwo.csv")
write.csv(tablethree, "C:/Repositories/bios6623-elcotton/Project0/Reports/tableThree.csv")









