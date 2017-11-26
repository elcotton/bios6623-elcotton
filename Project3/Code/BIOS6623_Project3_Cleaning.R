##############################################
# BIOS 6623: Project 3                       #
# Date: 11/7/17                              #
# Purpose: Clean the data/orgainze it        #
##############################################


#import the data
ment <- read.csv("C:/Users/cottonel/Documents/BIOS6623_AdvancedData/Project_Three/Project3Data.csv")

# ment$numNA <- NA
# #Delete any rows that did not have a single outcome
# for(i in 1:nrow(ment)){
#   ment$numNA[i] <- sum(is.na(ment[i,c(6:9)]))
# }
# 
# ment <- ment[ment$numNA != 4,]


#Delete those who have less than three measurements
keep <- names(which(table(ment$id)>=3))
ment <- ment[ment$id %in% keep,]


#save the data
write.csv(ment, "C:/Users/cottonel/Documents/BIOS6623_AdvancedData/Project_Three/Project3Data_Cleaned.csv")
