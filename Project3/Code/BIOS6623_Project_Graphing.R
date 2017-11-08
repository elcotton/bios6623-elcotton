##############################################
# BIOS 6623: Project 3                       #
# Date: 11/7/17                              #
# Purpose: Graph the data                    #
##############################################

#library
library(ggplot2)

#import data
ment <- read.csv("C:/Users/cottonel/Documents/BIOS6623_AdvancedData/Project_Three/Project3Data_Cleaned.csv")

ment$demind <- factor(ment$demind)

###make the spaghetti plots: each outcome by age
#For the Wechsler Memory Scale Logical Memory I Story A
#logmemI variable
ggplot(data = ment, aes(x = age, y = logmemI, group = id, color = demind)) +
  geom_line()

#For the Wechsler Memory Scale Logical Memory II Story A;
#logmemII variable
ggplot(data = ment, aes(x = age, y = logmemII, group = id, color = demind)) +
  geom_line()


#For category fluency for animals;
#Animals variable
ggplot(data = ment, aes(x = age, y = animals, group = id, color = demind)) +
  geom_line()


#For the Wechsler Adult Intelligence Scale-Revised Block Design
#BlockR
ggplot(data = ment, aes(x = age, y = blockR, group = id, color = demind)) +
  geom_line()







