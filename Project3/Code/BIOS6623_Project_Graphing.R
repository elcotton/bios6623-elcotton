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

#Subset into complete for each
mentLogI <- ment[!is.na(ment$logmemI),]
mentLogII <- ment[!is.na(ment$logmemII),]
animal <- ment[!is.na(ment$animals),]
blockR <- ment[!is.na(ment$blockR),]

#Delete anyone who has less than 3 measurements for each
keep <- names(which(table(mentLogI$id)>=3))
mentLogI <- mentLogI[mentLogI$id %in% keep,]

#delete those who don't have three measurement
keep <- names(which(table(mentLogI$id)>=3))
mentLogI <- mentLogI[mentLogI$id %in% keep,]

keep <- names(which(table(mentLogII$id)>=3))
mentLogII <- mentLogII[mentLogII$id %in% keep,]

keep <- names(which(table(animal$id)>=3))
animal <- animal[animal$id %in% keep,]

keep <- names(which(table(blockR$id)>=3))
blockR <- blockR[blockR$id %in% keep,]

###make the spaghetti plots: each outcome by age
#For the Wechsler Memory Scale Logical Memory I Story A
#logmemI variable
ggplot(data = mentLogI, aes(x = age, y = logmemI, group = id, color = demind)) +
  geom_line() + 
  scale_color_manual(values=c("deepskyblue", "darkgoldenrod1"), name = "Dementia\nStatus",
                     labels = c("No", "Yes")) +
  theme_bw() +
  scale_x_continuous(name = "Age (Years)") +
  scale_y_continuous(name = "Logical Memory I Story A score") +
  labs(title = "Graph 1: Logical Memory I Score over time")+
  theme(text = element_text(size=12)) +
  theme(plot.title = element_text(size = rel(2), hjust = 0.5))+ 
  theme(axis.title = element_text(face = "bold")) +
  theme(legend.title = element_text(face = "bold")) +
  theme(legend.title.align=0.5) 

#For the Wechsler Memory Scale Logical Memory II Story A;
#logmemII variable
ggplot(data = mentLogII, aes(x = age, y = logmemII, group = id, color = demind)) +
  geom_line()+ 
  scale_color_manual(values=c("deepskyblue", "darkgoldenrod1"), name = "Dementia\nStatus",
                     labels = c("No", "Yes")) +
  theme_bw() +
  scale_x_continuous(name = "Age (Years)") +
  scale_y_continuous(name = "Logical Memory II Story A score") +
  labs(title = "Graph 2: Logical Memory II Score over time")+
  theme(text = element_text(size=12)) +
  theme(plot.title = element_text(size = rel(2), hjust = 0.5))+ 
  theme(axis.title = element_text(face = "bold")) +
  theme(legend.title = element_text(face = "bold")) +
  theme(legend.title.align=0.5) 


#For category fluency for animals;
#Animals variable
ggplot(data = animal, aes(x = age, y = animals, group = id, color = demind)) +
  geom_line() + 
  scale_color_manual(values=c("deepskyblue", "darkgoldenrod1"), name = "Dementia\nStatus",
                     labels = c("No", "Yes")) +
  theme_bw() +
  scale_x_continuous(name = "Age (Years)") +
  scale_y_continuous(name = "Animal Score") +
  labs(title = "Graph 1: Animal Score over time")+
  theme(text = element_text(size=12)) +
  theme(plot.title = element_text(size = rel(2), hjust = 0.5))+ 
  theme(axis.title = element_text(face = "bold")) +
  theme(legend.title = element_text(face = "bold")) +
  theme(legend.title.align=0.5) 


#For the Wechsler Adult Intelligence Scale-Revised Block Design
#BlockR
ggplot(data = blockR, aes(x = age, y = blockR, group = id, color = demind)) +
  geom_line() + 
  scale_color_manual(values=c("deepskyblue", "darkgoldenrod1"), name = "Dementia\nStatus",
                     labels = c("No", "Yes")) +
  theme_bw() +
  scale_x_continuous(name = "Age (Years)") +
  scale_y_continuous(name = "Block Design Test Score") +
  labs(title = "Graph 4: Block Design Test Score over time")+
  theme(text = element_text(size=12)) +
  theme(plot.title = element_text(size = rel(2), hjust = 0.5))+ 
  theme(axis.title = element_text(face = "bold")) +
  theme(legend.title = element_text(face = "bold")) +
  theme(legend.title.align=0.5) 

# #save these datasets
# write.csv(animal, "C:/Users/cottonel/Documents/BIOS6623_AdvancedData/Project_Three/Project3Data_Animal.csv")
# write.csv(blockR, "C:/Users/cottonel/Documents/BIOS6623_AdvancedData/Project_Three/Project3Data_Block.csv")
# write.csv(mentLogI, "C:/Users/cottonel/Documents/BIOS6623_AdvancedData/Project_Three/Project3Data_LogI.csv")
# write.csv(mentLogII, "C:/Users/cottonel/Documents/BIOS6623_AdvancedData/Project_Three/Project3Data_LogII.csv")






