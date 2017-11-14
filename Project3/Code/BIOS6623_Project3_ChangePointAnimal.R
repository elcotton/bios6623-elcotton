###############################################
# BIOS 6623: Project 3                        #
# Date: 11/7/17                               #
# Purpose: Identify change point and model it #
#          for animals                        #
###############################################

#library
library(ggplot2)
library(nlme)

#import data
ment <- read.csv("C:/Users/cottonel/Documents/BIOS6623_AdvancedData/Project_Three/Project3Data_Cleaned.csv")

#get the difference between age and age of onset
ment$ageDiff <- ment$age-ment$ageonset

mentDem <- ment[ment$demind == 1,]

animal <- mentDem[!is.na(mentDem$animals),]
keep <- names(which(table(animal$id)>=3))
animal <- animal[animal$id %in% keep,]

ggplot(data = animal, aes(x = ageDiff, y = animals, group = id)) +
  geom_line() + 
  theme_bw() +
  scale_x_continuous(name = "Year before Dx") +
  scale_y_continuous(name = "Animal Score") +
  labs(title = "Graph 1: Animal Score over time")+
  theme(text = element_text(size=12)) +
  theme(plot.title = element_text(size = rel(2), hjust = 0.5))+ 
  theme(axis.title = element_text(face = "bold")) +
  geom_vline(xintercept = 0, color = "red")

###Following camille's example-Animals
#Data
patid<-as.character(animal$id)
t1<-animal$ageDiff
y<-animal$animals
ses <- animal$SES
gender <- as.factor(animal$gender)
age <- animal$age

#Sequence of change points to consider
cps<-seq(-12,-0.1,0.1)

#Create a function to search for change point
#and fit final change point model
cp.search_and_fit<-function(patid, t1, y, cps,ses,gender,age){
  #Place to store likelihoods from the CP search
  ll<-data.frame(changepoint=rep(NA,length(cps)), ll=rep(NA,length(cps)))
  
  #Search for the CP
  for (i in 1:length(cps)){cp<-cps[i]
  t2<-ifelse(t1>cp, t1-cp, 0)
  cp.model<-lme(y~t1+t2 + gender + ses + age, random=~1|patid, method='ML')
  ll[i,]<-c(cp,logLik(cp.model))
  }
  
  #Plot the likelihood
  plot(ll$changepoint, ll$ll, type='l', xlab='Change Point (years)', ylab='Log Likelihood')
  
  #Find the max
  cp<-ll[which(ll$ll==max(ll$ll)),'changepoint']
  print(cp)
  
  #Fit the final model
  t2<-ifelse(t1>cp, t1-cp, 0)
  cp.model<-lme(y~t1+t2+ gender + ses + age, random=~1|patid)
  return(list(cp=cp, model=cp.model))}


#Run the function on the dataset
cp.model <-cp.search_and_fit(patid, t1, y, cps, ses,gender,age)

summary(cp.model$model)
