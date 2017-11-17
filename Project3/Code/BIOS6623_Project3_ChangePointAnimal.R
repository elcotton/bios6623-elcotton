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

###Make graph that is in the paper, for block ID
animalsGraph <- mentDem[!is.na(mentDem$animals),]
keep <- names(which(table(animalsGraph$id)>=3))
animalsGraph <- animalsGraph[animalsGraph$id %in% keep,]

ggplot(data = animalsGraph, aes(x = ageDiff, y = animals, group = id)) +
  geom_line() + 
  theme_bw() +
  scale_x_continuous(name = "Year before Dx") +
  scale_y_continuous(name = "Block R Score") +
  labs(title = "Graph 1: Block R Score over time")+
  theme(text = element_text(size=12)) +
  theme(plot.title = element_text(size = rel(2), hjust = 0.5))+ 
  theme(axis.title = element_text(face = "bold")) +
  geom_vline(xintercept = 0, color = "red")

###Following camille's example-animals
#Data
animals <- ment[!is.na(ment$animals),]
keep <- names(which(table(animals$id)>=3))
animals <- animals[animals$id %in% keep,]


patid<-as.character(animals$id)
t1<-animals$age-59
ageDiff <- ifelse(is.na(animals$ageDiff),0,animals$ageDiff)
y<-animals$animals
ses <- animals$SES
gender <- as.factor(animals$gender)
demind <- animals$demind



#Sequence of change points to consider
cps<-seq(-12,-2,0.1)

#Create a function to search for change point
#and fit final change point model
cp.search_and_fit<-function(patid, t1, y, cps,ses,gender,age){
  #Place to store likelihoods from the CP search
  ll<-data.frame(changepoint=rep(NA,length(cps)), ll=rep(NA,length(cps)))
  
  #Search for the CP
  for (i in 1:length(cps)){
    cp<-cps[i]
    t2<-ifelse(ageDiff>cp, ageDiff-cp, 0)
    cp.model<-lme(y~t1+t2 + gender + ses + demind + demind*t1, random=~1|patid, method='ML')
    ll[i,]<-c(cp,logLik(cp.model))
  }
  
  #Plot the likelihood
  plot(ll$changepoint, ll$ll, type='l', xlab='Change Point (years)', ylab='Log Likelihood')
  
  #Find the max
  cp<-ll[which(ll$ll==max(ll$ll)),'changepoint']
  print(cp)
  
  #Fit the final model
  t2<-ifelse(ageDiff>cp, ageDiff-cp, 0)
  cp.model<-lme(y~t1+t2 + gender + ses + demind + demind*t1, random=~1|patid)
  return(list(cp=cp, model=cp.model))}


#Run the function on the dataset
cp.model <-cp.search_and_fit(patid, t1, y, cps, ses,gender,age)

summary(cp.model$model)