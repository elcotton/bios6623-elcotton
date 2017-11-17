###############################################
# BIOS 6623: Project 3                        #
# Date: 11/7/17                               #
# Purpose: Identify change point and model it #
#          for blockR                         #
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
blockRgraph <- mentDem[!is.na(mentDem$blockR),]
keep <- names(which(table(blockRgraph$id)>=3))
blockRgraph <- blockRgraph[blockRgraph$id %in% keep,]

ggplot(data = blockRgraph, aes(x = ageDiff, y = blockR, group = id)) +
  geom_line() + 
  theme_bw() +
  scale_x_continuous(name = "Year before Dx") +
  scale_y_continuous(name = "Block R Score") +
  labs(title = "Graph 1: Block R Score over time")+
  theme(text = element_text(size=12)) +
  theme(plot.title = element_text(size = rel(2), hjust = 0.5))+ 
  theme(axis.title = element_text(face = "bold")) +
  geom_vline(xintercept = 0, color = "red")

###Following camille's example-BlockR
#Data
blockR <- ment[!is.na(ment$blockR),]
keep <- names(which(table(blockR$id)>=3))
blockR <- blockR[blockR$id %in% keep,]


patid<-as.character(blockR$id)
t1<-blockR$age-59
ageDiff <- ifelse(is.na(blockR$ageDiff),0,blockR$ageDiff)
y<-blockR$blockR
ses <- blockR$SES
gender <- as.factor(blockR$gender)
demind <- blockR$demind



#Sequence of change points to consider
cps<-seq(-14,-1,0.1)

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


