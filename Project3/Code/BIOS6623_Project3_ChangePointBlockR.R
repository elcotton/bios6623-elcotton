###############################################
# BIOS 6623: Project 3                        #
# Date: 11/7/17                               #
# Purpose: Identify change point and model it #
#          for blockR                         #
###############################################

#library
library(ggplot2)
library(nlme)
library(multcomp)

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
  labs(title = "Graph 8: Block R Score over time")+
  theme(text = element_text(size=12)) +
  theme(plot.title = element_text(size = rel(2), hjust = 0.5))+ 
  theme(axis.title = element_text(face = "bold")) +
  geom_vline(xintercept = 0, color = "red")

###Following camille's example-BlockR
#Data
blockR <- ment[!is.na(ment$blockR),]
keep <- names(which(table(blockR$id)>=3))
blockR <- blockR[blockR$id %in% keep,]
blockR$age_Adj <- blockR$age-59


patid<-as.character(blockR$id)
t1<-blockR$age-59
ageDiff <- ifelse(is.na(blockR$ageDiff),0,blockR$ageDiff)
y<-blockR$blockR
ses <- blockR$SES
gender <- as.factor(blockR$gender)
demind <- blockR$demind

#Sequence of change points to consider
cps<-seq(-6,2,0.1)

#Create a function to search for change point
#and fit final change point model
cp.search_and_fit<-function(patid, t1, y, cps,ses,gender,demind, ageDiff){
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
  #plot(ll$changepoint, ll$ll, type='l', xlab='Change Point (years)', ylab='Log Likelihood')
  
  #Find the max
  cp<-ll[which(ll$ll==max(ll$ll)),'changepoint']
  #print(cp)
  
  #Fit the final model
  t2<-ifelse(ageDiff>cp, ageDiff-cp, 0)
  cp.model<-lme(y~t1+t2 + gender + ses + demind + demind*t1, random=~1|patid)
  return(list(cp=cp, model=cp.model))
}


#Run the function on the dataset
cp.model <-cp.search_and_fit(patid, t1, y, cps, ses,gender,demind, ageDiff)

summary(cp.model$model)




###Bootstrap attend: Get SE and CI for changepoint
cps<-seq(-6,2,0.1)

boot.function<-function(ids=blockR$id, dat=blockR, cps=cps){
  #Step 1: Get a bootstrap sample
  
  #Since we took a random sample of SUBJECTS, must sample subjects in our bootstrap
  ids.u<-unique(ids)
  boot.subjects<-sample(ids.u, length(ids.u), replace=T)
  
  #Grab the data for each of the chosen subjects
  boot.dat<-NULL
  for (i in 1:length(ids.u)){ temp<-cbind(i, dat[ids==boot.subjects[i],])
  boot.dat<-rbind(boot.dat, temp)
  }
  
  #Step 2: Repeat the analysis on the bootstrap sample
  boot.model <-cp.search_and_fit(patid = as.character(boot.dat$id), 
                                 t1 = boot.dat$age_Adj, 
                                 y = boot.dat$blockR, 
                                 cps = cps,
                                 ses = boot.dat$SES, 
                                 gender = as.factor(boot.dat$gender), 
                                 demind = boot.dat$demind, 
                                 ageDiff = ifelse(is.na(boot.dat$ageDiff),0,boot.dat$ageDiff))
  
  #Step 3: Save the estimates and CP's
  
  boot.rslt<-c(boot.model$cp,
               confint(glht(boot.model $model, matrix(c(0,1, 1,0,0,0,0), nrow=1)))$confint[1], #slope after change point
               confint(glht(boot.model $model, matrix(c(0,1, 0,0,0,0,0), nrow=1)))$confint[1]) #slope before change point
  names(boot.rslt)<-c("CP","Slope1", "Slope2")
  
  return(boot.rslt)
}

#Run on lots of samples!! TAKES SO LONG!
niter <- 100

bootstraps<-matrix(NA, ncol=3, nrow=1000)
ptm <-proc.time()
for (j in 1:niter){
  bootstraps[j,]<-boot.function(ids=blockR$id, dat=blockR, cps=cps)
  print(j)
}
proc.time()-ptm #15.64

#doesn't work
library(parallel)
cl <- makeCluster(getOption("cl.cores",4))
clusterExport(cl, list("boot.function", "blockR", "cps", "cp.search_and_fit", "lme", "glht"))

bootBlockR <- NULL
ptm <-proc.time()
bootBlockR <- matrix(unlist(parLapply(cl,1:100,function(x) boot.function(ids=blockR$id, dat=blockR, cps=cps))),ncol = 3,byrow = TRUE)
proc.time()-ptm

write.csv(bootBlockR, "")

stopCluster(cl)
