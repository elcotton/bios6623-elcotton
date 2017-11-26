###############################################
# BIOS 6623: Project 3                        #
# Date: 11/7/17                               #
# Purpose: Identify change point and model it #
#          for animals                        #
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
animalsGraph <- mentDem[!is.na(mentDem$animals),]
keep <- names(which(table(animalsGraph$id)>=3))
animalsGraph <- animalsGraph[animalsGraph$id %in% keep,]

ggplot(data = animalsGraph, aes(x = ageDiff, y = animals, group = id)) +
  geom_line() + 
  theme_bw() +
  scale_x_continuous(name = "Year before Dx", breaks = c(-12,-10,-8,-6,-4,-2,0,2,4,6,8)) +
  scale_y_continuous(name = "Animal Score", breaks = c(0,5,10,15,20,25,30)) +
  labs(title = "Graph 2: Animal Score over time for Dementia Patients")+
  theme(text = element_text(size=12)) +
  theme(plot.title = element_text(size = rel(2), hjust = 0.5))+ 
  theme(axis.title = element_text(face = "bold")) +
  geom_vline(xintercept = 0, color = "red")+
  geom_vline(xintercept = -3.9, color = "blue", linetype = "longdash")

###Following camille's example-animals
#Data
animals <- ment[!is.na(ment$animals),]
keep <- names(which(table(animals$id)>=3))
animals <- animals[animals$id %in% keep,]
animals$age_Adj <- animals$age-59


patid<-as.character(animals$id)
t1<-animals$age-59
ageDiff <- ifelse(is.na(animals$ageDiff),0,animals$ageDiff)
y<-animals$animals
ses <- animals$SES
gender <- as.factor(animals$gender)
demind <- animals$demind



#Sequence of change points to consider
cps<-seq(-6,2,0.1)

#Create a function to search for change point
#and fit final change point model
cp.search_and_fit<-function(patid, t1, y, cps,ses,gender,demind,ageDiff){
  #Place to store likelihoods from the CP search
  ll<-data.frame(changepoint=rep(NA,length(cps)), ll=rep(NA,length(cps)))
  
  #Search for the CP
  for (i in 1:length(cps)){
    cp<-cps[i]
    t2<-ifelse(ageDiff>cp, ageDiff-cp, 0)
    cp.model <- lme(y~t1+t2 + gender + ses + demind + demind*t1, random=~1|patid, correlation = corAR1(), method="REML")
    #cp.model <- lme(y~t1+t2 + gender + ses + demind + demind*t1, random=~1|patid, correlation = corCAR1(form=~t1), method="REML")
    #cp.model<-lme(y~t1+t2 + gender + ses + demind + demind*t1, random=~1|patid, method='ML')
    ll[i,]<-c(cp,logLik(cp.model))
  }
  
  #Plot the likelihood
  #plot(ll$changepoint, ll$ll, type='l', xlab='Change Point (years)', ylab='Log Likelihood')
  
  #Find the max
  cp<-ll[which(ll$ll==max(ll$ll)),'changepoint']
  #print(cp)
  
  #Fit the final model
  t2<-ifelse(ageDiff>cp, ageDiff-cp, 0)
  cp.model<-lme(y~t1+t2 + gender + ses + demind + demind*t1, random=~1|patid, correlation = corAR1(), method="REML")
  return(list(cp=cp, model=cp.model))}


#Run the function on the dataset
cp.model <-cp.search_and_fit(patid, t1, y, cps, ses,gender,demind,ageDiff)

summary(cp.model$model)

###Bootstrap attend: Get SE and CI for changepoint
cps<-seq(-6,0,0.1)

###ONLY RUN ONCE-RESULTS SAVED AND LOADED BELOW, TAKES A VERY LONG TIME TO RUN!
# boot.function<-function(ids=animals$id, dat=animals, cps=cps){
#   #Step 1: Get a bootstrap sample
#   
#   #Since we took a random sample of SUBJECTS, must sample subjects in our bootstrap
#   ids.u<-unique(ids)
#   boot.subjects<-sample(ids.u, length(ids.u), replace=T)
#   
#   #Grab the data for each of the chosen subjects
#   boot.dat<-NULL
#   for (i in 1:length(ids.u)){ temp<-cbind(i, dat[ids==boot.subjects[i],])
#   boot.dat<-rbind(boot.dat, temp)
#   }
#   
#   #Step 2: Repeat the analysis on the bootstrap sample
#   boot.model <-cp.search_and_fit(patid = as.character(boot.dat$id), 
#                                  t1 = boot.dat$age_Adj, 
#                                  y = boot.dat$animals, 
#                                  cps = cps,
#                                  ses = boot.dat$SES, 
#                                  gender = as.factor(boot.dat$gender), 
#                                  demind = as.factor(boot.dat$demind), 
#                                  ageDiff = ifelse(is.na(boot.dat$ageDiff),0,boot.dat$ageDiff))
#   
#   #Step 3: Save the estimates and CP's
#   
#   boot.rslt<-c(boot.model$cp,
#                confint(glht(boot.model $model, matrix(c(0,1, 1,0,0,0,0), nrow=1)))$confint[1], #slope after change point
#                confint(glht(boot.model $model, matrix(c(0,1, 0,0,0,0,0), nrow=1)))$confint[1], #slope before change point
#                boot.model$model$coefficients$fixed[1],
#                boot.model$model$coefficients$fixed[2],
#                boot.model$model$coefficients$fixed[3],
#                boot.model$model$coefficients$fixed[4],
#                boot.model$model$coefficients$fixed[5],
#                boot.model$model$coefficients$fixed[6],
#                boot.model$model$coefficients$fixed[7]) 
#   names(boot.rslt)<-c("CP","Slope1", "Slope2", "intercept", "t1", "t2", "gender", "ses", "demind", "interaction")
#   
#   return(boot.rslt)
# }
# 
# #Run on lots of samples!!
# niter <- 1000
# 
# bootstraps<-matrix(NA, ncol=10, nrow=1000)
# ptm <-proc.time()
# for (j in 223:niter){
#   bootstraps[j,]<-boot.function(ids=animals$id, dat=animals, cps=cps)
#   print(j)
# }
# proc.time()-ptm
# 
# #save the bootstraps
# write.csv(bootstraps, "C:/Repositories/bios6623-elcotton/Project3/Reports/bootStrap.csv")

boot <- read.csv("C:/Repositories/bios6623-elcotton/Project3/Reports/bootStrap.csv")
colnames(boot) <- c("x","CP","Slope1", "Slope2", "intercept", "t1", "t2", "gender", "ses", "demind", "interaction")
quantile(boot[,2], probs = c(0.025,0.975))

###Make table2-results of bootstrap and modelling!
#run and save our model

patid<-as.character(animals$id)
t1<-animals$age-59
ageDiff <- ifelse(is.na(animals$ageDiff),0,animals$ageDiff)
y<-animals$animals
ses <- animals$SES
gender <- as.factor(animals$gender)
demind <- animals$demind
cp <- -3.9
t2<-ifelse(ageDiff>cp, ageDiff-cp, 0)

model <- lme(y~t1+t2 + gender + ses + demind + demind*t1, random=~1|patid, correlation = corAR1(), method="REML")

tableTwo <- data.frame(matrix(NA,nrow = 7, ncol = 5))
colnames(tableTwo) <- c("Variable", "Estimate", "SE", "95% CI", "P-value")
tableTwo[,1] <- c("Intercept", "Age", "Change Point", "Gender", "SES",
                  "Dementia", "Interaction (Dem and Age)")

tableTwo[1:7,2] <- model$coefficients$fixed[1:7]
tableTwo[1:7,3] <- unlist(lapply(boot[,5:11], sd))
tableTwo[1:7,4] <- paste("(",round(tableTwo[1:7,2] - 1.96*tableTwo[1:7,3],3),",",round(tableTwo[1:7,2] + 1.96*tableTwo[1:7,3],3),")", sep = "")

#get t-values
tval <- tableTwo[1:7,2]/tableTwo[1:7,3]

tableTwo[c(2,3,4,6),5] <- pnorm(tval[c(2,3,4,6)])*2
tableTwo[c(1,5,7),5] <- (1-pnorm(tval[c(1,5,7)]))*2

#round everything
tableTwo[1:7,2] <- round(tableTwo[1:7,2],3)
tableTwo[1:7,3] <- round(tableTwo[1:7,3],4)
tableTwo[1:7,5] <- round(tableTwo[1:7,5],4)

write.csv(tableTwo, "C:/Repositories/bios6623-elcotton/Project3/Reports/tableTwo.csv")






















