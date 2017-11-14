###############################################################################################
#Analysis Spine Surgery Data
###############################################################################################

#Read in Data
	dat_analysis <-read.csv('/Users/camillemoore/Dropbox/boot_ex.csv')
	

#Spaghetti Plots
	xyplot(dat_analysis $VAS~ dat_analysis $time, groups= dat_analysis $patid, type='l', xlim=c(0,20), xlab="Time (months)", ylab='VAS')

	scatter.smooth(dat_analysis $VAS~ dat_analysis $time, xlim=c(0,12), col=dat_analysis$newid)



###############################################################################################
#Fit change point model
###############################################################################################

#Data
	patid<-as.character(dat_analysis $patid)
	t1<-dat_analysis$time
	y<-dat_analysis$VAS

#Sequence of change points to consider
	cps<-seq(0.1,12,0.01)


#Create a function to search for change point
#and fit final change point model

	cp.search_and_fit<-function(patid, t1, y, cps){
	#Place to store likelihoods from the CP search
		ll<-data.frame(changepoint=rep(NA,length(cps)), ll=rep(NA,length(cps)))

	#Search for the CP
		for (i in 1:length(cps)){cp<-cps[i]
			t2<-ifelse(t1>cp, t1-cp, 0)
			cp.model<-lme(y~t1+t2, random=~1|patid, method='ML')
 			ll[i,]<-c(cp,logLik(cp.model))
                          }

	#Plot the likelihood
		plot(ll$changepoint, ll$ll, type='l', xlab='Change Point (months)', ylab='Log Likelihood')

	#Find the max
		cp<-ll[which(ll$ll==max(ll$ll)),'changepoint']
		print(cp)
	
	#Fit the final model
		t2<-ifelse(t1>cp, t1-cp, 0)
		cp.model<-lme(y~t1+t2, random=~1|patid)
		return(list(cp=cp, model=cp.model))}

#Run the function on the dataset
	cp.model <-cp.search_and_fit(patid, t1, y, cps)

	summary(cp.model$model)

	coeff<-cp.model$model$coefficients$fixed

#Get estimates of Changes at 3, 6, 9, and 12 months, as well as slopes before and after change point
#These CI's are likely too narrow since they don't take into account the fact that we estimated the cp!
library(multcomp)

	confint(glht(cp.model$model, matrix(c(0, 3, 3-0.64), nrow=1))	) #Estimate of change at 3 months
	confint(glht(cp.model$model, matrix(c(0, 6, 6-0.64), nrow=1))	) #Estimate of change at 6 months
	confint(glht(cp.model$model, matrix(c(0, 9, 9-0.64), nrow=1))	) #Estimate of change at 9 months
	confint(glht(cp.model$model, matrix(c(0, 12, 12-0.64), nrow=1))) #Estimate of change at 12 months
	confint(glht(cp.model$model, matrix(c(0,1, 1), nrow=1))) #slope after change point
	confint(glht(cp.model$model, matrix(c(0,1, 0), nrow=1))) #slope before change point




#####################################################################################################
#Use a bootstrap to get estimates of standard errors and a CI for the changepoint
#####################################################################################################

boot.function<-function(ids=dat_analysis$newid, dat=dat_analysis, cps=cps){
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
	boot.model <-cp.search_and_fit(boot.dat$i, boot.dat$time, boot.dat$VAS, cps)


#Step 3: Save the estimates and CP's
	
boot.rslt<-c(boot.model$cp, confint(glht(boot.model $model, matrix(c(0, 3, 3-boot.model$cp), nrow=1)))$confint[1], #Estimate of change at 3m
	confint(glht(boot.model $model, matrix(c(0, 6, 6-boot.model$cp), nrow=1)))$confint[1], #Estimate of change at 6 months
	confint(glht(boot.model $model, matrix(c(0, 9, 9-boot.model$cp), nrow=1)))$confint[1], #Estimate of change at 9 months
	confint(glht(boot.model $model, matrix(c(0, 12, 12-boot.model$cp), nrow=1)))$confint[1], #Estimate of change at 12 months
	confint(glht(boot.model $model, matrix(c(0,1, 1), nrow=1)))$confint[1], #slope after change point
	confint(glht(boot.model $model, matrix(c(0,1, 0), nrow=1)))$confint[1]) #slope before change point
names(boot.rslt)<-c("Changepoint", "Month_3_Change","Month_6_Change","Month_9_Change","Month_12_Change", "Slope1", "Slope2")

return(boot.rslt)
}

#Step 4: Repeat many times

niter<-1000

bootstraps<-matrix(NA, ncol=7, nrow=1000)
for (j in 1:niter){bootstraps[j,]<-boot.function(ids=dat_analysis$newid, dat=dat_analysis, cps=cps)
	}

#Step 5:Inspect the bootstrap distributions
	
	#Change Point
	hist(bootstraps[,1], xlab="Change Point", main='Bootstrap Distribution of Change Points')
	lines(c(cp,cp), c(0,1000), col='red')
	lines(c(mean(bootstraps[,1]),mean(bootstraps[,1])), c(0,1000))

	mean(bootstraps[,1])
	sd(bootstraps[,1])
	quantile(bootstraps[,1], c(0.025, 0.975))

	#Change in VAS at 3 months
	hist(bootstraps[,2], xlab="3 Month Change in VAS", main='Bootstrap Estimates of 3 Month VAS Change')
	lines(c(-3.3010,-3.3010), c(0,1000), col='red')
	lines(c(mean(bootstraps[,2]),mean(bootstraps[,2])), c(0,1000))

	mean(bootstraps[,2])
	sd(bootstraps[,2])
	quantile(bootstraps[,2], c(0.025, 0.975))

#Get percentile CI's for other estimates of interest
	quantile(bootstraps[,3], c(0.025, 0.975))
	quantile(bootstraps[,4], c(0.025, 0.975))
	quantile(bootstraps[,5], c(0.025, 0.975))
	quantile(bootstraps[,6], c(0.025, 0.975))
	quantile(bootstraps[,7], c(0.025, 0.975))


