*Project 1-Initial analysis;

*Import the data;
proc import datafile = "C:\Users\cottonel\Documents\BIOS6623_AdvancedData\Project_One\project1_CleanedDataSas.csv"
	out = hiv dbms = csv replace;
	getnames = yes;
run;

*PROC MCMC---Copy Nichole's code from inclass model selection stuff;
*QOL-Physical as outcome;
proc mcmc data = hiv nbi = 30000 nmc = 100000 plots = all DIC;
	parms betaint 0 betaSmokerInd 0 betaDrinksInd 0 betaRaceInd 0 betaEducationInd 0 betaAdherenceInd 0 betaIncomeMedInd 0 
	betaIncomeHighInd 0 betaHashVInd 0 betaBMI 0 betaAge 0 betaHardDrugs 0 betaQOLBase 0;
	parms sigma2 1;
	prior betaint betaSmokerInd betaDrinksInd betaRaceInd betaEducationInd betaAdherenceInd betaIncomeMedInd 
	betaIncomeHighInd betaHashVInd betaBMI betaAge betaHardDrugs betaQOLBase ~ normal(mean = 0, var = 1000);
	prior sigma2 ~igamma(shape = 2.001, scale = 1.001);
	mu = betaint + betaSmokerInd*SmokerInd + betaDrinksInd*DrinksInd + betaEducationInd*EducationInd + betaAdherenceInd*AdherenceInd
			+ betaIncomeMedInd*IncomeMedInd + betaIncomeHighInd*IncomeHighInd + betaHashVInd*HashVInd + betaBMI*BMI +
			betaAge*Age + betaHardDrugs*HardDrugs + betaQOLBase*QOL_PhysBase;
	model QOL_PhysDiff ~ normal(mu, var = sigma2);
	title "Model 1: Full model with QOL_PhysDiff and All variables";
run;

proc mcmc data = hiv nbi = 30000 nmc = 100000 plots = all DIC;
	parms betaint 0 betaHardDrugs 0 betaQOLBase 0;
	parms sigma2 1;
	prior betaint betaHardDrugs betaQOLBase ~ normal(mean = 0, var = 1000);
	prior sigma2 ~igamma(shape = 2.001, scale = 1.001);
	mu = betaint + betaHardDrugs*HardDrugs + betaQOLBase*QOL_PhysBase;
	model QOL_PhysDiff ~ normal(mu, var = sigma2);
	title "Model 1: Crude model with QOL_PhysDiff and All variables";
run;

*GOOD TO GO!;


