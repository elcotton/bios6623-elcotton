*Project 1-Initial analysis;

*Import the data;
proc import datafile = "C:\Users\cottonel\Documents\BIOS6623_AdvancedData\Project_One\project1_CleanedDataSas.csv"
	out = hiv dbms = csv replace;
	getnames = yes;
run;

*PROC MCMC---Copy Nichole's code from inclass model selection stuff;
*CD4Count as outcome---Full Model;
proc mcmc data = hiv nbi = 30000 nmc = 300000 plots = all thin = 5 DIC;
	parms betaint 0 betaSmokerInd 0 betaDrinksInd 0 betaRaceInd 0 betaEducationInd 0 betaAdherenceInd 0 betaIncomeMedInd 0 
	betaIncomeHighInd 0 betaHashVInd 0 betaBMI 0 betaAge 0 betaHardDrugs 0 betaCD4Base 0;
	parms sigma2 1;
	prior betaint betaSmokerInd betaDrinksInd betaRaceInd betaEducationInd betaAdherenceInd betaIncomeMedInd 
	betaIncomeHighInd betaHashVInd betaBMI betaAge betaHardDrugs betaCD4Base ~ normal(mean = 0, var = 1000);
	prior sigma2 ~igamma(shape = 2.001, scale = 1.001);
	mu = betaint + betaSmokerInd*SmokerInd + betaDrinksInd*DrinksInd + betaEducationInd*EducationInd + betaAdherenceInd*AdherenceInd
			+ betaIncomeMedInd*IncomeMedInd + betaIncomeHighInd*IncomeHighInd + betaHashVInd*HashVInd + betaBMI*BMI +
			betaAge*Age + betaHardDrugs*HardDrugs + betaCD4Base*CD4CountBase;
	model CD4CountDiff ~ normal(mu, var = sigma2);
	title "Model 1: Full model with CD4Count and All variables";
run;

*PROC MCMC---Copy Nichole's code from inclass model selection stuff;
*CD4Count as outcome---Crude Model;
proc mcmc data = hiv nbi = 30000 nmc = 300000 plots = all thin = 5 DIC;
	parms betaint 0 betaHardDrugs 0 betaCD4Base 0;
	parms sigma2 1;
	prior betaint betaHardDrugs betaCD4Base ~ normal(mean = 0, var = 1000);
	prior sigma2 ~igamma(shape = 2.001, scale = 1.001);
	mu = betaint + betaHardDrugs*HardDrugs + betaCD4Base*CD4CountBase;
	model CD4CountDiff ~ normal(mu, var = sigma2);
	title "Model 1: Crude Model with CD4Count and All variables";
run;

*The Crude and Full don't change the estimate of drug use! DONE! GOLDEN!;
