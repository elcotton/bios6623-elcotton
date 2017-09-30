*Project 1-Initial analysis;

*Import the data;
proc import datafile = "C:\Users\cottonel\Documents\BIOS6623_AdvancedData\Project_One\project1_CleanedDataSas.csv"
	out = hiv dbms = csv replace;
	getnames = yes;
run;

*PROC MCMC---Copy Nichole's code from inclass model selection stuff;

