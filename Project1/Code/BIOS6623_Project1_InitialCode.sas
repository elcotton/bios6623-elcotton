*Project 1-Initial analysis;

*Import the data;
proc import datafile = "C:\Users\cottonel\Documents\BIOS6623_AdvancedData\Project_One\BIOS6623_Project1_DataSas.csv"
	out = hiv dbms = csv replace;
	getnames = yes;
run;
	
