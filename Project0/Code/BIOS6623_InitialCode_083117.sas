*BIOS 6623-Project Zero;

*import data;
proc import datafile = "C:\Users\cottonel\Documents\BIOS6623_AdvancedData\Project_Zero\ProjectZeroData.csv"
	out = teeth dbms = csv replace;
	getnames = yes;
run;

