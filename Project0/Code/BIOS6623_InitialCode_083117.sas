*BIOS 6623-Project Zero;

*import data;
proc import datafile = "C:\Users\cottonel\Documents\BIOS6623_AdvancedData\Project_Zero\ProjectZeroData.csv"
	out = teeth dbms = csv replace;
	getnames = yes;
run;

data teeth;
	set teeth;
	attachDiff = attach1year-attachbase;
	pdDiff = pd1year-pdbase;
run; 

*Run model;
*add to get coefficient estimates;
proc glm data = teeth;
	class trtgroup;
	model attach1year = trtgroup attachbase;
run;

proc glm data = teeth;
	class trtgroup;
	model pd1year = trtgroup pdbase;
run;

