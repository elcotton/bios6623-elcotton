**********************************************;
* BIOS 6623: Project 3                        ;
* Date: 11/14/17                              ;
* Purpose: run model with change point        ;
**********************************************;

*import the datasets;
proc import datafile = "C:/Users/cottonel/Documents/BIOS6623_AdvancedData/Project_Three/Project3Data_Animal.csv"
	out = animal dbms = csv replace;
	getnames = yes;
run;

proc import datafile = "C:/Users/cottonel/Documents/BIOS6623_AdvancedData/Project_Three/Project3Data_Block.csv"
	out = block dbms = csv replace;
	getnames = yes;
run;

proc import datafile = "C:/Users/cottonel/Documents/BIOS6623_AdvancedData/Project_Three/Project3Data_LogI.csv"
	out = logI dbms = csv replace;
	getnames = yes;
run;

proc import datafile = "C:/Users/cottonel/Documents/BIOS6623_AdvancedData/Project_Three/Project3Data_LogII.csv"
	out = logII dbms = csv replace;
	getnames = yes;
run;

data logII;
	set logII;
	ageDiff = age-ageonset;
	time_star = max(-3.9-ageDiff,0);
run;

data animal;
	set animal;
	ageDiff = age-ageonset;
	time_star = max(-3.9-ageDiff,0);
run;

data logI;
	set logI;
	ageDiff = age-ageonset;
	time_star = max(-3.1-ageDiff,0);
run;

data block;
	set block;
	ageDiff = age-ageonset;
	time_star = max(-3.6-ageDiff,0);
run;


*run mixed model;
proc mixed data=animal;
	class gender id; 
	model animals=ageonset time_star SES age gender / outp=predA s; 
	random intercept / subject=id;
run;

proc mixed data=block;
	class gender id; 
	model blockR=ageonset time_star SES age gender / outp=predB s; 
	random intercept / subject=id;
run;

proc mixed data=logI;
	class gender id; 
	model logmemI=ageonset time_star SES age gender / outp=predI s; 
	random intercept / subject=id;
run;

proc mixed data=logII;
	class gender id; 
	model logmemII=ageonset time_star SES age gender / outp=predII s; 
	random intercept / subject=id;
run;
