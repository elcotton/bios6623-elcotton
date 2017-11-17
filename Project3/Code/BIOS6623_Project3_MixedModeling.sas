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
	if ageDiff = . then ageDiff = 0;
	age_Adj = age-59;
	age_Cat = age_Adj;
	change_point = max(ageDiff+3,0);
run;

data animal;
	set animal;
	ageDiff = age-ageonset;
	if ageDiff = . then ageDiff = 0;
	age_Adj = age-59;
	age_Cat = age_Adj;
	change_point = max(ageDiff+3.9,0);
run;

data logI;
	set logI;
	ageDiff = age-ageonset;
	if ageDiff = . then ageDiff = 0;
	age_Adj = age-59;
	age_Cat = age_Adj;
	change_point = max(ageDiff+2.8,0);
run;

data block;
	set block;
	ageDiff = age-ageonset;
	if ageDiff = . then ageDiff = 0;
	age_Adj = age-59;
	age_Cat = age_Adj;
	change_point = max(ageDiff+3.9,0);
run;



*run mixed model on Animals;
*RI: AIC = 7908.5;
proc mixed data=animal;
	class gender id demind; 
	model animals = age_Adj demind age_Adj*demind change_point SES gender / outp=predA s; 
	random intercept / subject=id;
run;

*RI and RS-No Corr: AIC = 7909.9;
proc mixed data=animal;
	class gender id; 
	model animals = age_Adj demind age_Adj*demind change_point SES gender / outp=predA s; 
	random intercept age_Adj/ subject=id;
run;

*RI and RS-Corr: AIC = 7901.5;
proc mixed data=animal;
	class gender id demind; 
	model animals = age_Adj demind age_Adj*demind change_point SES gender / outp=predA s; 
	random intercept age_Adj/type = UN subject=id;
run;

*repeated, SP(POW): AIC = 8027.6;
proc mixed data=animal;
	class gender id age_Cat demind; 
	model animals = age_Adj demind age_Adj*demind change_point SES gender / outp=predA s; 
	repeated age_Cat/subject = id type = SP(POW)(age); 
run;

*RI and repeated SP(POW): AIC = 7881.3;
proc mixed data=animal;
	class gender id age_Cat demind; 
	model animals = age_Adj demind age_Adj*demind change_point SES gender / outp=predA s;
	random intercept/subject = id; 
	repeated age_Cat/subject = id type = SP(POW)(age); 
run;

*RI and repeated AR(1): AIC = 7910.3;
proc mixed data=animal;
	class gender id age_Cat demind; 
	model animals = age_Adj demind age_Adj*demind change_point SES gender / outp=predA s;
	random intercept/subject = id; 
	repeated age_Cat/subject = id type = AR(1); 
run;



*run mixed models on BlockR;
*RI: AIC = 9048.7;
proc mixed data=block;
	class gender id demind; 
	model blockR = age_Adj demind age_Adj*demind change_point SES gender/ outp=predB s; 
	random intercept / subject=id;
run;

*RI and RS-No Corr: AIC = 9044.6;
proc mixed data=block;
	class gender id demind; 
	model blockR = age_Adj demind age_Adj*demind change_point SES gender / outp=predA s; 
	random intercept age_Adj/ subject=id;
run;

*RI and RS-Corr: AIC = 8992.5;
proc mixed data=block;
	class gender id demind; 
	model blockR = age_Adj demind age_Adj*demind change_point SES gender / outp=predA s; 
	random intercept age_Adj/type = UN subject=id;
run;

*repeated, SP(POW): AIC = 9264.9;
proc mixed data=block;
	class gender id age_Cat demind; 
	model blockR = age_Adj demind age_Adj*demind change_point SES gender / outp=predA s; 
	repeated age_Cat/subject = id type = SP(POW)(age); 
run;

*RI and repeated SP(POW): AIC = 9023.9;
proc mixed data=block;
	class gender id age_Cat demind; 
	model blockR = age_Adj demind age_Adj*demind change_point SES gender / outp=predA s;
	random intercept/subject = id; 
	repeated age_Cat/subject = id type = SP(POW)(age); 
run;

*RI and repeated AR(1): AIC = 9049.5;
proc mixed data=block;
	class gender id age_Cat demind; 
	model blockR = age_Adj demind age_Adj*demind change_point SES gender / outp=predA s;
	random intercept/subject = id; 
	repeated age_Cat/subject = id type = AR(1); 
run;



*run mixed models on logI;
*RI: AIC = 7806.7;
proc mixed data=logI;
	class gender id demind; 
	model logmemI = age_Adj demind age_Adj*demind change_point SES gender/outp=predI s; 
	random intercept / subject=id;
run;

*RI and RS-No Corr: AIC = 7771.2;
proc mixed data=logI;
	class gender id demind; 
	model logmemI = age_Adj demind age_Adj*demind change_point SES gender / outp=predA s; 
	random intercept age_Adj/ subject=id;
run;

*RI and RS-Corr: AIC = 7714.1;
proc mixed data=logI;
	class gender id demind; 
	model logmemI = age_Adj demind age_Adj*demind change_point SES gender / outp=predA s; 
	random intercept age_Adj/type = UN subject=id;
run;

*repeated, SP(POW): AIC = 7811.3;
proc mixed data=logI;
	class gender id age_Cat demind; 
	model logmemI = age_Adj demind age_Adj*demind change_point SES gender / outp=predA s; 
	repeated age_Cat/subject = id type = SP(POW)(age); 
run;

*RI and repeated SP(POW): AIC = 7699.8;
proc mixed data=logI;
	class gender id age_Cat demind; 
	model logmemI = age_Adj demind age_Adj*demind change_point SES gender / outp=predA s;
	random intercept/subject = id; 
	repeated age_Cat/subject = id type = SP(POW)(age); 
run;

*RI and repeated AR(1): AIC = 7692.7;
proc mixed data=logI;
	class gender id age_Cat demind; 
	model logmemI = age_Adj demind age_Adj*demind change_point SES gender / outp=predA s;
	random intercept/subject = id; 
	repeated age_Cat/subject = id type = AR(1); 
run;



*run mixed models-logII;
*RI: AIC = 8049.9;
proc mixed data=logII;
	class gender id demind; 
	model logmemII = age_Adj demind age_Adj*demind change_point SES gender/ outp=predII s; 
	random intercept / subject=id;
run;

*RI and RS-No Corr: AIC = 7999.0;
proc mixed data=logII;
	class gender id demind; 
	model logmemII = age_Adj demind age_Adj*demind change_point SES gender / outp=predA s; 
	random intercept age_Adj/ subject=id;
run;

*RI and RS-Corr: AIC = 7920.1;
proc mixed data=logII;
	class gender id demind; 
	model logmemII = age_Adj demind age_Adj*demind change_point SES gender / outp=predA s; 
	random intercept age_Adj/type = UN subject=id;
run;

*repeated, SP(POW): AIC = 7988.8;
proc mixed data=logII;
	class gender id age_Cat demind; 
	model logmemII = age_Adj demind age_Adj*demind change_point SES gender / outp=predA s; 
	repeated age_Cat/subject = id type = SP(POW)(age); 
run;

*RI and repeated SP(POW): AIC = 7904.1;
proc mixed data=logII;
	class gender id age_Cat demind; 
	model logmemII = age_Adj demind age_Adj*demind change_point SES gender / outp=predA s;
	random intercept/subject = id; 
	repeated age_Cat/subject = id type = SP(POW)(age); 
run;

*RI and repeated AR(1): AIC = 8049.8;
proc mixed data=logII;
	class gender id age_Cat demind; 
	model logmemII = age_Adj demind age_Adj*demind change_point SES gender / outp=predA s;
	random intercept/subject = id; 
	repeated age_Cat/subject = id type = AR(1); 
run;

