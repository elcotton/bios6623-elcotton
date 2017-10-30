Advanced Data Analysis-Project 2
10/30/17
Eleanor Cotton
Software used: R version 3.4.2

Goal of this project:
	The goal of this project is to get an estimated death rate for 44 VA hospitals and
a 95% CI interval around this estimate. 


Data location:
C:\Users\cottonel\Documents\BIOS6623_AdvancedData\Project_Two


CODE FOLDER CONTENTS:
BIOS6623_Project1_CleaningandTableOne R file:
	This is the first file that should be run in this analysis. It cleans the data
provided by my instructors (file: vadata1.sas7bdat). It saves this cleaned data (cleanedData.csv)
to be used in the analysis file. It also generates Table One (summary of comorbidities) and 
generates the first part of Table Two that will also be used in the analysis file. It saves both table
one and table two in the reports file (tableOne.csv and tableTwo.csv, respectively). 

BIOS6623_Project_ModelBoot R file:
	This is the second file that should be run, after the above file. This file imports
the cleaned data from the above file and runs logistic regression on the data, to get an estimated
death rate for each individual. It also runs a bootstrap to get a 95% confidence interval around
this estimate. Finally, it makes several graphs of the results, and adds the results found to 
table two. It saves the final Table Two (tableTwoPlus.csv) in the reports file. It also saves
the results of the bootstrap (proportions.csv), so it does not need to be run again. 


DOC FOLDER CONTENTS:
BIOS6623_Project3_Overview.pdf:
	This file contains an overview of the project, as well as the codebook for the variables
in the original data set (file: vadata1.sas7bdat). 


REPORTS FOLDER CONTENTS:
BIOS6623_Project2_AnalysisReport.doc:
	This is the final analysis report for the project.

BIOS6623_Project2_InterimPresentation.pdf/powerpoint:
	Interim analysis report, necessary for inclass presentation.

Proportions, tableOne, tableTwo, tableTwoPlus csvs:
	Tables generated from the above R-code files.

BIOS6623_Project2_FinalPresentation.pdf/powerpoint:
	Final presentation given in class. 









