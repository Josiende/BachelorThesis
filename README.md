# BachelorThesis
Bachelor Thesis of Josien Demmers

In This README page the explanation and steps of the code are provided. 
First to replicate the results of Meinshausen (2006) the files Replication part.R and replicationPlots.R are needed.  Replication part.R has all steps to fit the models and calculate the losses. Then in replicationPlots.R the csv files of those losses are used to replicate the plots of Meinshausen (2006).

Next for the application of the quantile regression forest on macroeconomic data, the filles FredQD.R and AnalysisOverTime.R are used. The FredQD code file is the main file for this part. In there, one can find all models and forecasts for the variables GDP, Headline inflation, and Core inflation. AnalysisOverTime.R is used to calculate the performance measures during the Great Recession and the COVID-19 crises. Sfp.R has all code for the analysis of the Survey of Professional Forecasters. 

Lastly, in significance Diebold Mariano.R one can find the Diebold Mariano test used to determine if the RMSPE’s are significantly different. Sometimes, CSV files are used in the code. All necessary CSV files can be found in the repository with the same name as in the code files. 
