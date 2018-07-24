# capstone
This repository contains source data, code, generated plots, and supporting documentation for my Introduction to Data Science capstone project, completed through Springboard. 
* Documentation:
  * capstone_proposal files contain an introduction to the project and associated data, the problem, and the methods for completing the project
  * exploratory_data_analysis_report files contains a summarizing report of the univariate visual analysis done in visualization.R. It contains code, plots, and analysis and explanation.
  * statistical_analysis_report files summarize the bivariate plot investigation and supporting statistical analysis. Code from statistics.R and visualization.R was used.
  * capstone_milestone_report is an extension of the capstone_proposal file, adding the visualization and statistical analysis components.

*	CSV files: 
    * CT_cleaned.csv is my original dataset containing State Police traffic stop information
    * CT_cleaned_edit.csv one of the two dataset outputs from the capstone_data_wrangling.R code
    * CT_cleaned_split.csv is the other of the two dataset outputs from the capstone_data_wrangling.R code
    * cc-est2016-alldata-09.csv contains Census population data
    * 2014_Daily_Vehicle_Miles_Travelled_By_Town_And_Roadway_Classification.csv contains 2014 traffic volume for Connecticut
  
*	R code files:
    * capstone_data_wrangling.R contains all the data wrangling code of the original dataset mainly using tidyr and dyplr
    *	visualization.R contains all the R code using for exploratory visualization, largely relying on ggplot
    *	statistics.R contains code for running statistical tests on trends identified in visualization.R
    * machine_learning.R contains the machine learning algorithm code used to find the best model for predicting arrest status outcome
    
*	Image folders:
    *	EDA_images contains the corresponding images to visualization.R
    *	stats_images contains the corresponding images to statistics.R
    
