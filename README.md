# capstone
This repository contains source data, code, generated plots, and supporting documentation for my Introduction to Data Science capstone project, completed through Springboard. 

#### Summary:

The project covers the lifecycle of data science; beginning with data wrangling, then visual analysis, utilizing statistics to test hypotheses posed in the visual analysis section, and finally ending with machine learning. 

Specifically, we investigate what factors, if any, predict which Connecticut State Police traffic stops end in arrest. And, which, if any, of those factors are demographic, indicating the possible prevalence of police profiling. 

To this end, we tested multiple supervised machine learning classification models, ending up with a CART model that utilizes a Penalty Matrix to handle data imbalance. Demographic factors, such as gender and race were investigated more closely, particularly in the visual analysis section, where we uncovered some statistically significant differences in arrests/stops ratios between demographic groups. However, the final model was ultimately not a good performer, with a Kappa of 0.2849794 and an AUC value of 0.720. And in the end, it was determined that more data, ideally, more granualar data, is necessary to achieve a better performing predictor model. 

* Documentation:
  * capstone_proposal files contain an introduction to the project and associated data, the problem, and the methods for completing the project
  * exploratory_data_analysis_report files contains a summarizing report of the univariate visual analysis done in visualization.R. It contains code, plots, and analysis and explanation.
  * statistical_analysis_report files summarize the bivariate plot investigation and supporting statistical analysis. Code from statistics.R and visualization.R was used.
  * capstone_milestone_report is an extension of the capstone_proposal file, adding the visualization and statistical analysis components
  * machine_learning_report files summarize the supervised machine learning algorithms tested for the classification problem in machine_learning.R, including the final model used
  * capstone_report files contain the final report of the capstone project; summarizing the problem, client, data, data wrangling steps, visualization, statistics, machine learning, and conclusions
  * Capstone Slide Deck contains the PowerPoint slide deck presentation of the final project, written for a non-technical client audience

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
    
