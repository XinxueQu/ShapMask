
# ShapMask
This is a repository for the codes and datasets used in Qu, X., Bilson Darku, F., \& Guo, H. (xxxx) *Shapley-Value-Based Feature Attribution for Data Masking*.

## [1. Data](Data/)
The experimental evaluations in the paper are based on four datasets: 
1. salary dataset, 
2. credit dataset, 
3. CRSP/Compustat dataset, and 
4. a simulated dataset. 
  
For the salary and credit datasets, we have provided both the original data sources and the versions we used, hosted on Kaggle.com. For the CRSP/Compustat data, which requires a subscription to the WRDS database, we have outlined the necessary steps to configure the query. For the simulated dataset, we provide both the raw data, from which we learned the distribution of the features, and the source code used for the simulation.

### 1.1. Salary Data
+ Original raw data was retrieved from:  https://sites.pitt.edu/~galletta/salsurv.html
+ A copy of the data is also saved on Kaggle: https://www.kaggle.com/datasets/shawnqu/data-for-shapmask?select=Salary_Data.csv 

### 1.2. Credit Dataset
+ Original raw data was retrieved from:  https://archive.ics.uci.edu/dataset/144/statlog+german+credit+data
+ A copy of the data is also saved on Kaggle: https://www.kaggle.com/datasets/shawnqu/data-for-shapmask?select=german_credit.data 

### 1.3. CRSP/compustat dataset
The dataset is collected from [Wharton Research Data Services (WRDS)](https://wrds-www.wharton.upenn.edu/), which is not publicly available. Readers who are interested in this dataset can obtain access from their institution and download the data using the following settings. 

1. Data Year- Fiscal (`fyear`), 2000-01 to 2022-06
2. Use gvkey to "search the entire database"
   * Screening Variables: keep the default selection (i.e., Consolidation Level: C; Industry Format: INDL; Data Format: STD; Population Source: D; Currency: USD & CAD; Company Status: Active & Inactive; )
3. Linking Options. 
   * Link Types: LC, LU
   * Fiscal Period and Like Data Requirements: Fiscal period end date must be within link date range
4. Choose query variables:
   * Select Variable Types: Data Items, then select “All”
5. Select query output
   * Output Format: *csv; Compression Type: Uncompressed; Date Format: YYYY-MM-DD

### 1.4. Simulated Dataset
* Original Dataset: https://www.kaggle.com/competitions/home-credit-credit-risk-model-stability
* Final simulated data is shared on Kaggle: https://www.kaggle.com/datasets/shawnqu/data-for-shapmask?select=home_credit_synthetic_data_for_sensitivity.csv 
* Code and steps for simulation


## [2. R Codes](R%20Codes/)
The following R codes are required for loading the packages and custom functions needed for implementing the ShapMask framework.
* [`install_load_packages.R`](R%20Codes/install_load_packages.R/): installs and loads all the necessary R packages that are needed implementing the ShapMask approach.
* [`custom_functions.R`](R%20Codes/custom_functions.R): contains the custom functions that carry out most of the tasks in the work.
* [`ShapMask_SingleConf.R`](R%20Codes/ShapMask_SingleConf.R): contains the ShapMask function for implementing the framework for a single confidential variable.
* [`ShapMask_MultiConf.R`](R%20Codes/ShapMask_MultiConf.R): contains the ShapMask function for implementing the framework for multiple confidential variables.
* [`ShapMask_simulated_data.R`](R%20Codes/ShapMask_simulated_data.R): contains the ShapMask function for implementing the framework for for our simulated data set.
  
###	[2.1 Codes for Experiment](R%20Codes/Experiments/)
* [`CompuStat_replication.R`](R%20Codes/Experiments/CompuStat_replication.R): 
* [`Credit_replication.R`](R%20Codes/Experiments/Credit_replication.R):
* [`Salary_Single_DV_replication.R`](R%20Codes/Experiments/Salary_Single_DV_replication.R):
* [`Salary_Multi_DV_replication.R`](R%20Codes/Experiments/Salary_Multi_DV_replication.R):

###	[2.2. Codes for Data Simulation](R%20Codes/Data%20Simulation/)
* [`Home_Credit_Data_Cleaning.R`](R%20Codes/Data%20Simulation/Home_Credit_Data_Cleaning.R):
* [`Synthetic_Data_Simulation_Functions.R`](R%20Codes/Data%20Simulation/Synthetic_Data_Simulation_Functions.R):
* [`Synthetic_Data_Generation.R`](R%20Codes/Data%20Simulation/Synthetic_Data_Generation.R):
* [`Synthetic_Data_replication.R`](R%20Codes/Data%20Simulation/Synthetic_Data_replication.R):

### [2.3. Codes for Time Sensitivity Analysis](R%20Codes/Time%20Sensitivity%20Analysis/)
* `Sensitivity_Analysis.R`: This performs sensitivity analyses for the framework using Home Credit data set to determine the time complexity base on the number observations and fearures in a data set..


## 3. Tables and Graphs

### 3.1 Tables
All the outputs from experimental evaluations are shared on Kaggle: https://www.kaggle.com/datasets/shawnqu/shapmask-experiment-results. Based on these experiment results, we have also shared a notebook (in R) that consolidates experiment results and generates the main tables reported in the paper. The shared notebook can be accessed using the following link: https://www.kaggle.com/code/shawnqu/experiment-results-summary.

### [3.2 Graphs](Graphs/)
All graphs can be found and recreated using the Rmarkdown files in the Graphs folder. This folder contains three files, namely:
* [`Credit Card - Figure_4_6.RMD`](Graphs/Credit%20Card%20-%20Figure_4_6.Rmd): This creates figures 4 to 6 in the manuscript.
* [`Illustrative_Case_Credit_Fig7.RMD`](Graphs/Illustrative_Case_Credit_Fig7.Rmd): This creates figures 7 (a) and (b) in the manuscript.
* [`Illustrative_Case_Salary_Fig8.RMD`](Graphs/Illustrative_Case_Salary_Fig8.Rmd): This creates figures 8 (a) and (b) in the manuscript.
Each of the RMD files has a corresponding HTML created by knitting the RMD file.
