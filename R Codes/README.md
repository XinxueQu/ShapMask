## [R Codes](R%20Codes/)
The following R codes are required for loading the packages and custom functions needed for implementing the ShapMask framework.
* [`install_load_packages.R`](R%20Codes/install_load_packages.R/): installs and loads all the necessary R packages that are needed for implementing the ShapMask approach.
* [`custom_functions.R`](R%20Codes/custom_functions.R): contains the custom functions that carry out most of the tasks in the work.
* [`ShapMask_SingleConf.R`](R%20Codes/ShapMask_SingleConf.R): contains the ShapMask function for implementing the framework for a single confidential variable.
* [`ShapMask_MultiConf.R`](R%20Codes/ShapMask_MultiConf.R): contains the ShapMask function for implementing the framework for multiple confidential variables.
* [`ShapMask_simulated_data.R`](R%20Codes/ShapMask_simulated_data.R): contains the ShapMask function for implementing the framework for our simulated data set.
  
###	[1. Codes for Experiment](R%20Codes/Experiments/)
* [`CompuStat_replication.R`](R%20Codes/Experiments/CompuStat_replication.R): This file implements the proposed framework on CompuStat data set with 1000 replications and compares the results to approaches that mask only the confidential feature(s).
* [`Credit_replication.R`](R%20Codes/Experiments/Credit_replication.R): This file implements the proposed framework on the German Credit data set with 1000 replications and compares the results to approaches that mask only the confidential feature(s).
* [`Salary_Single_DV_replication.R`](R%20Codes/Experiments/Salary_Single_DV_replication.R): This file implements the proposed framework on Salary data with 1000 replications and compares the results to approaches that mask only the confidential feature(s). Here, it is assumed that the data has only one confidential feature.
* [`Salary_Multi_DV_replication.R`](R%20Codes/Experiments/Salary_Multi_DV_replication.R): This file implements the proposed framework on Salary data with 1000 replications and compares the results to approaches that mask only the confidential feature(s). Here, it is assumed that the data has two confidential features.

###	[2. Codes for Data Simulation](R%20Codes/Data%20Simulation/)
* [`Home_Credit_Data_Cleaning.R`](R%20Codes/Data%20Simulation/Home_Credit_Data_Cleaning.R): The original data comes in multiple files and therefore, this file reads in all the files, removes duplicate records, selects features of interest, and merges the data into a single file.
* [`Synthetic_Data_Simulation_Functions.R`](R%20Codes/Data%20Simulation/Synthetic_Data_Simulation_Functions.R): This file contains custom functions that are used to learn the statistical distribution of the features in the original data. 
* [`Synthetic_Data_Generation.R`](R%20Codes/Data%20Simulation/Synthetic_Data_Generation.R): This file uses the custom functions in the `Synthetic_Data_Simulation_Functions.R` to learn the statistical distribution of the features, generate a synthetic data set, and impose a relationship between the confidential feature, as well as the utility feature, with some of the other features in the data set.
* [`Synthetic_Data_replication.R`](R%20Codes/Data%20Simulation/Synthetic_Data_replication.R): This file implements the proposed framework on the synthetic data set with 1000 replications and compares the results to approaches that mask only the confidential feature(s).

### [2.3. Codes for Time Sensitivity Analysis](R%20Codes/Time%20Sensitivity%20Analysis/)
* [`Sensitivity_Analysis.R`](R%20Codes/Time%20Sensitivity%20Analysis/Sensitivity_Analysis.R): This performs sensitivity analyses for the framework using Home Credit data set to determine the time complexity base on the number observations and fearures in a data set.
