## Data
The experimental evaluations in the paper are based on four datasets: 
1. salary dataset, 
2. credit dataset, 
3. CRSP/Compustat dataset, and 
4. a simulated dataset. 
  
For the salary and credit datasets, we have provided both the original data sources and the versions we used, hosted on Kaggle.com. For the CRSP/Compustat data, which requires a subscription to the WRDS database, we have outlined the necessary steps to configure the query. For the simulated dataset, we provide both the raw data, from which we learned the distribution of the features, and the source code used for the simulation.

### 1. Salary Data
+ Original raw data was retrieved from:  https://sites.pitt.edu/~galletta/salsurv.html
+ A copy of the data is also saved on Kaggle: https://www.kaggle.com/datasets/shawnqu/data-for-shapmask?select=Salary_Data.csv 

### 2. Credit Dataset
+ Original raw data was retrieved from:  https://archive.ics.uci.edu/dataset/144/statlog+german+credit+data
+ A copy of the data is also saved on Kaggle: https://www.kaggle.com/datasets/shawnqu/data-for-shapmask?select=german_credit.data 

### 3. CRSP/compustat dataset
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

### 4. Simulated Dataset
* Original Dataset: https://www.kaggle.com/competitions/home-credit-credit-risk-model-stability
* Final simulated data is shared on Kaggle: https://www.kaggle.com/datasets/shawnqu/data-for-shapmask?select=home_credit_synthetic_data_for_sensitivity.csv 
* Code and steps for simulation
  1. Clean the data: read each of the individual files, select the features of interest, remove duplicate records, and merge all data sets into one data set.
  2. Generate synthetic data: learn statistical distribution of the features, establish statistical relationships between the confidential feature and certain other features, as well as between the utility feature and other related features. 

