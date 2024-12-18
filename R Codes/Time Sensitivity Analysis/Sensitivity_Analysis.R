#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Installing and Loading Required Packages ####
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

source("../install_load_packages.R")

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Load Custom Functions ####
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

source("../custom_functions.R")
source("../ShapMask_SingleConf.R")

start <- Sys.time() # Start timer

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Load and Clean Dataset ####
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# Load the data set.
## Data is also available on Kaggle: https://www.kaggle.com/datasets/shawnqu/data-for-shapmask?select=home_credit_synthetic_data_for_sensitivity.csv
DataRaw <- readRDS("../../Data/home_credit_synthetic_data_for_sensitivity.rds")

# List of numerical variables
numerical_atts <- names(DataRaw)[sapply(DataRaw, is.numeric)]

# List of categorical variables
categorical_atts <- c("marital_status" , "postype_4733339M", "purposeofcred_426M", 
                      "education_927M" , "language1_981M")

# Confidential variable
conf_var <- "log_credamount_590A"

# Utility variable
pred_var <- "install_amt"

# List of variables that are correlated with the confidential variable(s)
cor_conf_var <- c("credquantity_984L", "maxdebt4_972A", "maxinstallast24m_3658928A",
                  "debtvalue_227A", "log_price_1097A")

# List of variables that are correlated with the utility variable(s)
cor_pred_var <- c("log_debtoverdue_47A", "age", "outstandingamount_362A", 
                  "debtvalue_227A", "log_price_1097A", conf_var, "maxdebt4_972A", 
                  "credquantity_984L")

Data <- DataRaw


# List of variables to be selected from the data set
sub_atts <- c(numerical_atts, categorical_atts)

# Further data cleaning
# Convert currency and character variables to numeric
Data[, numerical_atts] <- Data[, numerical_atts] %>%
  dplyr::mutate_if(is.character, parse_number, na = c("", "NA", "na", "$-"))

# Convert categorical variables into factors
Data[, categorical_atts] <- Data[, categorical_atts] %>%
  dplyr::mutate_if(is.character, as.factor)


# Select only numerical variables for fair comparison in sensitivity analysis.
Data <- Data %>%  dplyr::select(all_of(numerical_atts))

# Delete missing observations
Data <- Data %>% drop_na()


#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Sample the data for computing cost evaluations ####
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# number of observations
# We repeated the simulation using 10000, 20000, 30000
sample_size <- 10000

# number of features
# We repeated the simulation using 15, 20, 25
n_features <- 15

# must-have features that are needed for the simulation
features_must_have <- unique(c(conf_var, pred_var, cor_conf_var, cor_pred_var))

# randomly select features
sampled_vars <- sample(setdiff(names(Data), features_must_have), 
                       size = n_features - length(features_must_have))

sampled_vars <- c(sampled_vars, features_must_have)

# Select features and observations
Data <- Data[, sampled_vars] %>%
  dplyr::sample_n(sample_size)

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Define the important features ####
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# Name of confidential variable after masking
mask_var_name <- paste(conf_var, "Masked", sep = "_")

Data_Masked <- Data %>%
  mutate(across(all_of(conf_var), ~ .x, .names = "{.col}_Masked")) # this works for multiple conf-vars

Base_Data <- Data_Masked

# Risk and utility model formulas
risk_model_formula <- as.formula(paste("cbind(",  paste(conf_var, collapse= ","), ") ~ ."))  # for one or multiple confi-vars
utility_model_formula <- as.formula(paste(pred_var, "~ ."))


#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Set Simulation Parameters ####
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# Seed number
seed_number <- 1234

# Number of simulation runs
n_sim <- 100

# Masking methods
adp_vec <- c("BCADP", "Laplace")[1]
mdp_vec <- c("MNS2", "TwinUnif")[1]
mask_vec <- c(adp_vec, mdp_vec)
# To reduce the computation time, select one masking method at a time
# mask_vec <- c(adp_vec, mdp_vec)[1]

# Types of inferential models
model_vec <- c("LM", "CART")

# Risk Metrics
metric_vec <- "R2"

# selection Criteria
sel_vec <- c("benchmark", "random", "risk", "wc")
w <- 0.75 # weight for weighted cost (wc)

# Determine if Calculation of Shapley values will be exact or approximate
SV_calc <- (ncol(Data) <= 14) # Exact if TRUE, approximated if FALSE


#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Simulation ####
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

## Prepare Parallel Computing ####
no_cores <- 10 # future::availableCores()

start_time <- Sys.time()

results <-  distributr::gapply(ShapMask, 
                               mask = mask_vec, 
                               metric = metric_vec, 
                               model = model_vec,
                               sim = 1:n_sim, # simulation runs
                               .reps = 1, 
                               .mc.cores = no_cores) #'mc.cores' > 1 is not supported on Windows

Sys.time() - start_time


#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Summarizing Results ####
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

results2 <- results %>%
  dplyr::select(sim, mask, model, metric, sel, R_initial, R_final, U_initial, U_final, comp_time) %>%#
  distinct() %>%
  mutate(R_diff = R_initial - R_final, U_diff = U_initial - U_final) %>%
  mutate(R_diff_pc = R_diff/R_initial*100,
         U_diff_pc = U_diff/U_initial*100)

# Calculate the mean for each combination of masking method, model, metric, and feature selection
results_mean <- results2 %>%
  dplyr::select(!c(sim)) %>%
  mutate(mask = factor(mask, levels = mask_vec),
         model = factor(model, levels = model_vec),
         metric = factor(metric, levels = metric_vec),
         sel = factor(sel, levels = sel_vec)) %>%
  group_by(mask, model, metric, sel) %>%
  summarise_all(mean)

results_mean


#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

end <- Sys.time()
ymd_hms(end) - ymd_hms(start)

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::