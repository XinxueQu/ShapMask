#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Installing and Loading Required Packages ####
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

source("../install_load_packages.R")

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Load Custom Functions ####
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

source("../custom_functions.R")
source("../ShapMask_simulated_data.R")

start <- Sys.time() # Start timer

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Load and Clean Dataset ####
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# Load the data set.
## Data is also available on Kaggle: https://www.kaggle.com/datasets/shawnqu/data-for-shapmask?select=home_credit_synthetic_data_for_sensitivity.csv
DataRaw <- read.csv("../../Data/home_credit_synthetic_data_for_sensitivity.csv")

numerical_atts   <- names(DataRaw)[sapply(DataRaw, is.numeric)] 
categorical_atts <- c("marital_status" , "postype_4733339M", "purposeofcred_426M", "education_927M" , "language1_981M")

# List of variables to be selected from the data set
sub_atts <- c(numerical_atts, categorical_atts)

# Select only numerical variables
Data <- DataRaw %>%  dplyr::select(all_of(sub_atts))

# Further data cleaning
# Convert currency and character variables to numeric
Data[, numerical_atts] <- Data[, numerical_atts] %>%
  mutate_if(is.character, parse_number, na = c("", "NA", "na", "$-"))

# Convert categorical variables into factors
Data[, categorical_atts] <- Data[, categorical_atts] %>%
  mutate_if(is.character, as.factor)

# Selecting all features
Data <- Data %>%  
  dplyr::select( all_of(numerical_atts)) %>%
  sample_n(10000)

# Delete missing observations
Data <- Data %>% drop_na()


#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Define the important features ####
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# Confidential variable
conf_var <- "log_credamount_590A"

# Utility variable
pred_var <- "install_amt"

# Name of confidential variable after masking
mask_var_name <- paste(conf_var, "Masked", sep = "_")

Data_Masked <- Data %>% mutate((!! mask_var_name) := (!! rlang::sym(conf_var)))
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
n_sim <- 1000

# Masking methods
adp_vec <- c("BCADP", "Laplace")
mdp_vec <- c("MNS2", "TwinUnif")
mask_vec <- c(adp_vec, mdp_vec)
# To reduce the computation time, select one masking method at a time
# mask_vec <- c(adp_vec, mdp_vec)[1]

# Types of inferential models
model_vec <- c("LM", "CART")

# Risk Metrics
metric_vec <- c("R2", "MAE", "RMSE")

# selection Criteria
sel_vec <- c("benchmark", "random", "risk", "wc")
w <- 0.75 # weight for weighted cost

# Determine if Calculation of Shapley values will be exact or approximate
SV_calc <- (ncol(Data) <= 14) # Exact if TRUE, approximated if FALSE


#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Simulation ####
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

## Prepare Parallel Computing ####
no_cores <- 12 # future::availableCores()

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