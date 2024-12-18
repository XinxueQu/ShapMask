#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Installing and Loading Required Packages ####
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

source("../install_load_packages.R")

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Load Custom Functions ####
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

source("../custom_functions.R")
source("../ShapMask_MultiConf.R")

start <- Sys.time() # Start timer

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Load and Clean Dataset ####
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# Load the data set.
## Data is also available on Kaggle: https://www.kaggle.com/datasets/shawnqu/data-for-shapmask?select=Salary_Data.csv
Data <- read_csv("../../Data/Salary_Data.csv")

# Convert currency and character variables to numeric
# List of numerical variables
numerical_atts <- c("Yrs_exp", "Top_J_Pubs", "Ref_J_Pubs", "Textbooks",
                    "Research_Books", "Other_Pubs", "Salary", "Summer", 
                    "summer_yrs", "res_budget", "move_supp", 
                    "Tenure_Req_Lvl", "Tenure_Req_A", "Tenure_Req_tot")

# List of categorical variables
categorical_atts <- c("Year", "Education", "Position", "pub_prvt", "schl_deg", 
                      "tch_load", "accredit")

# List of variables to be selected from the data set
sub_atts <- c(numerical_atts, categorical_atts)

# Select only numerical variables
Data <- Data %>%  dplyr::select(all_of(sub_atts))

# Further data cleaning
# Convert currency and character variables to numeric
Data[, numerical_atts] <- Data[, numerical_atts] %>% 
  mutate_if(is.character, parse_number, na = c("", "NA", "na", "$-"))

# Convert categorical variables into factors
Data[, categorical_atts] <- Data[, categorical_atts] %>% 
  mutate_if(is.character, as.factor)

# Next only use selected continuous variables
Data <-  Data[, numerical_atts]  %>%  #
  dplyr::select(-Research_Books, -Textbooks) 

# Delete missing observations
Data <- Data %>% drop_na()

# For certain features, the minimum value is 0, cannot work with MNS2 method (Log(X) to generate variance)
Data <- Data %>% mutate(Yrs_exp = Yrs_exp + 1,
               Top_J_Pubs = Top_J_Pubs + 1,
               Ref_J_Pubs = Ref_J_Pubs + 1,
               Other_Pubs = Other_Pubs + 1,
               Tenure_Req_A = Tenure_Req_A + 1,
               Tenure_Req_tot = Tenure_Req_tot + 1)


#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Define the important features ####
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# Confidential variable
conf_var <- c("Salary", "Summer")

# Utility variable
pred_var <- "res_budget"

# Name of confidential variable after masking
mask_var_name <- paste(conf_var, "Masked", sep = "_")

Data_Masked <- Data %>% mutate(across(all_of(conf_var), ~ .x, .names = "{.col}_Masked")) # this works for multiple conf-vars

Base_Data <- Data_Masked

# Risk and utility model formulas
risk_model_formula <- as.formula(paste("cbind(",  paste(conf_var, collapse= ","), ") ~ ."))  # for one or multiple confi-vars
utility_model_formula <- as.formula(paste(pred_var, "~ ."))


#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Set Simulation Parameters  ####
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
metric_vec <- "R2"

# selection Criteria
sel_vec <- c("benchmark", "random", "risk", "wc")
w <- 0.75 # weight for weighted cost

# Determine if Calculation of Shapley values will be exact or approximate
SV_calc <- (ncol(Data) <= 16) # Exact if TRUE, approximated if FALSE


#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Simulation ####
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

## Prepare Parallel Computing ####
no_cores <- 16 # future::availableCores()

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
  dplyr::select(sim, mask, model, metric, sel, R_initial, R_final, U_initial, U_final, comp_time) %>%
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