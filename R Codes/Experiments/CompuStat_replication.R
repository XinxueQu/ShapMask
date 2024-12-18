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

# The exact data cannot be shared but can be obtained from  Wharton Research Data Services (WRDS)
# Please follow section 1.3 of README page on https://github.com/XinxueQu/ShapMask
# for the settings to obtain the data from WRDS
compustat <- read.cs("CRSP-Compustat Merged - Fundamentals Annual.csv")

# select data for 2021
compustat_2021 <- subset(compustat, fyear == 2021)

## Remove columns with more than 10% NA or 90% valid values
compustat_2021 <- compustat_2021[, which(colMeans(!is.na(compustat_2021)) > 0.9)] 

# remove firms that appear multiple times
compustat_2021 <- compustat_2021 %>%
  filter( !(GVKEY %in% subset(data.frame(table(compustat_2021$GVKEY)), Freq > 1)$Var1 )) 

# remove all integer values which could represent some industry code (but keep gvkey as id)
compustat_2021 <- compustat_2021[, !sapply(compustat_2021, is.integer)] 

# remove columns that are character
compustat_2021 <- compustat_2021[, !sapply(compustat_2021, is.character)]

Data <- compustat_2021 

# remove any row with NAs
Data <- Data[complete.cases(Data), ] 

# remove variable of no variance
Data <- Data[,- caret::nearZeroVar(Data)]

# create correlation matrix
cor_matrix <- cor(Data)

# Set correlation threshold
corThreshold <- 0.9 # 0.5 leaves 49 features; 06 leaves 56 features

# Find highly correlated variables
highlyCorrelated <- which(abs(cor_matrix) > corThreshold & upper.tri(cor_matrix, diag = FALSE), arr.ind = TRUE)

# Print the highly correlated variables
cor_summary <- data.frame(Row = rownames(cor_matrix)[highlyCorrelated[, 1]],  
                          Column = colnames(cor_matrix)[highlyCorrelated[, 2]],
                          Correlation = cor_matrix[highlyCorrelated]) %>%
  mutate(abs_cor = abs(Correlation)) %>% 
  arrange(desc(abs_cor))


# remove high correlated variables from the data
Data <- Data[, !(names(Data) %in% unique(cor_summary$Column))]

# Sampling firms with to remove extreme values and outliers
Data <- Data %>% 
  mutate(across(everything(), ~Winsorize(.x, quantile(.x, probs = c(0.02, 0.98)) ) ) )

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Load and Clean Dataset ####
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# List of numerical variables
numerical_atts <- names(Data)

# List of categorical variables
categorical_atts <- c()

# List of variables to be selected from the data set
sub_atts <- c(numerical_atts, categorical_atts)

# Select only numerical variables
Data <- Data %>% dplyr::select(all_of(sub_atts))

# Further data cleaning
# Convert currency and character variables to numeric
Data[, numerical_atts] <- Data[, numerical_atts] %>% 
  mutate_if(is.character, parse_number, na = c("", "NA", "na", "$-"))

# Convert categorical variables into factors
Data[, categorical_atts] <- Data[, categorical_atts] %>% 
  mutate_if(is.character, as.factor)

# Delete missing observations
Data <- Data %>% drop_na()


#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Define the important features ####
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

sampled_vars <- sample(names(Data), 3)

# Confidential variable
conf_var <- "ci"# CI: comprehensive income

# Utility variable
pred_var <- "prcc_c"

# Name of confidential variable after masking
mask_var_name <- paste(conf_var, "Masked", sep = "_")

Data_Masked <- Data %>% mutate(across(all_of(conf_var), ~ .x, .names = "{.col}_Masked")) # this works for multiple conf-vars

Base_Data <- Data_Masked

# Risk and utility model formulas
risk_model_formula <- as.formula(paste("cbind(",  paste(conf_var, collapse= ","), ") ~ ."))  # for one or multiple confi-vars
utility_model_formula <- as.formula(paste(pred_var, "~ ."))


#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Set Simulation Parameters ####
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# Number of simulation runs
n_sim <- 1000

# Masking methods
## Select the masking methods you are interested in
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
SV_calc <- (ncol(Data) <= 16) # Exact if TRUE, approximated if FALSE

# Seed number
seed_number <- 1234

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Simulation ####
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

## Prepare Parallel Computing ####
no_cores <- 11 # future::availableCores()

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