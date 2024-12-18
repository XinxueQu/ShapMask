#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# READ ME ####
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# This is the file for generating the simulated Home Credit Data from the cleaned
# data (home_credit_data_clean.csv)
# Users can skip this code and use the simulated data that we generated from this file
# The original data can be found using: 
## https://www.kaggle.com/competitions/home-credit-credit-risk-model-stability/data
# The final simulated data can be found here:
## https://www.kaggle.com/datasets/shawnqu/data-for-shapmask?select=home_credit_synthetic_data_for_sensitivity.csv

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Installing and Loading Required Packages ####
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

source("../install_load_packages.R")

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Load Custom Functions ####
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

source("Synthetic_Data_Simulation_Functions.R")


#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Load Data ####
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

Data0 <- read_csv("home_credit_data_clean.csv")

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# Columns to be treated as continuous variables
cont_att_orig <- c("install_amt", "amtinstpaidbefduel24m_4187115A",
                   "maxannuity_4075009A", "maxdebt4_972A",
                   "maxinstallast24m_3658928A", "totinstallast1m_4525188A",
                   "age", "outstandingamount_362A", "debtvalue_227A")

# Columns to be log-transformed
convert_to_log <- c("debtoverdue_47A", "credamount_590A",
                    "avglnamtstart24m_4525187A", "lastrejectcredamount_222A",
                    "maxlnamtstart6m_4525199A", "price_1097A")

# Columns to be treated as regular poisson and zero-inflated poisson variables
zi_pois_att <- c("num_credit", "numactivecreds_622L", "numinstlsallpaid_934L",
                 "days30_165L", "actualdpd_943P", "numinstls_657L",
                 "numberofcontrsvalue_358L", "credquantity_984L",
                 "applicationscnt_867L", "maxdpdinstlnum_3546846P")

# Columns to be treated as categorical variables
categorical_atts <- c("postype_4733339M", "purposeofcred_426M",
                      "description_351M", "education_927M", "language1_981M",
                      "lastst_736L", "twobodfilling_608L", "marital_status")

# Variables that follow normal distribution
normal_var <- c("log_avglnamtstart24m_4525187A", "log_credamount_590A",
                "log_lastrejectcredamount_222A", "log_maxlnamtstart6m_4525199A",
                "log_price_1097A")

# Variables that follow lognormal distribution
lognormal_var <- c("install_amt", "amtinstpaidbefduel24m_4187115A", "maxannuity_4075009A",
                   "maxdebt4_972A", "maxinstallast24m_3658928A",
                   "totinstallast1m_4525188A", "log_debtoverdue_47A", "age",
                   "outstandingamount_362A", "debtvalue_227A")

#...............................................................................
##.... log transformed continuous non-mixture

Data <- Data0 %>%
  mutate(across(all_of(convert_to_log) , ~log(.x + 1), .names = "log_{.col}")) %>%
  dplyr::select(!all_of(convert_to_log))

# Extract the names of all log-transformed variables
cont_att_log <- grep(x = colnames(Data), pattern = "^log_", value = TRUE)

cont_nonmix_att <- c(cont_att_orig, cont_att_log)

# List of numerical variables
numerical_atts <- c(cont_nonmix_att, zi_pois_att)


#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Estimate the moments of continuous variables ####
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

## Minimums
min_vec <- Data %>% dplyr::select(any_of(numerical_atts)) %>% as.matrix %>% colMins(na.rm = TRUE)
## Maximums
max_vec <- Data %>% dplyr::select(any_of(numerical_atts)) %>% as.matrix %>% colMaxs(na.rm = TRUE)
## Means
mean_vec <- Data %>%  dplyr::select(any_of(numerical_atts)) %>% as.matrix %>% colMeans(na.rm = TRUE)
## Variance
var_vec <- Data %>% dplyr::select(all_of(numerical_atts)) %>% as.matrix %>% colVars(na.rm = TRUE)
## Skewness
skew_vec <- Data %>% dplyr::select(any_of(numerical_atts)) %>% apply(2, skew, type = 1)
## Standardized Kurtosis
skurt_vec <- Data %>% dplyr::select(any_of(numerical_atts)) %>% apply(2, kurtosi, type = 1)
## fifths cumulant
fifth_cmt_vec <- Data %>% dplyr::select(any_of(numerical_atts)) %>% apply(2, kth_cumulant, k = 5)
## sixths cumulant
sixth_cmt_vec <- Data %>% dplyr::select(any_of(numerical_atts)) %>% apply(2, kth_cumulant, k = 6)
## Proportion of zeros
p0_vec <- Data %>% dplyr::select(any_of(numerical_atts)) %>%
  apply(2, function(x){mean(x == 0, na.rm = TRUE)})

## Remove zeros and find mean
nonzero_mean_vec <- Data %>% dplyr::select(any_of(numerical_atts)) %>%
  apply(2, nonzero_mean)
## Remove zeros and find var
nonzero_var_vec <- Data %>% dplyr::select(any_of(numerical_atts)) %>%
  apply(2, nonzero_var)


## meanlog for lognormal distributions
meanlog_vec <- Data %>% dplyr::select(any_of(numerical_atts)) %>% apply(2, lnorm_mean)
## sdlog for lognormal distributions
sdlog_vec <- Data %>% dplyr::select(any_of(numerical_atts)) %>% apply(2, lnorm_sd)

## pstr0 for zip
pstr0_vec <- (var_vec - mean_vec)/(var_vec + mean_vec^2 - mean_vec)
pstr0_vec <- pstr0_vec[zi_pois_att]
pstr0_vec <- ifelse(pstr0_vec < 0, 0, pstr0_vec)
## lambda for zip
lambda_vec <- (var_vec + mean_vec^2)/mean_vec - 1
lambda_vec <- lambda_vec[zi_pois_att]
lambda_vec <- ifelse(pstr0_vec == 0, mean_vec[zi_pois_att], lambda_vec)


# strictly positive continuous variables
cont_strictly_pos <- sapply(cont_nonmix_att, function(x){
  z <- Data0[[x]]
  z <- z[!is.na(z)]
  all(z > 0)
}) %>%
  which(. == TRUE) %>%
  names()

#  continuous variables with some zero values
cont_with_zeros <- setdiff(cont_nonmix_att, cont_strictly_pos)

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Generate Synthetic Data (without covariances) ####
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# Generate all combinations of categorical variables
# and find the of each variable for each partition

cat_combn <- Data %>%
  group_by(across(all_of(categorical_atts))) %>%
  summarise(n = n(),
            across(any_of(c(normal_var, lognormal_var)), nonzero_mean),
            across(all_of(zi_pois_att), ~mean(.x, na.rm = TRUE))) %>%
  ungroup() %>%
  filter(rowSums(across(where(is.numeric), ~!is.na(.))) > 0) %>%
  mutate(p = n/sum(n)) %>%
  mutate(across(any_of(c(normal_var, lognormal_var)), ~ifelse(is.na(.x), nonzero_mean_vec[cur_column()], .x))) %>%
  mutate(across(any_of(c(normal_var, lognormal_var)), ~ifelse(.x == 0, nonzero_mean_vec[cur_column()], .x))) %>%
  mutate(across(any_of(zi_pois_att), ~ifelse(is.na(.x), mean_vec[cur_column()], .x))) %>%
  mutate(across(any_of(zi_pois_att), ~ifelse(.x == 0, mean_vec[cur_column()], .x)))

# Total number of observations to generate
total_n <- 2e5

# Setup for Parallel Computing
no_cores <- future::availableCores()
cl <- parallel::makeCluster(max(no_cores - 2, 1))
doParallel::registerDoParallel(cl)
#...............................................................................

synth_dt <- foreach(i = seq_len(nrow(cat_combn)), .combine = 'rbind', .packages = cran_pkgs) %dopar% {

  # sample size
  n <- round(total_n*cat_combn$p[i]) #+ rpois(1, 10)

  # Means of continuous numerical variables
  cat_mean_vec <- cat_combn[i, numerical_atts] %>% c(recursive = TRUE)
  cat_var_vec <- var_vec[numerical_atts]
  cat_meanlog_vec <- log(cat_mean_vec^2/sqrt(cat_mean_vec^2 + var_vec))
  cat_sdlog_vec <- sqrt(log(1 + cat_mean_vec^2/cat_var_vec))
  cat_lambda_vec <- (cat_var_vec + cat_mean_vec^2)/cat_mean_vec - 1
  cat_lambda_vec <- cat_lambda_vec[zi_pois_att]
  cat_lambda_vec <- ifelse(pstr0_vec == 0, cat_mean_vec[zi_pois_att], cat_lambda_vec)

  dt <- data.frame(matrix(NA, nrow = n, ncol = length(numerical_atts)))
  colnames(dt) <- numerical_atts

  # normally distributed variables
  for(nv in normal_var){
    dt[, nv] <- gen_x(n, dist = "normal",
                      params = list(mean = cat_mean_vec[nv],
                                    sd = sqrt(nonzero_var_vec[nv]),
                                    p0 = p0_vec[nv]))
  }

  # lognormally distributed variables
  for(lv in lognormal_var){
    dt[, lv] <- gen_x(n, dist = "lognormal",
                      params = list(meanlog = cat_meanlog_vec[lv],
                                    sdlog = sdlog_vec[lv],
                                    p0 = p0_vec[lv]))
  }

  # (zero-inflated) Poisson variables
  for(pv in zi_pois_att){
    dt[, pv] <- gen_x(n, dist = "zipois",
                      params = list(lambda = cat_lambda_vec[pv],
                                    pstr0 = pstr0_vec[pv]))
  }

  return(cbind(cat_combn[i, categorical_atts], dt))
}

# End and release all clusters
stopCluster(cl)


#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Create Covariances Between Variables ####
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# Confidential variable
conf_var <- "credamount_590A"

# Utility variable
pred_var <- "install_amt"


# List of variables that are correlated with the confidential variable(s)
cor_conf_var <- c("credquantity_984L", "maxdebt4_972A", "maxinstallast24m_3658928A")

# List of variables that are correlated with the utility variable(s)
cor_pred_var <- c("debtoverdue_47A", "age", "outstandingamount_362A", "debtvalue_227A", "price_1097A", conf_var, "maxdebt4_972A", "credquantity_984L")

# Search and replace variable names that have been log-transformed
conf_var <- grep(x = colnames(synth_dt),
                 pattern = paste(paste0(conf_var, "$"), collapse = "|"),
                 value = TRUE)
pred_var <- grep(x = colnames(synth_dt),
                 pattern = paste(paste0(pred_var, "$"), collapse = "|"),
                 value = TRUE)
cor_conf_var <- grep(x = colnames(synth_dt),
                     pattern = paste(paste0(cor_conf_var, "$"), collapse = "|"),
                     value = TRUE)
cor_pred_var <- grep(x = colnames(synth_dt),
                     pattern = paste(paste0(cor_pred_var, "$"), collapse = "|"),
                     value = TRUE)
#...............................................................................

# Target Correlation Matrix
set.seed(1234)

rho_matrix <- matrix(0,
                     nrow = length(numerical_atts),
                     ncol = length(numerical_atts),
                     dimnames = list(numerical_atts, numerical_atts))

rho_matrix[upper.tri(rho_matrix)] <-
  round(runif(0.5*length(numerical_atts)*(length(numerical_atts) - 1),
              -0.05, 0.05), 3)

cor_both_var <-  intersect(cor_conf_var, cor_pred_var)

## Correlations between confidential variable and its independent variables
rho_matrix[conf_var, cor_conf_var] <-
  round(runif(length(cor_conf_var), 0.4, 0.95), 3) * sample(c(-1,1), length(cor_conf_var), replace = TRUE)

## Correlations between utility variable and its independent variables
rho_matrix[pred_var, cor_pred_var] <-
  round(runif(length(cor_pred_var), 0.1, 0.5), 3) * sample(c(-1,1), length(cor_pred_var), replace = TRUE)

# Create a symmetric correlation matrix
rho_matrix <- t(rho_matrix) + rho_matrix
diag(rho_matrix) <- 1

## Check if the correlation matrix is symmetric
isSymmetric(rho_matrix)
## Check if the correlation matrix is singular
det(rho_matrix)
## Check if the correlation matrix is positive definitive
is.positive.definite(rho_matrix)

## Fix correlation matrix to be positive definite
if(!is.positive.definite(rho_matrix)){
  rho_matrix_pd <- nearPD(rho_matrix, corr = TRUE,
                          ensureSymmetry = TRUE,
                          eig.tol = 1e-02,
                          base.matrix = TRUE)$mat
}else{
  rho_matrix_pd <- rho_matrix
}

colnames(rho_matrix_pd) <- rownames(rho_matrix_pd) <- numerical_atts

sd_vec <- colSds(as.matrix(synth_dt[, numerical_atts]))
cov_matrix_pd <- cor2cov(rho_matrix, sd_vec)

synth_dt_new <- synth_dt
beta_conf <- solve(cov_matrix_pd[cor_conf_var, cor_conf_var]) %*%
  cov_matrix_pd[cor_conf_var, conf_var]

synth_dt_new[, conf_var] <- 1 + as.matrix(synth_dt[, cor_conf_var]) %*%
  as.matrix(beta_conf, ncol = 1) + rnorm(nrow(synth_dt_new), 0, 2)

beta_pred <- solve(cov_matrix_pd[cor_pred_var, cor_pred_var]) %*%
  cov_matrix_pd[cor_pred_var, pred_var]

synth_dt_new[, pred_var] <- 1 + as.matrix(synth_dt[, cor_pred_var]) %*%
  as.matrix(beta_pred, ncol = 1) + rnorm(nrow(synth_dt_new), 0, 1)


#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Save Synthetic Data
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

write_csv(Data, "home_credit_data_synthetic.csv")
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::