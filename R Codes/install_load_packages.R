#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Installing and Loading Required Packages ####
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

if(!require(pacman)) {
  install.packages("pacman", dependencies = TRUE, repos = "http://cran.us.r-project.org")
  library(pacman)
}

# List of packages
cran_pkgs <- c("MASS", "matrixStats"
               , "doParallel" #
               , "future"
               , "tidyverse" # R packages for data science
               , "magrittr"
               , "CCA"       # Canonical correlation analysis
               , "rpart"     # Regression tress
               , "gam"       # Generalized additive models
               , "lmSubsets" # Exact variable-subset selection in regression
               , "tmvtnorm"  # Truncated multivariate normal and t dist.
               , "compositions" # for multivariate log-normal
               , "RcppAlgos" # High performance combinatorics
               , "abind"     # Combine multi-dimensional arrays
               , "ggrepel"
               , "lubridate"
               , "iml"
               , "remotes"   # R Package Installation from Remote Repositories, Including 'GitHub'
               , "randomForest"
               , "xgboost"
               , "DescTools"
               , "caret"
               , "janitor"
               , "Matrix"
               , "matrixcalc"
               , "synthpop"
               , "psych"
               , "VGAM"
               , "fitdistrplus"
               , "sdcMicro"  # Statistical disclosure control methods
)

# Install/update and load CRAN packages
pacman::p_load(char = cran_pkgs, install = TRUE, update = FALSE, character.only = TRUE)


# List of Github packages
github_pkgs <- c("patr1ckm/distributr")
pacman::p_load_gh(char = github_pkgs, install = TRUE, update = FALSE)

# Suppress message from `summarise()`
options(dplyr.summarise.inform = FALSE)
