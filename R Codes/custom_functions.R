#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Version Control and Updates ####
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# 2023-07-10 (v6)
# No frontier curve (frontier = "none") option was added to the fontier_curve() function

#...............................................................................
# 2023-07-10 (v5)
# Shapley.Value.Est2(): A new function to estimate Shapley values for large data sets
## Okhrati and Aldo Lipani, 2020
# Shapley.Value.Coalition() : A new function to calculate Shapley values for all possible coalitions
# MNS2: log of non-positive values issue were handled
# Shapley.Value.Est(): Stage 1 sampling issues that were resulting in NA variances were fixed

#...............................................................................
# 2023-07-10 (v4)
# Random forest model (model = "RF") was added to model_metric()
# XGBoost model (model = "XGB") was added to model_metric()
# GADP masking method can handle non-invertible matrices
# Laplace masking method has been added to ADP() masking function ( i.e. method = "Laplace")
# Twin Uniform multiplicative noise masking method (TwinUnif) has been added to MDP(), i.e. method = "TwinUnif"

#...............................................................................
# 2023-06-26 (v3)
# Delta() was updated to use model_metric().
# Now all model evaluations are handled by model_metric().

#...............................................................................
# 2023-06-26 (v2)
# Shapley.Value() is upgraded to handle the new update from model_metric() and all.subsets.model() functions
# all.subsets.model() was updated to use model_metric() instead of duplicating functions

#...............................................................................
# 2023-06-14 (v1)
## Delta() and model_metric() functions was upgraded to include models with multiple confidential attributes
## Shapley.Value.Est(), Delta(), model_metric() functions accepts "CCA" as argument for "metric"

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Perturbation Techniques ####
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

#...............................................................................
## Additive Data Perturbation (ADP) Methods (Muralidhar et al, 1999) ####
#...............................................................................
ADP <- function(X, S = NULL, d = 1, Sigma_e = NULL,
                method = c("SADP", "CADP", "BCADP", "GADP", "Laplace")) {

  # X: confidential attribute
  # S: Non-confidential attributes
  # d: Magnitude of noise
  # Sigma_e: Specify the variance/covariance structure of the noise distribution
  # method: "SADP", "CADP", "BCADP", "GADP"

  method <- match.arg(method)

  X <- as.matrix(X)
  if (!is.null(S)) {S <- as.matrix(S)}

  n <- nrow(X)
  p <- ncol(X)

  Sigma_XX <- var(X)

  if(ncol(Sigma_XX)>1){
    D <- diag(diag(Sigma_XX))
  }else{
    D <- Sigma_XX
  }


  if (method == "SADP") {
    # Simple Additive Data Perturbation (ADP) Methods (Muralidhar et al, 1999)
    # use provide variance or variance from the feature itself, doesn't account for correlations between confi-variables
    if (is.null(Sigma_e)) {
      Sigma_e <- D
    } else {
      Sigma_e <- diag(diag(Sigma_e))
    }
    e <- MASS::mvrnorm(n = n, mu = numeric(p), Sigma = d*Sigma_e)
    Y <- X + e

  } else if (method == "CADP") {
    # doesn't correct for bias in variance; account for correlation between confi-variables. (if only one confi-vaiable, SADP=CADP)
    if (is.null(Sigma_e)) {Sigma_e <- Sigma_XX}
    e <- MASS::mvrnorm(n = n, mu = numeric(p), Sigma = d*Sigma_e)
    Y <- X + e

  } else if (method == "BCADP") {
    ## Bias-Corrected Additive Data Perturbation (BCADP) Methods
    ## (Muralidhar et al, 1999; Kim, 1986; Tendick and Matloff, 1994)

    d1 <- sqrt(1 + d)
    d2 <- d1 - 1
    mu_X <- colMeans(X)
    Sigma_e <- d*Sigma_XX
    e <- MASS::mvrnorm(n = n, mu = numeric(p), Sigma = Sigma_e)
    Y <- (X + e)/d1 + mu_X*d2/d1

  } else if (method == "GADP") {
    # General Additive Data Perturbation (ADP) Methods (Muralidhar et al, 1999)

    if(!require(CCA)) {install.packages("CCA")}

    # theta <- max(CCA::cc(X, S)$cor)

    U <- cbind(X, S)

    mu_X <- as.matrix(colMeans(X))
    mu_U <- as.matrix(colMeans(U))

    Sigma_YY <- Sigma_XX
    Sigma_YS <- t(cov(S, X)) # Sigma_SX
    Sigma_YX <- d*Sigma_XX # Sigma_YX = (theta^2)*Sigma_XX for maximum security
    Sigma_UU <- var(U)
    Sigma_YU <- cbind(Sigma_YX, Sigma_YS)

    Y <- t(apply(U, 1, FUN = function(c_i) {

      # Force covariance matrix to be positive definitive
      if (det(Sigma_UU) == 0){Sigma_UU <- Matrix::nearPD(Sigma_UU)}

      mu_Y <- mu_X + Sigma_YU %*% solve(Sigma_UU) %*% (c_i - mu_U)
      Sigma_Y <- Sigma_YY - Sigma_YU %*% solve(Sigma_UU) %*% t(Sigma_YU)

      # Force covariance matrix to be positive definitive
      if (det(Sigma_Y) == 0) {Sigma_Y <- Matrix::nearPD(Sigma_Y)$mat}

      y <- MASS::mvrnorm(n = 1, mu = mu_Y, Sigma = Sigma_Y)
      return(y)

    }))


  } else if (method == "Laplace") {
    # use provide variance or variance from the feature itself, doesn't account for correlations between confi-variables
    if (is.null(Sigma_e)) {
      Sigma_e <- D
    } else {
      Sigma_e <- diag(diag(Sigma_e))
    }

    if(!require(rmutil)) {install.packages("rmutil")}
    e <- rmutil::rlaplace(n, 0, d*sqrt(Sigma_e) ) # replaced "d*Sigma_e" on 07/24/2023
    Y <- X + e
  }


  colnames(Y) <- paste(colnames(X), "Masked", sep = "_")
  return(Y)
}


#...............................................................................
## Multiplicative  Data Perturbation (MDP) Methods ####
#...............................................................................
MDP <- function(X, d = 1, method = c("MNS1", "MNS2", "LN", "TwinUnif"), ...) {

  # X: confidential attribute
  # d: Magnitude of noise (between 0 and 1)
  # ...: several inputs depending on method

  #Extra parameters that can be supplied when using method=MNS1
  # mu_e: Mean vector for the noise
  # Sigma_e: # Covariance matrix for the noise
  # lower: # Lower bound for the noise
  # upper: # Upper bound for the noise
  # mu:  center of TwinUnif distribution
  # a_min: value for defining the inner bounds of the TwinUnif Distribution
  # a_max: value for defining the outer bounds of the TwinUnif Distribution
  # a : constant value added to values to make them positive for multiplactive masking

  method <- match.arg(method)
  ex.args <- list(...)
  #ex.args <- eval(substitute(alist(...)))

  X <- as.matrix(X)

  n <- nrow(X)
  p <- ncol(X)



  if (method == "MNS1") {
    # Multiplicative Noise Scheme I, Kim & Winkler (2003)
    if(!require(tmvtnorm)) {install.packages("tmvtnorm")}

    # Mean vector for the noise
    if ("mu_e" %in% names(ex.args)){
      mu_e <- ex.args$mu_e
    }else{
      mu_e <- rep(0, p)
    }

    # Covariance matrix for the noise
    if ("Sigma_e" %in% names(ex.args)){
      Sigma_e <- ex.args$Sigma_e
    }else{
      Sigma_e <- diag(p)
    }

    # Lower bound for the noise
    if ("lower" %in% names(ex.args)){
      lower <- ex.args$lower
    }else{
      lower <- rep(-Inf, p)
    }

    # Upper bound for the noise
    if ("upper" %in% names(ex.args)){
      upper <- ex.args$upper
    }else{
      upper <- rep(Inf, p)
    }


    e <- tmvtnorm::rtmvnorm(n, mean = mu_e, sigma = d*Sigma_e,
                            lower = lower, upper = upper)
    Y <- X*e

  } else if (method == "MNS2") {

    # Multiplicative Noise Scheme II, Kim & Winkler (2003)
    # lnX <- log(X)
    if (any(X <= 0)) {
      a <- abs(min(X[X != 0]))*1.01
      lnX <- log(X + a)
    } else {
      lnX <- log(X)
    }

    e <- MASS::mvrnorm(n = n, mu = numeric(p), Sigma = d*var(lnX))
    Y <- X*exp(e)

  } else if (method == "LN") {

    # Lognormal Multiplicative Noise, Muralidhar et al (1995)

    if(!require(compositions)) {install.packages("compositions")}

    e <- compositions::rlnorm.rplus(n, meanlog = rep(0, p),
                                    varlog = d*diag(0.25^2, p))
    Y <- X*as.numeric(e)

  } else if (method == "TwinUnif") {

    # Twin Uniform Multiplicative Noise, Bracknery et al 2020
    ## On masking and releasing smart meter data at micro-level:
    ## the multiplicative noise approach

    if ("d_max" %in% names(ex.args)) {d_max <- ex.args$d_max}

    # Parameters for the twin uniform distribution
    # Mean for the noise
    if ("mu" %in% names(ex.args)){
      mu <- ex.args$mu
    }else{
      # mu <- rep(20, p) # 20 was a guess
      mu <- d * sd(X) #(1+d/d_max) * mean(X, na.rm=TRUE) #20
    }

    # Lower bound for the noise
    if ("a_min" %in% names(ex.args)){
      a_min <- ex.args$a_min
    }else{
      # a_min <- rep(0.1, p)
      a_min <- 0.6 * d/(d_max + 0.001)
    }

    # Upper bound for the noise
    if ("a_max" %in% names(ex.args)){
      a_max <- ex.args$a_max
    }else{
      # a_max <- rep(d, p)
      a_max <- 1 *d/(d_max + 0.001)
    }

    # Shifting parameter for noise
    if ("a" %in% names(ex.args)){
      a <- ex.args$a
    }else{
      # Automatically determining the shifting parameter so that zero values will also be masked
      x_min <- min(X)
      if (x_min < 0) {
        a <- abs(x_min) + min(abs(X[X != 0]))/2
      } else if (x_min == 0) {
        a <- min(X[X != 0])/2
      } else {
        a <- 0
      }
      # a <- rep(a, p)
    }

    # Twin uniform distribution (noise)
    e1 <- runif(n, mu*(1 - a_max), mu*(1 - a_min))
    e2 <- runif(n, mu*(1 + a_min), mu*(1 + a_max))
    w <- rbinom(n, 1, 0.5)
    e <- e1*w + e2*(1 - w)

    # Data masking
    Y <- (X + a)*e/mu - a
  }

  colnames(Y) <- paste(colnames(X), "Masked", sep = "_")
  return(Y)

}


#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Measure of Security/Disclosure ####
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

#...............................................................................
## Canonical Correlation Approach (Muralidhar et al, 1999) ####
#...............................................................................
# Maximum Eigen Value:
# Maximum proportion of variance that a snooper can explain for any linear
# combination of unknown (confidential) attributes, using a linear combination
# of known attributes (Johnson and Wichern 1992)
data_disclosure_risk_cc <- function(X, Y) {

  # X: a data frame of confidential attributes
  # Y: a data frame of (masked and) other attributes

  # This checks and installs the Canonical Correlation Analysis package
  if(!require(CCA)) {
    install.packages("CCA")
    library(CCA)
  }

  if (!is.data.frame(X)) {X = data.frame(X1 = X)}
  if (!is.data.frame(X)) {Y = data.frame(Y1 = Y)}
  if (nrow(X) != nrow(Y)) {
    stop("Number of observations in the two data sets must be equal")
  }

  max_eigen <- max(CCA::cc(X, Y)$cor)^2

  return(max_eigen)
}


# library(iml) # Interpretable Machine Learning package ( for computing Shapley value)



#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#  Given a vector of non-confidential feature (names), generate all possible combinations of these features
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Input: a vector with all names of attributes
# Output: a list of all possible combinations of attributes (each element is a set of attribtues with different length )
Attri_set_Gen <- function(attribute_candidate){
  feature_combn <- list()

  for(num in 1:length(attribute_candidate)){
    tempt_combn <- list(combn(attribute_candidate, num,simplify = FALSE))
    feature_combn=append(feature_combn,tempt_combn)
  }

  return(feature_combn)
  # return a list, each element is a set of attributes with different length
}


#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Attribute Sets Sampling ####
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Input: a list of attribute sets, sampling method
# Output: a list of sampled attribute sets

Heuristic_Sample <- function(feature_combo, m){
  # set.seed(4321)
  #feature_combo= feature_comb
  feasture_sets <- list()
  for(i in 1:length(feature_combo)){
    feasture_sets <- c(feasture_sets, feature_combo[[i]])
  }
  if(m>length(feasture_sets)){
    sampled_attribute_comb <- feasture_sets
  }else{
    sampled_attribute_comb <- sample(feasture_sets, m, replace=FALSE)
  }

  return(sampled_attribute_comb)
}


Weighted_Sample <- function(feature_combo, m){
  weights <- c()
  feature_combo_size <-c()
  for(i in 1:length(feature_combo)){
    feature_combo_size <- c(feature_combo_size,length(feature_combo[[i]]))
  }
  weights <- feature_combo_size/sum(feature_combo_size)

  sampled_attribute_comb <- list()

  if(m>sum(feature_combo_size)){
    # when sample size m is larger than the population, take all feature combination
    for(i in 1:length(feature_combo)){
      sampled_attribute_comb <- c(sampled_attribute_comb,feature_combo[[i]])
    }
  } else{
    #set.seed(1234)
    for(i in 1:m){
      feature_set_length <- sample(c(1:length(feature_combo)),1,prob = weights)
      sampled_attribute_comb <- c(sampled_attribute_comb, sample(feature_combo[[feature_set_length]],1,replace=FALSE))
    }
  }

  return (sampled_attribute_comb)
}


#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Main function for SV attribution ####
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
Shapley_Value <- function(Data, features, attribute_of_interest,
                          sample_method = "Heuristic", m,
                          model = "linear", evaluation ="r-square"){

  candidate_attributes <- setdiff(features,attribute_of_interest)

  SV_non_confi <- c()

  for(attribute_iterate in candidate_attributes){
    #cat(attribute_iterate,"\n")
    other_attributes <- setdiff(candidate_attributes,attribute_iterate)

    # create all possible combination of attributes (which does not include confidential attribute(s) or the attribute of interest)
    feature_combo <- Attri_set_Gen(other_attributes)
    feature_combo <- c(feature_combo,1)
    # Exact shapley value calcuation is almost impossible
    # generate a sample of attribute combinations

    if (sample_method=="Weighted") {

      sampled_attribute_comb <- Weighted_Sample(feature_combo, m)

    } else if (sample_method=="Heuristic"){ # if not specified or incorrectly specified, use sample_method=="Heuristic"

      sampled_attribute_comb <- Heuristic_Sample(feature_combo, m)

    }

    # parallel computing to calculate marginal effect on sampled feature combinations

    if(model=="cart"){
      if(!require(rpart)) {install.packages("rpart"); library(rpart)}
    } else if(model=="gam"){
      if(!require(gam)) {install.packages("gam"); library(gam)}
    }

    sampled_att_length<- c()
    # the weight in sampling SV
    # https://www.displayr.com/shapley-vs-relative-weights/
    for(att in sampled_attribute_comb){
      if(att[1]==1) {
        tempt_len=0
      }else{
        tempt_len=length(att)
      }
      sampled_att_length <- c(sampled_att_length, tempt_len)
    }


    sampled_att_weight <- data.frame(table(sampled_att_length))
    sampled_att_weight$weight <- (1/nrow(sampled_att_weight))/sampled_att_weight$Freq

    foreach::registerDoSEQ()

    marginal_effect_with_weights <- foreach(atts=sampled_attribute_comb,.packages=c("gam","rpart")) %dopar% {

      formula_0 <- as.formula(paste(attribute_of_interest,paste(atts,collapse=" + "),sep=" ~ "))
      formula_1 <- as.formula(paste(attribute_of_interest,paste(paste(atts,collapse=" + "),attribute_iterate,sep=" + "),sep=" ~ "))

      if(model=="linear"){
        model0 <- lm(formula_0,data=Data)
        model1 <- lm(formula_1,data=Data)
      } else if(model=="cart"){
        model0 <- rpart(formula_0,data=Data)
        model1 <- rpart(formula_1,data=Data)
      } else if(model=="gam"){
        model0 <- gam(formula_0,data=Data)
        model1 <- gam(formula_1,data=Data)
      }

      if(atts[1]==1){ # empty set ~1
        weight_tempt <- sampled_att_weight[sampled_att_weight$sampled_att_length==0,"weight"]
      }else{
        weight_tempt <- sampled_att_weight[sampled_att_weight$sampled_att_length==length(atts),"weight"]
      }

      if(evaluation=="r-square"){
        marginal_effect_tempt <-  weight_tempt*(sum(residuals(model0)^2)/(var(Data[,attribute_of_interest])*(nrow(Data)-1)) -
                                                  sum(residuals(model1)^2)/(var(Data[,attribute_of_interest])*(nrow(Data)-1)))
      }else if(evaluation=="mse"){
        marginal_effect_tempt <- weight_tempt*(mean((residuals(model1))^2)-mean((residuals(model0))^2))
      } else if(evaluation=="mae"){
        marginal_effect_tempt <- weight_tempt*(mean(abs(residuals(model1)))-mean(abs(residuals(model0))))
      }
      return(c(marginal_effect_tempt,weight_tempt))
    }

    marginal_effect_att_ite <- data.frame(matrix(unlist(marginal_effect_with_weights), nrow=length(marginal_effect_with_weights), byrow=T))

    #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

    SV_non_confi <- rbind(SV_non_confi,c(attribute_iterate,sum(marginal_effect_att_ite[,1])/sum(marginal_effect_att_ite[,2])))

  }

  ## Commented out For higher simulations
  # stopCluster(cl)

  SV_non_confi <- data.frame(SV_non_confi)
  names(SV_non_confi) <- c("Variable", evaluation)
  SV_non_confi[,2] <- as.numeric(SV_non_confi[,2])
  return(SV_non_confi)

}

# Attribute Sets Sampling ####
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

## Shapley value for multiple regression model using R square or MSE

Shapley.lm <- function(formula, data, metric = c("R2", "MSE"),
                       order = FALSE) {

  # formula: an R formula
  # data: a data frame containing variables for the formula
  # metric: metric for assessing model
  # order: If TRUE, the independent variables will be sorted according to their Shapley values

  method <- match.arg(metric)

  if(!require(dplyr)) {install.packages("dplyr"); library(dplyr)}
  if(!require(lmSubsets)) {install.packages("lmSubsets"); library(lmSubsets)}
  if(!require(doParallel)) {install.packages("doParallel"); library(doParallel)}
  if(!require(future)) {install.packages("future"); library(future)}

  #list of independent variables in the model
  var.names <- attr(terms(formula, which = "term.labels", data = data),
                    which = "term.labels")
  # number of independent varaibles
  nvar <- length(var.names)

  # number of observations
  nobs <- nrow(data)

  # Check all variables that are not numeric
  v.not.num <- names(which(!sapply(data[, var.names], is.numeric)))

  # number of levels in each factor
  n.all.levels <- sum(apply(as.matrix(data[, v.not.num]), 2, function(x) length(unique(x))))
  n.used.levels <- n.all.levels - length(v.not.num)
  n.used.variables <- nvar - length(v.not.num) + n.used.levels

  # maximum number of combinations
  nmax <- max(choose(n.used.variables, 1:n.used.variables))

  # fit all possible subset models
  lm_all <- lmSubsets::lmSubsets(formula, data = data, nbest = nmax)

  # all possible subsets of the regression model
  dt.subsets <- lm_all$subset


  dt.subsets$i <- 1:nrow(dt.subsets)


  # select only models that includes intercept, & drop the intercept column
  dt.subsets <- dplyr::select(dplyr::filter(dt.subsets, `(Intercept)` == TRUE),
                              -`(Intercept)`)


  if (length(v.not.num) > 0){

    for (v in v.not.num){

      v.all <- paste0(v, levels(data[, v]))
      v.all <- v.all[v.all %in% colnames(dt.subsets)]

      #Filter out all cases when the some factor levels were included and some were left out
      dt.subsets <- dt.subsets %>% filter(rowMeans(.[, v.all]) == 0 | rowMeans(.[, v.all]) == 1)

      # Create acolumn for the variable itself
      dt.subsets <- dt.subsets %>% mutate(!!v := as.logical(rowMeans(.[, v.all])))

      #Drop the factor levels
      dt.subsets <- dplyr::select(dt.subsets, -any_of(v.all))

    }

  }


  # Summary (SIZE, BEST, RSS) of all possible models
  dt.submodels <- lm_all$submodel[dt.subsets$i, ]
  dt.subsets <- dplyr::select(dt.subsets, -i)


  # combine models and their summaries
  p <- 0

  if (metric == "R2") {
    R2 <- 0
    dt.subregs <- cbind(dt.subsets, p, dt.submodels, R2)
  } else if (metric == "MSE") {
    MSE <- 0
    dt.subregs <- cbind(dt.subsets, p, dt.submodels, MSE)
  }

  # count the number of features
  dt.subregs$p <- rowSums(dt.subsets)


  if (metric == "R2") {

    # Total Sum of Squares
    SST <- var(data[, all.vars(formula)[1]])*(nobs - 1)

    # computing R2 for each model
    dt.subregs$R2 <- 1 - dt.subregs$RSS/SST

    # average R square for each possible number of subsets
    R2_s.bar <- dt.subregs %>% group_by(p) %>% summarise(R2.bar = mean(R2))

  } else if (metric == "MSE") {

    # MSE for an intercept only model
    MSE.0 <- 0

    # computing MSE for each model
    dt.subregs$MSE <- dt.subregs$RSS/(nobs - dt.subregs$SIZE)

    # average MSE for each possible number of subsets
    MSE_s.bar <- dt.subregs %>% group_by(p) %>% summarise(MSE.bar = mean(MSE))
  }


  # Denominators for computing Shapley Values
  denom <- nvar - 1:(nvar - 1)

  # cl <- makeCluster(detectCores() - 2)
  # registerDoParallel(cl)
  foreach::registerDoSEQ()

  results <- foreach(v = var.names, .packages = c("dplyr")) %dopar% {
    # results <- foreach(v = var.names) %do% {

    #Check if the variable is a numeric variable or not
    v.num <- is.numeric(data[, v])

    if (metric == "R2")  {
      # average R square for each possible number of subsets with that variable
      R2_var.bar <- dt.subregs %>% filter((!!as.name(v)) == TRUE) %>%
        group_by(p) %>% summarise(R2.bar = mean(R2))

      # Shapley value of the variable
      SV.R2 <- sum((R2_var.bar$R2.bar[-nvar] - R2_s.bar$R2.bar[-nvar])/denom) +
        R2_s.bar$R2.bar[nvar]/nvar

    } else if (metric == "MSE")  {
      # average MSE for each possible number of subsets with that variable
      MSE_var.bar <- dt.subregs %>% filter((!!as.name(v)) == TRUE) %>%
        group_by(p) %>% summarise(MSE.bar = mean(MSE))

      # Shapley value of the variable
      SV.MSE <- sum((MSE_var.bar$MSE.bar[-nvar] - MSE_s.bar$MSE.bar[-nvar])/denom) +
        (MSE_s.bar$MSE.bar[nvar] - MSE.0)/nvar
    }

    if (metric == "R2") {
      return(list(Variable = v, SV.R2 = SV.R2))
    }else if (metric == "MSE") {
      return(list(Variable = v, SV.MSE = SV.MSE))
    }

  }

  # stopCluster(cl)

  results <- as.data.frame(bind_rows(results))

  if (metric == "R2") {
    results$SV.R2.pct <- results$SV.R2/sum(results$SV.R2)*100
    results$SV.R2.Rank <- rank(desc(results$SV.R2))
  }else if (metric == "MSE") {
    results$SV.MSE.pct <- results$SV.MSE/sum(results$SV.MSE)*100
    results$SV.MSE.Rank <- rank(results$SV.MSE)
  }


  if (order == TRUE){
    if (metric == "R2") {
      results <- arrange(results, desc(SV.R2))
    }else if (metric == "MSE") {
      results <- arrange(results, SV.MSE)
    }
  }

  return(results)
}


#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
## All Subsets Linear Model ####
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

all.subsets.model <- function(formula, data,
                              model = c("LM", "GLM", "GAM", "CART", "RF", "XGB"),
                              metric = c("R2", "MSE", "MAE", "RMSE", "VoD", "1-R2", "CCA"),
                              metric_sum = c("sum", "mean", "max"),
                              ...) {

  # formula: an R formula
  # data: a data frame containing variables for the formula
  # model: statistical model
  # metric: metric for assessing statistical model
  # metric_sum: how to summarize the metric for multiple dependent variables
  # ...: additional arguments

  data <- data[complete.cases(data), ]
  model <- match.arg(model)
  metric <- match.arg(metric)
  metric_sum <- match.arg(metric_sum)
  ex.args <- eval(substitute(alist(...)))

  if (!exists("ex.args")) {ex.args <- eval(substitute(alist(...)))}

  set <- attr(terms(formula, which = "term.labels", data = data),
              which = "term.labels")
  n <- length(set)
  bin <- vector(mode = "list", length = n)
  for (i in 1L:n) {
    bin[[i]] <- rep.int(c(rep.int(F, 2L ^ (i - 1L)),
                          rep.int(T, 2L ^ (i - 1L))),
                        2L ^ (n - i))
  }

  x.set <- do.call(cbind, bin)
  colnames(x.set) <- set
  x.set <- x.set[-1,]

  # name of dependent variable
  y.names <- setdiff(all.vars(formula), c(".", set))

  ## Commented out For higher simulations
  # if (length(set) > 10) {
  #   no_cores <- future::availableCores()
  #   cl <- parallel::makeCluster(ifelse(no_cores > 3, no_cores - 2, 1))
  #   cat("Building Models: running parallel\n")
  #   doParallel::registerDoParallel(cl)
  #   on.exit(try(stopCluster(cl)))
  # } else {
  #   cat("Building Models: running sequentially\n")
  #   foreach::registerDoSEQ()
  # }

  foreach::registerDoSEQ()

  out <- foreach(j = 1:nrow(x.set), .combine = "rbind") %dopar% {

    x.select <- set[x.set[j, ]]
    X <- model.matrix(~., as.data.frame(data[, x.select]))

    sub_formula <- paste0("cbind(", paste(y.names, collapse = ","), ") ~ ", paste0(x.select, collapse = "+"))
    model_out <- model_metric(as.formula(sub_formula), data, model, metric, metric_sum)

    ans <- c(p = length(x.select), SIZE = length(x.select) + 1, model_out)
    names(ans)[3] <- metric

    return(ans)
  }

  return(cbind.data.frame(x.set, out))

}


#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

## Function for computing exact Shapley values ####

Shapley.Value <- function(formula, data,
                          model = c("LM", "GLM", "GAM", "CART", "RF", "XGB"),
                          metric = c("R2", "MSE", "MAE", "RMSE", "VoD", "1-R2", "CCA"),
                          metric_sum = c("sum", "mean", "max"),
                          order = FALSE, ...){

  # formula: an R formula
  # data: a data frame containing variables for the formula
  # metric: metric for assessing model
  # order: If TRUE, the independent variables will be sorted according to their Shapley values
  # ...: extra (optional) arguments needed for GLM, GAM, & CART models

  data <- data[complete.cases(data), ]
  model <- match.arg(model)
  metric <- match.arg(metric)
  metric_sum <- match.arg(metric_sum)
  ex.args <- eval(substitute(alist(...)))


  if(!require(dplyr)) {install.packages("dplyr"); library(dplyr)}
  if(!require(lmSubsets)) {install.packages("lmSubsets"); library(lmSubsets)}
  if(!require(gam)) {install.packages("gam"); library(gam)}
  if(!require(rpart)) {install.packages("rpart"); library(rpart)}
  if(!require(doParallel)) {install.packages("doParallel"); library(doParallel)}
  if(!require(future)) {install.packages("future"); library(future)}

  #list of independent variables in the model
  var.names <- attr(terms(formula, which = "term.labels", data = data),
                    which = "term.labels")
  # number of independent variables
  nvar <- length(var.names)

  # number of observations
  nobs <- nrow(data)


  dt.subregs <- all.subsets.model(formula, data, model, metric, metric_sum)
  colnames(dt.subregs)[which(colnames(dt.subregs) == metric)] <- "v"


  # average metric for each possible number of subsets
  v_s.bar <- dt.subregs %>% group_by(p) %>% summarise(v.bar = mean(v))

  # Denominators for computing Shapley Values
  denom <- nvar - 1:(nvar - 1)

  ## Commented out For higher simulations
  # if (nvar > 15) {
  #   no_cores <- future::availableCores()
  #   cl <- parallel::makeCluster(ifelse(no_cores > 3, no_cores - 2, 1))
  #   cat("Calculating Shapley Values: running parallel\n")
  #   doParallel::registerDoParallel(cl)
  #   on.exit(try(stopCluster(cl)))
  # } else {
  #   cat("Calculating Shapley Values: running sequentially\n")
  #   foreach::registerDoSEQ()
  # }

  foreach::registerDoSEQ()

  results <- foreach(vn = var.names, .packages = c("dplyr")) %dopar% {

    #Check if the variable is a numeric variable or not
    v.num <- is.numeric(data[, vn])

    # average R square for each possible number of subsets with that variable
    v_var.bar <- dt.subregs %>% filter((!!as.name(vn)) == TRUE) %>%
      group_by(p) %>% summarise(v.bar = mean(v))

    # Shapley value of the variable
    SV <- sum((v_var.bar$v.bar[-nvar] - v_s.bar$v.bar[-nvar])/denom) +
      (v_s.bar$v.bar[nvar])/nvar

    return(list(Variable = vn, SV = SV))

  }

  results <- as.data.frame(bind_rows(results))

  results$SV.pct <- results$SV/sum(results$SV)*100
  results$SV.Rank <- rank(desc(results$SV))


  if (order == TRUE){
    results <- arrange(results, SV.Rank)
  }

  return(results)
}

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

## Function for computing exact Shapley values for all possible coalitions ####

Shapley.Value.Coalition <- function(formula, data,
                                    model = c("LM", "GLM", "GAM", "CART", "RF", "XGB"),
                                    metric = c("R2", "MSE", "MAE", "RMSE", "VoD", "1-R2", "CCA"),
                                    metric_sum = c("sum", "mean", "max"),
                                    order = FALSE, ...){

  # formula: an R formula
  # data: a data frame containing variables for the formula
  # metric: metric for assessing model
  # order: If TRUE, the independent variables will be sorted according to their Shapley values
  # ...: extra (optional) arguments needed for GLM, GAM, & CART models

  data <- data[complete.cases(data), ]
  model <- match.arg(model)
  metric <- match.arg(metric)
  metric_sum <- match.arg(metric_sum)
  ex.args <- eval(substitute(alist(...)))


  if(!require(dplyr)) {install.packages("dplyr"); library(dplyr)}
  if(!require(lmSubsets)) {install.packages("lmSubsets"); library(lmSubsets)}
  if(!require(gam)) {install.packages("gam"); library(gam)}
  if(!require(rpart)) {install.packages("rpart"); library(rpart)}
  if(!require(doParallel)) {install.packages("doParallel"); library(doParallel)}
  if(!require(future)) {install.packages("future"); library(future)}

  #list of independent variables in the model
  var.names <- attr(terms(formula, which = "term.labels", data = data),
                    which = "term.labels")
  # number of independent variables
  nvar <- length(var.names)

  # number of observations
  nobs <- nrow(data)


  dt.subregs <- all.subsets.model(formula, data, model, metric, metric_sum)
  colnames(dt.subregs)[which(colnames(dt.subregs) == metric)] <- "v"


  # Add row ID
  dt.subregs$ID <- dt.subregs$p*10^nvar + as.matrix(dt.subregs[, 1:nvar]) %*% matrix(10^((nvar-1):0))
  dt.subregs <- dt.subregs %>%
    arrange(p,-ID) %>%
    mutate(SV = NA) %>%
    relocate(ID)

  rownames(dt.subregs) <- NULL

  combinations <- dt.subregs %>%
    dplyr::select(-ID, -(p:SV))

  # Loop through all possible coalition sizes
  for(q in 1:(nvar)){

    # Filter based on size of coalition
    dt <- dt.subregs %>%
      filter(p == q)

    for(r in 1:nrow(dt)){

      # Select the coalition of interest
      coal <- dt[r, 2:(nvar+1)]
      coal_ID <- dt$ID[r]

      # Coalitions that contains the target coaltions
      dt.subregs$SC <- as.vector(as.matrix(combinations) %*% t(as.matrix(coal)) == q )

      # Coalitions that do not contain the target coalitions
      dt.subregs$S <- as.vector(as.matrix(!combinations) %*% t(as.matrix(coal)) == q )

      # All coalitions needed to calculate the Shapley value of the target coalition
      dt_coal <- dt.subregs %>% filter(SC | S)

      # Recalculate the number of players in the game
      dt_coal <- dt_coal %>%
        mutate(p = ifelse(SC, p-q+1, p))

      # average metric for each possible number of subsets
      v_s.bar <- dt_coal %>% group_by(p) %>% summarise(v.bar = mean(v))

      # Total number of players (accounting for coalitions)
      ncoal <- nvar - q + 1

      # Denominators for computing Shapley Values
      denom <- ncoal - 1:(ncoal - 1)

      # average R square for each possible number of subsets with that variable
      v_var.bar <- dt_coal %>% filter(SC) %>%
        group_by(p) %>% summarise(v.bar = mean(v))

      # Shapley value of the target coalition
      SV <- sum((v_var.bar$v.bar[-ncoal] - v_s.bar$v.bar[-ncoal])/denom) +
        (v_s.bar$v.bar[ncoal])/ncoal

      dt.subregs$SV[dt.subregs$ID == coal_ID] <- SV
    }

  }

  dt.subregs$SV[nrow(dt.subregs)] <- model_metric(formula, data, model, metric, metric_sum)

  results <- dt.subregs %>%
    dplyr::select(-SIZE, -SC, -S) %>%
    mutate(SV.pct = SV/SV[n()]*100,
           SV.Rank = rank(-SV))

  if (order == TRUE){
    results <- arrange(results, SV.Rank)
  }

  return(results)
}



#...............................................................................
## Function for computing change in v() functions for estimating Shapley Values
## This function is used in Shapley.Value.Est() function

Delta <- function(x, y, data, i, l, m,
                  model = c("LM", "GLM", "GAM", "CART", "RF", "XGB"),
                  metric = c("R2", "MSE", "MAE", "RMSE", "VoD", "1-R2", "CCA"),
                  metric_sum = c("sum", "mean", "max"),
                  ...) {

  # x: a vector containing the names of dependent variables
  # y: a vector containing the names of independent variables
  # data: data set
  # i: number indicating the variable of interest in x
  # l: position of variable of interest
  # m: number of permutations to sample
  # model: statistical model
  # metric: metric for assessing statistical model
  # metric_sum: how to summarize the metric for multiple dependent variables
  # ...: additional arguments

  model <- match.arg(model)
  metric <- match.arg(metric)
  metric_sum <- match.arg(metric_sum)
  ex.args <- eval(substitute(alist(...)))

  if(!require(RcppAlgos)) {install.packages("RcppAlgos"); library(RcppAlgos)}
  if(!require(gam)) {install.packages("gam"); library(gam)}
  if(!require(rpart)) {install.packages("rpart"); library(rpart)}
  if(!require(dplyr)) {install.packages("dplyr"); library(dplyr)}
  if(!require(matrixStats)) {install.packages("matrixStats"); library(matrixStats)}
  if(!require(CCA)) {install.packages("CCA"); library(CCA)}

  # Model with only one independent variable

  if (l == 1) {

    x.select0 <- NULL
    x.select1 <- x[i]

    fmla0 <- paste("cbind(", paste(y, collapse = ","), ") ~ 1")
    fmla1 <- paste("cbind(", paste(y, collapse = ","), ") ~", paste(x.select1, collapse = "+"))

    v0 <- model_metric(as.formula(fmla0), data, model, metric, metric_sum)
    v1 <- model_metric(as.formula(fmla1), data, model, metric, metric_sum)

    out <- v1 - v0

  }else{

    # Model with several independent variables

    all.perm <- RcppAlgos::permuteIter(x[-i], l - 1)
    perm.n <- all.perm$summary()$totalResults

    if (m > perm.n){m <- perm.n}

    if (perm.n < .Machine$integer.max) {
      # number of permutations is with machine integer limit
      sel.perm <- all.perm[[sample.int(perm.n, m)]]
    } else {
      # number of permutations is more than machine integer limit
      smp <- floor(runif(m)*as.double(perm.n))
      smp[smp == 0] <- 1
      if(any(smp > 10^13)) {smp <- gmp::as.bigz(smp)}
      sel.perm <- all.perm[[smp]]
    }


    if(m == 1){sel.perm <- t(as.matrix(sel.perm))}

    out <- apply(sel.perm, 1, function(S) {

      x.select0 <- S
      x.select1 <- c(S, x[i])

      fmla0 <- paste("cbind(", paste(y, collapse = ","), " )~", paste(x.select0, collapse = "+"))
      fmla1 <- paste("cbind(", paste(y, collapse = ","), " )~", paste(x.select1, collapse = "+"))

      v0 <- model_metric(as.formula(fmla0), data, model, metric, metric_sum)
      v1 <- model_metric(as.formula(fmla1), data, model, metric, metric_sum)


      d <- v1 - v0

      return(d)
    })

  }

  phi <- sum(out)
  sigma2 <- var(out)
  return(list(phi = phi, sigma2 = sigma2))
}


#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
## Function for computing an Estimate of Shapley values based on "m" permutation samples ####

Shapley.Value.Est <- function(formula, data, m = 5000,
                              model = c("LM", "GLM", "GAM", "CART", "RF", "XGB"),
                              metric = c("R2", "MSE", "MAE", "RMSE", "VoD", "1-R2", "CCA"),
                              metric_sum = c("sum", "mean", "max"),
                              order = FALSE, ...){

  # formula: an R formula
  # data: a data frame containing variables for the formula
  # m: total sample size
  # metric: metric for assessing model
  # metric_sum: how to summarize the metric for multiple dependent variables
  # order: If TRUE, the independent variables will be sorted according to their Shapley values
  # ...: additional arguments

  data <- data[complete.cases(data), ]
  model <- match.arg(model)
  metric <- match.arg(metric)
  metric_sum <- match.arg(metric_sum)
  ex.args <- eval(substitute(alist(...)))

  if(!require(abind)) {install.packages("abind"); library(abind)}

  #list of independent variables in the model
  x.names <- attr(terms(formula, which = "term.labels", data = data),
                  which = "term.labels")

  # name of dependent variable
  y.name <- setdiff(all.vars(formula), c(".", x.names))

  # number of independent variables
  nvar <- length(x.names)


  # STAGE 1
  q <- matrix(factorial(nvar - 1)/(factorial(1:nvar - 1)*factorial(nvar - 1:nvar)),
              nrow = nvar, ncol = nvar, byrow = TRUE)

  m.exp <- pmin(q, max(2, floor(m/(2*nvar^2)))) # add max(2, ...) Aug 03. 2023

  Omega <- (m.exp == q) # Set of strata with sample size determined
  m.tot <- matrix(0, nrow = nvar, ncol = nvar)
  m.tot[Omega] <- q[Omega]
  m <- m - sum(q[Omega])

  foreach::registerDoSEQ()

  out.exp <- foreach::foreach(l = 1:nvar, .combine = function(...) abind(..., along = 3)) %:%
    foreach(i = 1:nvar, .combine = "rbind") %dopar% {
      D <- Delta(x.names, y.name, data, i, l, m.exp[i,l], model, metric, metric_sum)
      return(data.frame(phi = D$phi, sigma2 = D$sigma2))
    }

  phi.exp <- out.exp[, 1, ]
  sigma2 <- out.exp[, 2, ]

  sigma2[Omega] <- 0

  m.tot <- pmin(pmax(floor(m*sigma2/sum(sigma2)), m.exp, na.rm = TRUE), q)
  m.st <- m.tot - m.exp

  if (sum(m.st) == 0) {

    # Shapley Values
    SV = rowMeans(phi.exp/m.tot)

  } else {

    # STAGE 2
    foreach::registerDoSEQ()

    phi.st <- foreach::foreach(l = 1:nvar, .combine = "cbind") %:%
      foreach(i = 1:nvar, .combine = "rbind") %dopar% {
        if (m.st[i, l] == 0) {
          return(0)
        } else {
          D <- Delta(x.names, y.name, data, i, l, m.st[i,l], model, metric)
          return(D$phi)
        }
      }

    # Shapley Values
    SV = rowMeans((phi.exp + phi.st)/m.tot)
  }

  # Normalizing the Shapley values
  SV.pct = SV/sum(SV)*100

  SV.Rank <- rank(desc(SV))

  results <- data.frame(Variable = x.names, SV, SV.pct, SV.Rank)

  # Sorting the Shapley Values
  if (order == TRUE){
    results <- arrange(results, SV.Rank)
  }
  row.names(results) <- NULL
  return(results)
}

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
## Function for computing an Estimate of Shapley values based on "Q*M" permutation samples for each feature ####
# Reference: A multilinear sampling algorithm to estimate Shapley Values
# Okhrati and Aldo Lipani, 2020
Shapley.Value.Est2 <- function(formula, data, M = 10, Q = 100,
                  model = c("LM", "GLM", "GAM", "CART", "RF", "XGB"),
                  metric = c("R2", "MSE", "MAE", "RMSE", "VoD", "1-R2", "CCA"),
                  metric_sum = c("sum", "mean", "max"),
                  order = FALSE, ...){

  # formula: an R formula
  # data: a data frame containing variables for the formula
  # M: accuracy of the estimation of Delta for each feature
  # Q: level of discretization of the estimate of Delta for each feature
  ## M*Q = the total number of samples
  # metric: metric for assessing model
  # metric_sum: how to summarize the metric for multiple dependent variables
  # order: If TRUE, the independent variables will be sorted according to their Shapley values
  # ...: additional arguments

  data <- data[complete.cases(data), ]
  model <- match.arg(model)
  metric <- match.arg(metric)
  metric_sum <- match.arg(metric_sum)
  ex.args <- eval(substitute(alist(...)))

  if(!require(abind)) {install.packages("abind"); library(abind)}

  #list of independent variables in the model
  x.names <- attr(terms(formula, which = "term.labels", data = data),
                  which = "term.labels")

  # name of dependent variable
  y.name <- setdiff(all.vars(formula), c(".", x.names))

  # number of independent variables
  nvar <- length(x.names)

  S  <- vector(mode = "double", length = nvar)

  for(j in 1:nvar){
    Sj <- 0
    for(q in seq(from = 0, to = 0.5, by = 1/Q)){

      e <- 0

      for(m in 1:M){

        Iq_full <- Iq_reduce <- sample(c(TRUE, FALSE), size = nvar, replace = TRUE, prob = c(q, 1 - q))
        Ip_full <- Ip_reduce <- !Iq_full
        # Ip <- !Iq # the Haved Owen sample

        # Iq_full <- Iq_reduce <- Iq
        Iq_full[j] <- TRUE # Include variable j in the coalition
        Iq_reduce[j] <- FALSE # Remove variable j from the coalition

        # Ip_full <- Ip_reduce <- Ip
        Ip_full[j] <- TRUE # Include variable j in the coalition
        Ip_reduce[j] <- FALSE # Remove variable j from the coalition


        # Formulate the reduced models
        # Check if no feature is selected in Iq
        if(!any(Iq_reduce)){
          # there are no predictors
          fmla0_q <- paste0("cbind(", paste(y.name, collapse = ","), ") ~ 1")
        } else{
          fmla0_q <- paste0("cbind(", paste(y.name, collapse = ","), ") ~ ", paste(x.names[Iq_reduce], collapse = "+"))
        }

        # Check if no feature is selected in Ip
        if(!any(Ip_reduce)){
          # there are no predictors
          fmla0_p <- paste0("cbind(", paste(y.name, collapse = ","), ") ~ 1")
        } else{
          fmla0_p <- paste0("cbind(", paste(y.name, collapse = ","), ") ~ ", paste(x.names[Ip_reduce], collapse = "+"))
        }

        # Formulate the full models
        fmla1_q <- paste0("cbind(", paste(y.name, collapse = ","), ") ~ ", paste(x.names[Iq_full], collapse = "+"))
        fmla1_p <- paste0("cbind(", paste(y.name, collapse = ","), ") ~ ", paste(x.names[Ip_full], collapse = "+"))

        # Fit model and obtain model metrics
        v0_q <- model_metric(as.formula(fmla0_q), data, model, metric, metric_sum)
        v1_q <- model_metric(as.formula(fmla1_q), data, model, metric, metric_sum)

        v0_p <- model_metric(as.formula(fmla0_p), data, model, metric, metric_sum)
        v1_p <- model_metric(as.formula(fmla1_p), data, model, metric, metric_sum)

        # Calculate the margibal contribution of feature j
        h_q <- v1_q - v0_q
        h_p <- v1_p - v0_p

        e <- e + h_q + h_p
      }

      Sj <- Sj + e
    }
    S[j] <- Sj
  }

  SV <- S/(Q*M)

  # Normalizing the Shapley values
  SV.pct = SV/sum(SV)*100

  # Rank results
  SV.Rank <- rank(desc(SV))

  results <- data.frame(Variable = x.names, SV, SV.pct, SV.Rank)

  # Sorting the Shapley Values
  if (order){
    results <- arrange(results, SV.Rank)
  }
  row.names(results) <- NULL
  return(results)
}



#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Frontier Curve for Risk and Utility of Attributes ####
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

frontier_curve <- function(risk, utility, attribute_labels = NULL,
                           frontier = c("upper", "lower", "none"),
                           plot_labels = c("frontier", "all", "none"),
                           output = c("data", "graph", "both"),
                           special_points = NULL,
                           facet_var = NULL) {

  # risk: a vector of Shapley values of risk
  # utility: a vector of Shapley values of utility
  # attribute_labels: a vector names for the attributes
  # frontier: type of frontier curve
  #           "upper" - low risk and high utility;
  #           "lower" - high risk and low utility;
  #           "none" - no frontier curve.
  # plot_labels: "frontier" - labels will be added to only points on the frontier;
  #              "all" - labels will be added to all plot;
  #              "none" - no labels will be added to the points.
  # output: "data" - outputs only data
  #         "both" - outputs both graph and data
  #         "graph" - outputs only graph
  # special_points: labels of special points to be labeled on graph,
  #                 if plot_labels = "frontier"
  # facet_var: grouping variable for creating facet plots


  if(!require(ggplot2)) {install.packages("ggplot2"); library(ggplot2)}
  if(!require(dplyr)) {install.packages("dplyr"); library(dplyr)}

  if (length(risk) != length(utility)) {
    stop("risk and utility should be of the same length")
  }

  if (!is.null(attribute_labels) & length(risk) != length(attribute_labels)) {
    stop("attribute_labels should be of the same length as risk and utility")
  }

  if (!is.null(facet_var) & length(risk) != length(facet_var)) {
    stop("facet_var should be of the same length as risk and utility")
  }

  if (!is.null(special_points) & !is.null(attribute_labels)) {
    if (!all(special_points %in% attribute_labels)) {
      stop("special_points should be found in the attribute_labels")
    }
  }

  frontier <- match.arg(frontier)
  plot_labels <- match.arg(plot_labels)
  output <- match.arg(output)

  # Generate attribute labels if not provided
  if (is.null(attribute_labels)) {
    n <- length(risk)
    if (n <= 26) {
      attribute_labels = LETTERS[1:n]
    } else {
      LETTERS702 <- c(LETTERS, sapply(LETTERS, function(x) paste0(x, LETTERS)))
      attribute_labels = LETTERS702[1:n]
    }
  }

  dt <- data.frame(attribute = attribute_labels, risk, utility)

  # Check if there is a need for a facet wrap variable when not provided
  if (is.null(facet_var)) {

    tab_att <- table(attribute_labels)

    if (tab_att[1] > 1) {
      dt <- dt %>%
        group_by(attribute) %>%
        mutate(facet_var = 1:n()) %>%
        ungroup()
      # facet_n <- tab_att[1]
      # facet_var = rep(1:facet_n, each = length(tab_att))
      # dt <- dt %>% add_column(facet_var)
    }
  } else {

    dt <- dt %>% add_column(facet_var)
  }


  if (!("facet_var" %in% colnames(dt))) {

    n <- length(risk)

    # Detecting points that forms the frontier
    if (frontier == "lower") {
      # Low risk but high utility
      dominated <- sapply(1:n, function(i) {
        any((risk[i] > risk[-i]) & (utility[i] < utility[-i]))
      })
    } else if (frontier == "upper"){
      # High risk but low utility
      dominated <- sapply(1:n, function(i) {
        any((risk[i] < risk[-i]) & (utility[i] > utility[-i]))
      })
    } else {
      dominated <- FALSE
    }

    dt <- dt %>% add_column(dominated)

  } else {

    # Detecting points that forms the frontier

    if (frontier == "lower") {

      # Low risk but high utility
      dt <- dt %>%
        group_by(facet_var) %>%
        mutate(dominated = sapply(1:length(risk),
                                  function(i) {
                                    any((risk[i] > risk[-i]) & (utility[i] < utility[-i]))
                                  })
        ) %>%
        ungroup()

    } else if(frontier == "upper"){

      # High risk but low utility
      dt <- dt %>%
        group_by(facet_var) %>%
        mutate(dominated = sapply(1:length(risk),
                                  function(i) {
                                    any((risk[i] < risk[-i]) & (utility[i] > utility[-i]))
                                  })
        ) %>%
        ungroup()

    } else {

      dt$dominated <- FALSE
    }

  }


  if (output %in% c("both", "graph")) {

    # Plotting the frontier curve
    axes_lim <- range(dt %>% dplyr::select(risk, utility)) # Limit for axes

    p <- ggplot2::ggplot(data = dt, mapping = aes(x = utility, y = risk))

    if (!is.null(special_points)) {
      # Change plotting character of special points of interest
      point_shape <- (attribute_labels %in% special_points)
      p <- p +
        geom_point(mapping = aes(shape = point_shape, colour = dominated)) +
        scale_shape_manual(name = "Feature",
                           values = c(16, 1),
                           labels = c("Non Confidential", "Confidential"),
                           limits = c(FALSE, TRUE))
    } else {
      p <- p + geom_point(mapping = aes(colour = dominated))
    }

    if (frontier != "none"){

      p <- p +
        geom_line(aes(linetype = dominated, color = dominated)) +
        scale_linetype_manual(name = "Line",
                            values = c("solid", "blank"),
                            labels = c("Frontier", ""),
                            limits = c(FALSE, TRUE)) +
      #   expand_limits(x = axes_lim, y = axes_lim) +

      scale_colour_manual(name = "Line",
                          labels = c("Frontier", ""),
                          limits = c(FALSE, TRUE),
                          values = c("red", "blue")
      ) +
      guides(colour = guide_legend(override.aes = list(shape = NA)))

    } else {

      p <- p +
        scale_colour_manual(name = "Line",
                            labels = c("Frontier", ""),
                            limits = c(FALSE, TRUE),
                            values = c("black", "black")
                            ) +
        guides(colour = "none")


    }

    p <- p +
      theme_bw()  +
      labs(#title = "Risk-Utility Frontier Curve",
        #subtitle = ifelse(frontier == "lower",
        #                   paste("Low Risk but High Utility"),
        #                   paste("High Risk but Low Utility")),
        x = "Shapley Value of Utility for Features",
        y = "Shapley Value of Risk for Features"
      )



    # Labeling the points
    if (plot_labels == "frontier") {

      p <- p + geom_text_repel(mapping = aes(label = attribute),
                               data = filter(dt, dominated == FALSE),
                               alpha = 0.6, size = 3)

      if (!is.null(special_points)) {
        temp <- !(special_points %in% as.vector(dt[dt$dominated == FALSE, "attribute"]))
        sp <- special_points[temp]

        if (!is.null(sp)) {

          p <- p + geom_text_repel(mapping = aes(label = sp),
                                   data = filter(dt, attribute %in% sp),
                                   alpha = 0.6, size = 3)
        }

      }

    } else if (plot_labels == "all") {

      p <- p + geom_text_repel(mapping =  aes(label = attribute),
                               alpha = 0.9, size = 3)

    }

    # Adding facet wrap plot if possible

    if ("facet_var" %in% colnames(dt)) {
      if (length(unique(dt$facet_var)) >  1) {
        p <- p + facet_wrap(~ facet_var)
      }
    }

  }

  # Return output(s)

  if (output == "graph") {
    return(p)
  } else if (output == "data") {
    return(dt)
  } else { # output == "both"
    return(list(graph = p, data = dt))
  }

}


#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Unified function for all our models ####
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

model_metric <- function(formula, data,
                         model = c("LM", "GLM", "GAM", "CART", "RF", "XGB"),
                         metric = c("R2", "R2_pseudo", "MSE", "MAE", "RMSE", "VoD", "1-R2", "CCA"),
                         metric_sum = c("sum", "mean", "max"),
                         ...) {

  # formula: an R formula
  # data: a data frame containing variables for the formula
  # model: specify type of statistical model
  # metric: metric for assessing model
  # metric_sum: how to summarize the metric for multiple dependent variables
  # ...: additional arguments

  if(!require(gam)) {install.packages("gam"); library(gam)}
  if(!require(rpart)) {install.packages("rpart"); library(rpart)}
  if(!require(dplyr)) {install.packages("dplyr"); library(dplyr)}
  if(!require(matrixStats)) {install.packages("matrixStats"); library(matrixStats)}
  if(!require(CCA)) {install.packages("CCA"); library(CCA)}
  if(!require(randomForest)) {install.packages("randomForest"); library(randomForest)}
  if(!require(xgboost)) {install.packages("xgboost"); library(xgboost)}


  model <- match.arg(model)
  metric <- match.arg(metric)
  metric_sum <- match.arg(metric_sum)
  ex.args <- eval(substitute(alist(...)))

  #.............................................................................
  # Variable Extraction
  #.............................................................................
  frame <- model.frame(formula, data)

  Y <- as.matrix(model.extract(frame, "response"))
  y <- all.vars(formula)[1:ncol(Y)]

  if(length(y) == 1) {colnames(Y) <- y}

  if(ncol(Y) == length(all.vars(formula))) {
    # no independent variables
    X <- matrix(1, ncol = 1, nrow = nrow(Y))
    colnames(X) <- "(intercept)"
    x <- 1
  } else {
    X <- model.matrix(formula, frame)
    X <- X[, !grepl("cbind", colnames(X))] # remove columns that starts with "cbind"
    x <- colnames(X)[-1]
  }


  #.............................................................................
  # Model Residual Calculation
  #.............................................................................

  if (metric == "CCA") {

    # Do nothing since a model is not needed

  } else if (model == "LM") {

    model_res  <- as.matrix(.lm.fit(x = X, y = Y)$residuals)

  } else if (model == "GLM") {

    if (is.null(ex.args$family)) {
      fmly <- gaussian()
    } else{
      fmly <- eval(parse(text = paste(ex.args$family,"()", sep = "")))
    }

    model_res <- sapply(y,
                        FUN = function(y){glm.fit(x = X, y = Y[, y], family = fmly)$residuals},
                        simplify = TRUE
    )

  } else if (model == "GAM") {

    if (is.null(ex.args$family)) {
      fmly <- gaussian
    } else{
      fmly <- eval(parse(text = ex.args$family))
    }

    fmla <- paste(y, "~", paste(x, collapse = "+"))
    model_res <- sapply(fmla,
                        FUN = function(fml){gam::gam(as.formula(fml), data = data, family = fmly)$residuals},
                        simplify = TRUE
    )

  } else if (model == "CART") {

    fmla <- paste(y, "~", paste(x, collapse = "+"))
    model_res <- sapply(fmla,
                        FUN = function(fml) {

                          if (all(X == 1) & ncol(X) == 1) {
                            predictions <- mean(Y[, y])
                          } else {

                            fit = rpart::rpart(as.formula(fml), data = data, y = FALSE,
                                               method = "anova",
                                               control = rpart.control(minsplit = 20, minbucket = round(20/3),
                                                                       cp = 0.01, maxcompete = 0, maxsurrogate = 0,
                                                                       usesurrogate = 2, xval = 10,
                                                                       surrogatestyle = 0, maxdepth = 10))
                            # Predict the response variable
                            predictions <- predict(fit, type = "vector")

                          }


                          # Calculate the residuals
                          residuals <- data[, all.vars(as.formula(fml))[1]] - predictions

                          return(as.matrix(residuals))

                        },
                        simplify = TRUE
    )

  } else if (model == "RF") {

    model_res <- sapply(y,
                        FUN = function(y) {

                          if (all(X == 1) & ncol(X) == 1) {
                            pred <- mean(Y[, y])
                          } else {

                            rf <- randomForest(x = X, y = Y[, y])
                            pred <- rf$predicted
                          }

                          residuals <- Y[, y] - pred
                          return(as.matrix(residuals))
                        },
                        simplify = TRUE
    )

  } else if (model == "XGB") {

    model_res <- sapply(y,
                        FUN = function(y) {

                          if (all(X == 1) & ncol(X) == 1) {
                            pred <- mean(Y[, y])
                          } else {

                            xgb_train = xgb.DMatrix(data = X, label = Y[, y])
                            watchlist <- list(train = xgb_train, test = xgb_train)

                            params <- list(max.depth = 2,  ## the maximum depth of each decision tree
                                           eta = 0.1, # Step size shrinkage used in update to prevents overfitting. After each boosting step, we can directly get the weights of new features, and eta shrinks the feature weights to make the boosting process more conservative.  range: [0,1]
                                           nthread = 1,
                                           gamma = 10, # Minimum loss reduction required to make a further partition on a leaf node of the tree. The larger gamma is, the more conservative the algorithm will be.
                                           early_stopping_rounds = 10, ## if we dont see an improvement in this many rounds, stop)
                                           watchlist = watchlist,
                                           #num_parallel_tree,# [default=1] Number of parallel trees constructed during each iteration. This option is used to support boosted random forest.
                                           objective = "reg:squarederror",
                                           silent = 1,
                                           verbosity = 0 # verbosity parameter verbosity = 0 in the model definition. The values it can take are: 0 - "silent", 1 - "warning", 2 - "info", 3 - "debug"
                            )

                            xgb <- xgb.train(params, data = xgb_train, nrounds = 50) # max number of boosting iterations # reduced from nrounds = 1000 to avoid overfitting
                            pred <- predict(xgb, newdata = X)

                          }

                          residuals <- Y[, y] - pred

                          return(as.matrix(residuals))
                        },
                        simplify = TRUE
    )

  }

  #.............................................................................
  # Metric Evaluation Extraction
  #.............................................................................

  if (metric == "CCA") {

    if(is.numeric(x)) {
      # no dependent variable
      ans <- 0
    } else {
      ans <- max(CCA::cc(data[y], data[x])$cor)^2
    }


  } else if (metric == "R2") {

    SST <- colVars(Y)*(nrow(Y) - 1)
    ans <- 1 - colSums(model_res^2)/SST

  } else if (metric == "R2_pseudo") {

    if(length(y) == 1) {
      ans <- 1 - cor(Y, model_res)^2
    } else{
      ans <- 1 - diag(cor(Y, model_res))^2
    }


  } else if (metric == "MSE") {

    MSE_null <- colMeans((Y - colMeans(Y)[col(Y)])^2)
    ans <- MSE_null - colMeans(model_res^2)

  } else if (metric == "MAE"){

    MAE_null <- mean(abs(Y - colMeans(Y)[col(Y)]))
    ans <- MAE_null - colMeans(abs(model_res))
    #ans <- mean(abs(model0$residuals))

  } else if (metric == "RMSE") {

    RMSE_null <- sqrt(colMeans((Y - colMeans(Y)[col(Y)])^2))
    ans <- RMSE_null - sqrt(colMeans(model_res^2))

  } else if (metric == "VoD") { #VoD: Variance of Difference (security)

    ans <- colVars(model_res)/colVars(Y)

  } else { # 1-R2: 1 - R squared (security)

    SST <- colVars(Y)*(nrow(Y) - 1)
    ans <- sum(model_res^2)/SST

  }

  #.............................................................................
  # Summarize metric for multiple response variables
  #.............................................................................

  ans <- eval(call(metric_sum, ans))

  return(ans)
}
