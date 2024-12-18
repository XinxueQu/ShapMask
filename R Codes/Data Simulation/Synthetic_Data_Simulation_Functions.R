# Estimate the kth cumulant
kth_cumulant <- function(x, k){
  x <- x[!is.na(x)]
  xbar <- mean(x)
  stdev <- sd(x)
  ans <- mean((x - xbar)^k)/(stdev^k)
  return(ans)
}

# estimate the mean of a lognormal distribution
lnorm_mean <- function(x, x.pos = TRUE){
  x <- x[!is.na(x)]
  if(x.pos){x <- x[x>0]}
  xbar <- mean(x)
  stdev <- sd(x)
  ans <- log(xbar^2/sqrt(xbar^2 + stdev^2))
  return(ans)
}

# Estimate the standard deviation of a lognormal distribution
lnorm_sd <- function(x, x.pos = TRUE){
  x <- x[!is.na(x)]
  if(x.pos){x <- x[x>0]}
  xbar <- mean(x)
  stdev <- sd(x)
  ans <- sqrt(log(1 + (stdev/xbar)^2))
  return(ans)
}

# Find exclude zeros before computing mean
nonzero_mean <- function(x){
  x <- x[!is.na(x)]
  x <- x[x != 0]
  xbar <- mean(x)
  return(xbar)
}

# Find exclude zeros before computing mean
nonzero_var <- function(x){
  x <- x[!is.na(x)]
  x <- x[x != 0]
  varx <- var(x)
  return(varx)
}

# Find zero-inflated poisson distribution parameters given mean and standard deviation
zipoisson_params <- function(m, s){
  lambda <- (s^2 + m^2)/m - 1
  psrt0 <- (s^2 - m)/(s^2 + m^2 - m)
  return(c(lambda = lambda, psrt0 = psrt0))
}

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Fit distribution to a data and generate new observations
gen_x <- function(n = NULL, x = NULL, dist = c("normal", "lognormal", "zipois", "nbinom"), 
                  params = list(), estimate = FALSE, show_params = FALSE){
  
  if (!require("fitdistrplus")) {install.packages("fitdistrplus"); library(fitdistrplus)}
  if (!require("VGAM")) {install.packages("VGAM"); library(VGAM)}
  
  if(!is.null(x)){x <- x[!is.na(x)]}
  
  if(dist == "normal"){
    
    if(is.null(params$p0) & !is.null(x)){
      p0 <- mean(x == 0)
    }
    x2 <- x[x != 0]
    
    if(estimate){
      ft <- fitdist(x2, "norm")
      params <- list(mean = ft$estimate[["mean"]], sd = ft$estimate[["sd"]], p0 = p0)
    }else if(length(params) == 0){
      params <- list(mean = mean(x2), sd = sd(x2), p0 = p0)
    }
    
    if(!is.null(n)){
      y <- rnorm(n, params$mean, params$sd)
      i <- (runif(n) <= params$p0)
      y <- ifelse(i, 0, y)
    }
    
  }else if(dist == "lognormal"){
    
    if(is.null(params$p0) & !is.null(x)){
      p0 <- mean(x == 0)
    }
    x2 <- x[x != 0]
    
    if(estimate){
      ft <- fitdist(x2, "lnorm")
      params <- list(meanlog = ft$estimate[["meanlog"]], sdlog = ft$estimate[["sdlog"]], p0 = p0)
    }else if(length(params) == 0){
      ft <- fitdist(x2, "lnorm", method = "mme", order = 3)
      params <- list(meanlog = ft$estimate[["meanlog"]], sdlog = ft$estimate[["sdlog"]], p0 = p0)
    }
    
    if(!is.null(n)){
      y <- rlnorm(n, params$meanlog, params$sdlog)
      i <- (runif(n) <= params$p0)
      y <- ifelse(i, 0, y)
    }
    
  }else if(dist == "zipois"){
    
    if(estimate | length(params) == 0){
      xbar <- mean(x)
      varx <- var(x)
      lambda_est <- (varx + xbar^2)/xbar - 1
      pstr0_est <- (varx - xbar)/(varx + xbar^2 - xbar)
      # params <- list(lambda = lambda_est, pstr0 = pstr0_est)
      ft <- fitdistr(x, dzipois, start = list(lambda = lambda_est, pstr0 = pstr0_est),
                     lower = c(0, 0), upper = c(Inf, 1))
      params <- list(lambda =  c(ft$estimate[["lambda"]]), pstr0 = c(ft$estimate[["pstr0"]]))
    }
    
    if(!is.null(n)){y <- rzipois(n, params$lambda, params$pstr0)}
    
  }
  
  if(is.null(n) & !is.null(x)){
    return(unlist(params))
  }else if(!is.null(n) & is.null(x)){
    return(y)
  }else if(show_params){
    return(list(obs = y, params = unlist(params)))
  }else{
    return(y)
  }
  
}