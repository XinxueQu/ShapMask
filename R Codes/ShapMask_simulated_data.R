ShapMask <- function(mask, model, metric, sim) {

mask <- as.vector(mask)
model <- as.vector(model)
metric <- as.vector(metric)
sim <- as.numeric(sim)

library(magrittr)
library(tidyverse)
library(doParallel)

num_conf_vars <- length(conf_var)

d_max <- ifelse(mask %in% adp_vec, 2.7, 1.8)

# Change in noise delta
delta <- ifelse(mask %in% adp_vec, 0.3, 0.15)

# Noise levels
d_vec <- seq(from = delta, to = d_max, by = delta)

sel_vec <- c("benchmark", "random", "risk", "wc")


results_tempt <- sapply(sel_vec, 
                        function(sel) {
                          start_time <- Sys.time()
                          
                          # Memory allocation for storing individual value of risk and utility
                          k <- length(d_vec)
                          max_t <- phi_r <- phi_u <- R <- U <- numeric(k)
                          next_feat <- masked_feat <- vector(mode = "character", length = k)
                          
                          # Simulate a particular instance of the sequential masking
                          # Allocating memory to track each feature's noise level
                          d_all <- numeric(ncol(Data))
                          names(d_all) <- colnames(Data)
                          Data_Masked <- Data
                          for (d in d_vec) {
                            
                            # for other methods (including benchmark and proposed selection approach)
                            if (d == d_vec[1] | sel == "benchmark") {
                              next_att <- conf_var
                            }
                            
                            if(sel == "random" && d != d_vec[1] ){
                              # exclude both confi and predictive vars from random selection
                              nonconf_vars  <- setdiff( names(d_all), c(conf_var,pred_var))
                              selected_vars <- sample( nonconf_vars, length(nonconf_vars)/2 )
                              next_att <-  c(conf_var, selected_vars)
                            }
                            
                            # split the variance among all variables to be masked
                            d_all[next_att] <- d_all[next_att] + delta/length(next_att)                         
                            
                            # Storing 
                            index <- (d == d_vec)
                            masked_feat[index] <- paste( next_att,  collapse="_")
                            
                            for(var in next_att){ 
                              if(var %in% conf_var){
                                mask_var_name <- paste(var, "Masked",sep="_")
                              } else{
                                mask_var_name <- var
                              }
                              
                              set.seed(sim+1234)
                              
                              if (mask %in% adp_vec) {
                                Data_Masked[, mask_var_name] <- ADP(Data[, var],
                                                                    S = dplyr::select(Data, -all_of(c(pred_var))),
                                                                    d = d_all[var], Sigma_e = NULL, method = mask)
                              } else if (mask %in% mdp_vec) {
                                Data_Masked[, mask_var_name] <- MDP(Data[, var],
                                                                    d = d_all[var],
                                                                    method = mask)
                              } else {
                                print("Masking method not found.")
                              }
                              
                            }
                            
                            # Disclosure Risk & Utility
                            Total_Risk <- model_metric(formula = risk_model_formula,
                                                       data = dplyr::select(Data_Masked, !{{ pred_var }}),
                                                       model = model, metric = metric)
                            
                            Total_Utility <- model_metric(formula = utility_model_formula,
                                                          data = dplyr::select(Data_Masked, !{{ conf_var }}),
                                                          model = model, metric = metric)
                            
                            
                            # Storing the initial and risk and utility values
                            if (d == d_vec[1]) {
                              R_initial <- Total_Risk
                              U_initial <- Total_Utility
                            } else if (d == d_vec[length(d_vec)]) {
                              R_final <- Total_Risk
                              U_final <- Total_Utility
                            }
                            
                            R[index] <- Total_Risk
                            U[index] <- Total_Utility
                            
                            
                            if (sel != "benchmark" & sel != "random") {
                              
                              # Shapley values for Disclosure Risk & Utility
                              if (SV_calc) {# Compute exact Shapley values
                                
                                SV_Risk <- Shapley.Value(formula = risk_model_formula,
                                                         data = dplyr::select(Data_Masked, !{{ pred_var }}),
                                                         model = model, metric = metric)
                                
                                SV_Utility <- Shapley.Value(formula = utility_model_formula,
                                                            data = dplyr::select(Data_Masked, !{{ conf_var }}),
                                                            model = model, metric = metric)
                                
                              } else {# Approximate Shapley values
                                
                                SV_Risk <- Shapley.Value.Est(formula = risk_model_formula,
                                                             data = dplyr::select(Data_Masked, !{{ pred_var }}),
                                                             model = model, metric = metric)
                                
                                SV_Utility <- Shapley.Value.Est(utility_model_formula,
                                                                data = dplyr::select(Data_Masked, !{{ conf_var }}),
                                                                model = model, metric = metric)
                                
                              }
                              
                              SV_combined <- full_join(x = SV_Risk, y = SV_Utility,
                                                       by = "Variable",
                                                       suffix = c("_Risk", "_Utility"))
                              
                              if (sel == "wc") {
                                SV_combined <- SV_combined %>%
                                  mutate(t = w*SV_Risk/Total_Risk - (1 - w)* SV_Utility/Total_Utility)
                              } else if (sel == "risk") {
                                SV_combined <- SV_combined %>%
                                  mutate(t = SV_Risk) 
                              } else if (sel == "ratio") {
                                SV_combined <- SV_combined %>%
                                  mutate(t = SV.pct_Risk/SV.pct_Utility)
                              } else {
                                stop("Select an appropriate 'sel_method'")
                              }
                              
                              # Finding the frontier
                              SV_fc <- SV_combined %$%
                                frontier_curve(risk = SV_Risk,
                                               utility = SV_Utility,
                                               attribute_labels = Variable,
                                               output = "data")
                              
                              # Looking for all the points on the frontier
                              SV_combined_fc <- left_join(x = SV_combined, y = SV_fc,
                                                          by = c("Variable" = "attribute")) %>%
                                dplyr::select(Variable, dominated, t)
                              
                              # Finding the next attribute to mask
                              next_att <- SV_combined_fc %>%
                                mutate(Variable = str_remove(Variable, "_Masked")) %>%
                                filter(dominated == FALSE, Variable %in% c(numerical_atts)) %>%
                                slice_max(across(t), with_ties = FALSE) %>%
                                dplyr::select(Variable) %>%
                                pull()
                              
                              # If there is no numerical attribute on the frontier, choose the topic numerical feature under the metric
                              if(length(next_att) == 0) {
                                next_att <- SV_combined_fc %>%
                                  mutate(Variable = str_remove(Variable, "_Masked")) %>%
                                  filter(Variable %in% c(numerical_atts)) %>%
                                  slice_max(across(t), with_ties = FALSE) %>%
                                  dplyr::select(Variable) %>%
                                  pull()
                              }
                              
                              phi_r[index] <- SV_combined[SV_combined$Variable == mask_var_name, "SV_Risk"]
                              phi_u[index] <- SV_combined[SV_combined$Variable == mask_var_name, "SV_Utility"]
                              max_t[index] <- SV_combined[SV_combined$Variable == mask_var_name, "t"]
                              
                            } else{ # when sel = benchmark
                              
                              phi_r[index] <- NA
                              phi_u[index] <- NA
                              max_t[index] <- NA
                              
                            }
                            
                            next_feat[index] <- paste(next_att,collapse = "_")
                            
                          }
                          
                          
                          ans <- data.frame(
                            #mask, metric,model, sim, # need to comment out these columns when using gapply() function
                            d = d_vec, 
                            sel, masked_feat, R, U, 
                            next_feat, phi_r, phi_u, max_t,
                            R_initial, R_final, 
                            U_initial, U_final ,
                            comp_time = rep(Sys.time() - start_time, length(d_vec))
                          )
                          
                          return(ans)
                          
                        },
                        
                        simplify = FALSE
                        
)
results <- do.call(rbind, results_tempt)
return(results)

}