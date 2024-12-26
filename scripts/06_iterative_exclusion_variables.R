# **Iterative excluding variables analysed with fixed and random effects**


# General Libraries -------------------------------------------------------

library(lme4)
library(nlme)
library(tidyverse)
library(ggplot2)
library(readxl)
library(broom.mixed)
library(table1)
library(dplyr)
library(gt)
library(patchwork)


# Clear environment and set working directory -----------------------------

rm(list = ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))



# Functions ---------------------------------------------------------------

multivar_model <- function(response_var, predictors, data) {
  # Create the formula for the linear model
  formula <- as.formula(paste(response_var, "~", paste(predictors, collapse = " + ")))
  
  # Fit the linear model
  model <- lm(formula, data = data)
  
  # Print the summary of the model
  print(summary(model))
  
  # Extract coefficients and confidence intervals
  coef_info <- coef(summary(model))
  ci <- confint(model)
  
  # Ensure only valid coefficients are processed
  valid_vars <- rownames(coef_info)
  valid_ci <- rownames(ci)
  common_vars <- intersect(valid_vars, valid_ci) # Handle any discrepancies
  
  # Subset to valid coefficients only
  coef_info <- coef_info[common_vars, , drop = FALSE]
  ci <- ci[common_vars, , drop = FALSE]
  
  # Create a results data frame
  results <- data.frame(
    Variable = rownames(coef_info),
    Beta = coef_info[, "Estimate"],
    CI_Lower = ci[, 1],
    CI_Upper = ci[, 2],
    AIC = AIC(model),
    BIC = BIC(model),
    stringsAsFactors = FALSE
  )
  
  # Return a list: both the detailed summary and the results table
  return(list(
    Model_Summary = summary(model),  # Full summary output
    Results = results                # Results table for easy access
  ))
}



lmm_model <- function(response_var, predictors, random_effect, data) {
  # Create the fixed effects formula
  fixed_effects <- paste(predictors, collapse = " + ")
  
  # Combine fixed and random effects into a formula
  formula <- as.formula(paste(response_var, "~", fixed_effects, "+ (1 |", random_effect, ")"))
  
  # Fit the linear mixed model
  model <- lmer(formula, data = data)
  
  # Print the summary of the model
  print(summary(model))
  
  # Extract coefficients and confidence intervals
  coef_info <- broom.mixed::tidy(model, effects = "fixed", conf.int = TRUE)
  
  # Calculate AIC and BIC
  aic_value <- AIC(model)
  bic_value <- BIC(model)
  
  # Create a results data frame
  results <- data.frame(
    Variable = coef_info$term,
    Beta = coef_info$estimate,
    CI_Lower = coef_info$conf.low,
    CI_Upper = coef_info$conf.high,
    AIC = aic_value,
    BIC = bic_value,
    stringsAsFactors = FALSE
  )
  
  # Return a list: full model summary and structured results
  return(list(
    Model_Summary = summary(model),  # Full summary of the model
    Results = results                # Structured results table
  ))
}



iterative_exclu_lm <- function(data, response_var, predictors, num_iterations) {
  # Define the initial model formula
  initial_formula <- as.formula(paste(response_var, "~", paste(predictors, collapse = " + ")))
  print(paste("Initial formula:", deparse(initial_formula))) # Debugging print
  
  # Fit the initial model using lm
  updated_model <- lm(initial_formula, data = data)
  model_results <- broom::tidy(updated_model, conf.int = TRUE)
  
  # Initialize an iteration counter, a list to store results, and a set to keep track of excluded variables
  iteration <- 0
  results_list <- list()
  excluded_vars <- c()
  performance_metrics <- data.frame(Iteration = numeric(), AIC = numeric(), BIC = numeric())
  
  # Stepwise exclusion loop
  while (iteration < num_iterations) {
    # Filter out the intercept and already excluded variables
    remaining_vars <- model_results %>%
      filter(term != "(Intercept)", !term %in% excluded_vars)
    
    # Check if there are any more variables to exclude
    if (nrow(remaining_vars) == 0) {
      break
    }
    
    # Extract p-values
    pvalues <- remaining_vars$p.value
    
    # Identify the variable with the highest p-value
    var_to_exclude <- remaining_vars %>%
      arrange(desc(p.value)) %>%
      slice(1) %>%
      pull(term)
    
    print(paste("Iteration:", iteration + 1, "Excluding variable:", var_to_exclude)) # Debugging print
    
    # Save the current model results
    results_list[[iteration + 1]] <- remaining_vars %>%
      mutate(iteration = iteration + 1, excluded_var = var_to_exclude)
    
    # Update the list of excluded variables
    excluded_vars <- c(excluded_vars, var_to_exclude)
    
    # Update the formula to exclude the current and previously excluded variables
    remaining_vars <- setdiff(predictors, excluded_vars)
    updated_formula <- as.formula(paste(response_var, "~", paste(remaining_vars, collapse = " + ")))
    print(paste("Updated formula:", deparse(updated_formula))) # Debugging print
    
    # Refit the model
    updated_model <- lm(updated_formula, data = data)
    model_results <- broom::tidy(updated_model, conf.int = TRUE)
    
    # Calculate AIC and BIC explicitly
    aic_value <- AIC(updated_model)
    bic_value <- BIC(updated_model)
    
    # Store performance metrics
    performance_metrics <- rbind(performance_metrics, data.frame(Iteration = iteration + 1, AIC = aic_value, BIC = bic_value))
    
    # Increment iteration counter
    iteration <- iteration + 1
  }
  
  # Combine all results into a single data frame
  final_results <- bind_rows(results_list)
  
  return(list(final_results = final_results, performance_metrics = performance_metrics))
}



iterative_exclu_lmer <- function(data, response_var, predictors, random_effect, num_iterations) {
  # Define the initial model formula with random effects
  initial_formula <- as.formula(paste(response_var, "~", paste(predictors, collapse = " + "), "+ (1 |", random_effect, ")"))
  print(paste("Initial formula:", deparse(initial_formula))) # Debugging print
  
  # Fit the initial model using lmer from lmerTest
  updated_model <- lmer(initial_formula, data = data)
  model_results <- tidy(updated_model, effects = "fixed", conf.int = TRUE)
  
  # Initialize an iteration counter, a list to store results, and a set to keep track of excluded variables
  iteration <- 0
  results_list <- list()
  excluded_vars <- c()
  performance_metrics <- data.frame(Iteration = numeric(), AIC = numeric(), BIC = numeric())
  
  # Stepwise exclusion loop
  while (iteration < num_iterations) {
    # Filter out the intercept and already excluded variables
    first_dose_vars <- model_results %>%
      filter(!term %in% c("(Intercept)", excluded_vars))
    
    # Check if there are any more variables to exclude
    if (nrow(first_dose_vars) == 0) {
      break
    }
    
    # Extract p-values using lmerTest and match with tidy results
    pvalues <- summary(updated_model)$coefficients[, "Pr(>|t|)"]
    pvalues <- pvalues[match(first_dose_vars$term, rownames(summary(updated_model)$coefficients))]
    first_dose_vars$p.value <- pvalues
    
    # Identify the variable with the highest p-value
    var_to_exclude <- first_dose_vars %>%
      arrange(desc(p.value)) %>%
      slice(1) %>%
      pull(term)
    
    print(paste("Iteration:", iteration + 1, "Excluding variable:", var_to_exclude)) # Debugging print
    
    # Save the current model results
    results_list[[iteration + 1]] <- first_dose_vars %>%
      mutate(iteration = iteration + 1, excluded_var = var_to_exclude)
    
    # Update the list of excluded variables
    excluded_vars <- c(excluded_vars, var_to_exclude)
    
    # Update the formula to exclude the current and previously excluded variables
    remaining_vars <- setdiff(predictors, excluded_vars)
    updated_formula <- as.formula(paste(response_var, "~", paste(remaining_vars, collapse = " + "), "+ (1 |", random_effect, ")"))
    print(paste("Updated formula:", deparse(updated_formula))) # Debugging print
    
    # Refit the model
    updated_model <- lmer(updated_formula, data = data)
    model_results <- tidy(updated_model, effects = "fixed", conf.int = TRUE)
    
    # Calculate AIC and BIC explicitly
    aic_value <- AIC(updated_model)
    bic_value <- BIC(updated_model)
    
    # Store performance metrics
    performance_metrics <- rbind(performance_metrics, data.frame(Iteration = iteration + 1, AIC = aic_value, BIC = bic_value))
    
    # Increment iteration counter
    iteration <- iteration + 1
  }
  
  # Combine all results into a single data frame
  final_results <- bind_rows(results_list)
  
  return(list(final_results = final_results, performance_metrics = performance_metrics))
}




# Load in Data ------------------------------------------------------------

england_clean_data <- read.csv("../data/england_clean_std.csv")

vars <- c("Population",                                               
          "Pop_per_km2",                                              
          "Median_age",                                             
          "prop_travelling_to_work",                                 
          "prop_not_in_work",                                 
          "prop_all_other_white",                                     
          "prop_mixed_multiple",                                    
          "prop_asian",                                     
          "prop_black_afr_car",                                       
          "prop_other",                                      
          "IMD_Average_score",                                        
          "resident_earnings",                                        
          "mean_age",                                                 
          "prop_o65",
          "prop_u25",
          "mean_popden",                                              
          "Median_annual_income",                                     
          "no_jobs",                                                  
          "retail_and_recreation_percent_change_from_baseline",       
          "grocery_and_pharmacy_percent_change_from_baseline",        
          "parks_percent_change_from_baseline",                       
          "transit_stations_percent_change_from_baseline",            
          "workplaces_percent_change_from_baseline",                  
          "residential_percent_change_from_baseline",                 
          "Other_proportion",                                         
          "NHS_registered_population",                                
          "Core_services_funding",                                    
          "Primary_care_funding",                                     
          "Specialised_services",                                     
          "unringfenced_percapita",                                             
          "contain_outbreak_management_percapita",                              
          "ASC_infection_control_fund_percapita",                               
          "ASC_workforce_capacity",                                   
          "ASC_rapid_testing",                                        
          "CEV_fund_percapita",                                                 
          "compliance_and_enforcement",                               
          "welcome_back_fund_percapita",                                        
          "rough_sleeping",                                           
          "next_steps_accommodation",                                 
          "food_and_essential",                                       
          "home_to_school_transport",                                 
          "DWP_winter_grant_percapita",                                         
          "sales_compensation",                                        
          "fire_contingency",                                         
          "leisure_recovery",                                         
          "omicron_support_fund_percapita",                                     
          "protect_and_vaccinate",                                    
          "council_tax_support",                                      
          "tax_income_guarantee_council",                             
          "tax_income_guarantee_business_rates",                      
          "Income_score",                                             
          "Employment_score",                                         
          "Education_Skills_and_Training_score",                      
          "Health_Deprivation_and_Disability_score",                  
          "Crime_score",                                              
          "Barriers_to_Housing_and_Services_score",                   
          "Living_Environment_score")


# Script: Fixed Stepwise Exclusion ----------------------------------------


response_vars <- c("cumVaccPercentage_FirstDose", "cumVaccPercentage_SecondDose", "cumVaccPercentage_ThirdDose","d1_v_d2","d2_v_d3")

# ** First Dose


fd_stepwise_fixed <- iterative_exclu_lm(data = england_clean_data, 
                                        response_var = "cumVaccPercentage_FirstDose", 
                                        predictors = vars, 
                                        num_iterations = length(vars)-1)

fd_pm_fixed <- fd_stepwise_fixed$performance_metrics
fd_res_fixed <- fd_stepwise_fixed$final_results

# Identify the best iteration based on AIC
best_aic_iteration <- fd_pm_fixed %>%
  filter(AIC == min(AIC)) %>%
  pull(Iteration)

# Identify the best iteration based on BIC
best_bic_iteration <- fd_pm_fixed %>%
  filter(BIC == min(BIC)) %>%
  pull(Iteration)

cat("Best model based on AIC is at iteration:", best_aic_iteration, "\n")
cat("Best model based on BIC is at iteration:", best_bic_iteration, "\n")

# First Dose = iteration no. 2 (according to BIC)

fd_bic_fixed = best_bic_iteration




# ** Second Dose
sd_stepwise_fixed <- iterative_exclu_lm(data = england_clean_data, 
                                        response_var = "cumVaccPercentage_SecondDose", 
                                        predictors = vars, 
                                        num_iterations = length(vars)-1)
sd_pm_fixed <- sd_stepwise_fixed$performance_metrics
sd_res_fixed <- sd_stepwise_fixed$final_results


# Identify the best iteration based on AIC
best_aic_iteration <- sd_pm_fixed %>%
  filter(AIC == min(AIC)) %>%
  pull(Iteration)

# Identify the best iteration based on BIC
best_bic_iteration <- sd_pm_fixed %>%
  filter(BIC == min(BIC)) %>%
  pull(Iteration)

cat("Best model based on AIC is at iteration:", best_aic_iteration, "\n")
cat("Best model based on BIC is at iteration:", best_bic_iteration, "\n")

# Second Dose = iteration no. 5 (according to BIC)
sd_bic_fixed = best_bic_iteration



# ** Third Dose
td_stepwise_fixed <- iterative_exclu_lm(data = england_clean_data, 
                                        response_var = "cumVaccPercentage_ThirdDose", 
                                        predictors = vars, 
                                        num_iterations = length(vars)-1)
td_pm_fixed <- td_stepwise_fixed$performance_metrics
td_res_fixed <- td_stepwise_fixed$final_results


# Identify the best iteration based on AIC
best_aic_iteration <- td_pm_fixed %>%
  filter(AIC == min(AIC)) %>%
  pull(Iteration)

# Identify the best iteration based on BIC
best_bic_iteration <- td_pm_fixed %>%
  filter(BIC == min(BIC)) %>%
  pull(Iteration)

cat("Best model based on AIC is at iteration:", best_aic_iteration, "\n")
cat("Best model based on BIC is at iteration:", best_bic_iteration, "\n")

# Third Dose = iteration no. 1 (according to BIC)
td_bic_fixed = best_bic_iteration



# Dropout 1v2

stepwise_fixed_1v2 <- iterative_exclu_lm(data = england_clean_data, 
                                         response_var = "d1_v_d2", 
                                         predictors = vars, 
                                         num_iterations = length(vars)-1)
pm_fixed_1v2 <- stepwise_fixed_1v2$performance_metrics
res_fixed_1v2 <- stepwise_fixed_1v2$final_results

# Identify the best iteration based on AIC
best_aic_iteration <- pm_fixed_1v2 %>%
  filter(AIC == min(AIC)) %>%
  pull(Iteration)

# Identify the best iteration based on BIC
best_bic_iteration <- pm_fixed_1v2 %>%
  filter(BIC == min(BIC)) %>%
  pull(Iteration)

cat("Best model based on AIC is at iteration:", best_aic_iteration, "\n")
cat("Best model based on BIC is at iteration:", best_bic_iteration, "\n")


bic_fixed_1v2 = best_bic_iteration


# Dropout 2v3
stepwise_fixed_2v3 <- iterative_exclu_lm(data = england_clean_data, 
                                         response_var = "d2_v_d3", 
                                         predictors = vars, 
                                         num_iterations = length(vars)-1)
pm_fixed_2v3 <- stepwise_fixed_2v3$performance_metrics
res_fixed_2v3 <- stepwise_fixed_2v3$final_results


# Identify the best iteration based on AIC
best_aic_iteration <- pm_fixed_2v3 %>%
  filter(AIC == min(AIC)) %>%
  pull(Iteration)

# Identify the best iteration based on BIC
best_bic_iteration <- pm_fixed_2v3 %>%
  filter(BIC == min(BIC)) %>%
  pull(Iteration)

cat("Best model based on AIC is at iteration:", best_aic_iteration, "\n")
cat("Best model based on BIC is at iteration:", best_bic_iteration, "\n")

bic_fixed_2v3 = best_bic_iteration


# Script: Random Stepwise Exclusion ---------------------------------------

# ** First Dose
fd_stepwise_rand <- iterative_exclu_lmer(data = england_clean_data, 
                                         response_var = "cumVaccPercentage_FirstDose", 
                                         predictors = vars, 
                                         random_effect = "RGN21CD", 
                                         num_iterations = length(vars))
fd_pm_rand <- fd_stepwise_rand$performance_metrics
fd_res_rand <- fd_stepwise_rand$final_results


# Identify the best iteration based on AIC
best_aic_iteration <- fd_pm_rand %>%
  filter(AIC == min(AIC)) %>%
  pull(Iteration)

# Identify the best iteration based on BIC
best_bic_iteration <- fd_pm_rand %>%
  filter(BIC == min(BIC)) %>%
  pull(Iteration)

cat("Best model based on AIC is at iteration:", best_aic_iteration, "\n")
cat("Best model based on BIC is at iteration:", best_bic_iteration, "\n")

# First Dose = iteration no. 1 (according to BIC)

fd_bic_rand= best_bic_iteration



# ** Second Dose
sd_stepwise_rand <- iterative_exclu_lmer(data = england_clean_data, 
                                         response_var = "cumVaccPercentage_SecondDose", 
                                         predictors = vars, 
                                         random_effect = "RGN21CD", 
                                         num_iterations = length(vars))
sd_pm_rand <- sd_stepwise_rand$performance_metrics
sd_res_rand <- sd_stepwise_rand$final_results


# Identify the best iteration based on AIC
best_aic_iteration <- sd_pm_rand %>%
  filter(AIC == min(AIC)) %>%
  pull(Iteration)

# Identify the best iteration based on BIC
best_bic_iteration <- sd_pm_rand %>%
  filter(BIC == min(BIC)) %>%
  pull(Iteration)

cat("Best model based on AIC is at iteration:", best_aic_iteration, "\n")
cat("Best model based on BIC is at iteration:", best_bic_iteration, "\n")

# Second Dose = iteration no. 1
sd_bic_rand = best_bic_iteration



# ** Third Dose
td_stepwise_rand <- iterative_exclu_lmer(data = england_clean_data, 
                                         response_var = "cumVaccPercentage_ThirdDose", 
                                         predictors = vars, 
                                         random_effect = "RGN21CD", 
                                         num_iterations = length(vars))
td_pm_rand <- td_stepwise_rand$performance_metrics
td_res_rand <- td_stepwise_rand$final_results


# Identify the best iteration based on AIC
best_aic_iteration <- td_pm_rand %>%
  filter(AIC == min(AIC)) %>%
  pull(Iteration)

# Identify the best td_pm_rand based on BIC
best_bic_iteration <- td_pm_rand %>%
  filter(BIC == min(BIC)) %>%
  pull(Iteration)

cat("Best model based on AIC is at iteration:", best_aic_iteration, "\n")
cat("Best model based on BIC is at iteration:", best_bic_iteration, "\n")

# Third Dose = iteration no. 4 or 5 (go with 5 as simplest model with still same performance )
td_bic_rand = best_bic_iteration


# ** Dropout 1v2
stepwise_rand_1v2 <- iterative_exclu_lmer(data = england_clean_data, 
                                          response_var = "d1_v_d2", 
                                          predictors = vars, 
                                          random_effect = "RGN21CD", 
                                          num_iterations = length(vars))
pm_rand_1v2 <- stepwise_rand_1v2$performance_metrics
res_rand_1v2 <- stepwise_rand_1v2$final_results


# Identify the best iteration based on AIC
best_aic_iteration <- pm_rand_1v2 %>%
  filter(AIC == min(AIC)) %>%
  pull(Iteration)

# Identify the best iteration based on BIC
best_bic_iteration <- pm_rand_1v2 %>%
  filter(BIC == min(BIC)) %>%
  pull(Iteration)

cat("Best model based on AIC is at iteration:", best_aic_iteration, "\n")
cat("Best model based on BIC is at iteration:", best_bic_iteration, "\n")

bic_rand_1v2= best_bic_iteration


# ** Dropout 2v3

stepwise_rand_2v3 <- iterative_exclu_lmer(data = england_clean_data, 
                                          response_var = "d2_v_d3", 
                                          predictors = vars, 
                                          random_effect = "RGN21CD", 
                                          num_iterations = length(vars))
pm_rand_2v3 <- stepwise_rand_2v3$performance_metrics
res_rand_2v3 <- stepwise_rand_2v3$final_results


# Identify the best iteration based on AIC
best_aic_iteration <- pm_rand_2v3 %>%
  filter(AIC == min(AIC)) %>%
  pull(Iteration)

# Identify the best iteration based on BIC
best_bic_iteration <- pm_rand_2v3 %>%
  filter(BIC == min(BIC)) %>%
  pull(Iteration)

cat("Best model based on AIC is at iteration:", best_aic_iteration, "\n")
cat("Best model based on BIC is at iteration:", best_bic_iteration, "\n")

bic_rand_2v3 = best_bic_iteration


# Script: Extracting Selected Variables from Models -----------------------

# Now that we've determine best iteration, extract those variables from fixed and random models

# ** Fixed
fd_iter_vars_fixed <- fd_res_fixed %>%
  filter(iteration == fd_bic_fixed & term != "(Intercept)") %>%
  select(term) %>%
  distinct()
fd_iter_vars_fixed <- unlist(unique(fd_iter_vars_fixed$term))


sd_iter_vars_fixed <- sd_res_fixed %>%
  filter(iteration == sd_bic_fixed & term != "(Intercept)") %>%
  select(term) %>%
  distinct()
sd_iter_vars_fixed <- unlist(unique(sd_iter_vars_fixed$term))


td_iter_vars_fixed <- td_res_fixed %>%
  filter(iteration == td_bic_fixed & term != "(Intercept)") %>%
  select(term) %>%
  distinct()
td_iter_vars_fixed <- unlist(unique(td_iter_vars_fixed$term))


iter_vars_fixed_1v2 <- res_fixed_1v2 %>%
  filter(iteration == bic_fixed_1v2 & term != "(Intercept)") %>%
  select(term) %>%
  distinct()
iter_vars_fixed_1v2 <- unlist(unique(iter_vars_fixed_1v2$term))


iter_vars_fixed_2v3 <- res_fixed_2v3 %>%
  filter(iteration == bic_fixed_2v3 & term != "(Intercept)") %>%
  select(term) %>%
  distinct()
iter_vars_fixed_2v3 <- unlist(unique(iter_vars_fixed_2v3$term))





# ** Random


fd_iter_vars_rand <- fd_res_rand %>%
  filter(iteration == fd_bic_rand & term != "(Intercept)" & term != "sd__(Intercept)" & term != "sd__Observation") %>%
  select(term) %>%
  distinct()
fd_iter_vars_rand <- unlist(unique(fd_iter_vars_rand$term))


sd_iter_vars_rand <- sd_res_rand %>%
  filter(iteration == sd_bic_rand & term != "(Intercept)" & term != "sd__(Intercept)" & term != "sd__Observation") %>%
  select(term) %>%
  distinct()
sd_iter_vars_rand <- unlist(unique(sd_iter_vars_rand$term))


td_iter_vars_rand <- td_res_rand %>%
  filter(iteration == td_bic_rand & term != "(Intercept)" & term != "sd__(Intercept)" & term != "sd__Observation") %>%
  select(term) %>%
  distinct()
td_iter_vars_rand <- unlist(unique(td_iter_vars_rand$term))


iter_vars_rand_1v2 <- res_rand_1v2 %>%
  filter(iteration == bic_rand_1v2 & term != "(Intercept)" & term != "sd__(Intercept)" & term != "sd__Observation") %>%
  select(term) %>%
  distinct()
iter_vars_rand_1v2 <- unlist(unique(iter_vars_rand_1v2$term))


iter_vars_rand_2v3 <- res_rand_2v3 %>%
  filter(iteration == bic_rand_2v3 & term != "(Intercept)" & term != "sd__(Intercept)" & term != "sd__Observation") %>%
  select(term) %>%
  distinct()
iter_vars_rand_2v3 <- unlist(unique(iter_vars_rand_2v3$term))



# Script: Extract Results and Combine -------------------------------------

# Fixed
fd_step_fixed <- multivar_model("cumVaccPercentage_FirstDose", fd_iter_vars_fixed, england_clean_data)

fd_step_fixed<- fd_step_fixed$Results
fd_step_fixed <- fd_step_fixed %>%
  mutate(
    dose = "First"
  )

sd_step_fixed <- multivar_model("cumVaccPercentage_SecondDose", sd_iter_vars_fixed, england_clean_data)

sd_step_fixed <- sd_step_fixed$Results
sd_step_fixed <- sd_step_fixed %>%
  mutate(
    dose = "Second"
  )

td_step_fixed <- multivar_model("cumVaccPercentage_ThirdDose", td_iter_vars_fixed, england_clean_data)

td_step_fixed <- td_step_fixed$Results
td_step_fixed <- td_step_fixed %>%
  mutate(
    dose = "Third"
  )


stepwise_fixed <- rbind (fd_step_fixed,sd_step_fixed,td_step_fixed)

stepwise_fixed <- stepwise_fixed %>%
  mutate (
    model = "Stepwise",
    effects = "Fixed"
  )

step_fixed_1v2 <- multivar_model("d1_v_d2", iter_vars_fixed_1v2, england_clean_data)

step_fixed_1v2 <- step_fixed_1v2$Results %>%
  mutate(
    dose = "First vs Second"
  )

step_fixed_2v3 <- multivar_model("d2_v_d3", iter_vars_fixed_2v3, england_clean_data)

step_fixed_2v3 <- step_fixed_2v3$Results %>%
  mutate(
    dose = "Second vs Third"
  )


stepwise_fixed_dropout <- rbind (step_fixed_1v2,step_fixed_2v3)

stepwise_fixed_dropout <- stepwise_fixed_dropout %>%
  mutate (
    model = "Stepwise",
    effects = "Fixed"
  )

head(stepwise_fixed_dropout)
subset(stepwise_fixed_dropout, stepwise_fixed_dropout$Variable == "Pop_per_km2")




# Random

fd_step_rand <- lmm_model("cumVaccPercentage_FirstDose", fd_iter_vars_rand,"RGN21CD", england_clean_data )

fd_step_rand <- fd_step_rand$Results
fd_step_rand <- fd_step_rand %>%
  mutate (
    dose = "First"
  )

sd_step_rand <- lmm_model("cumVaccPercentage_SecondDose", sd_iter_vars_rand,"RGN21CD", england_clean_data )

sd_step_rand <- sd_step_rand$Results
sd_step_rand <- sd_step_rand %>%
  mutate (
    dose = "Second"
  )

td_step_rand <- lmm_model("cumVaccPercentage_ThirdDose", td_iter_vars_rand,"RGN21CD", england_clean_data )

td_step_rand <- td_step_rand$Results
td_step_rand <- td_step_rand %>%
  mutate (
    dose = "Third"
  )

stepwise_random <- rbind(fd_step_rand,sd_step_rand,td_step_rand )
stepwise_random <- stepwise_random %>%
  mutate(
    model = "Stepwise",
    effects = "Random"
  )


step_rand_1v2 <- lmm_model("d1_v_d2", iter_vars_rand_1v2,"RGN21CD", england_clean_data )
step_rand_1v2 <- step_rand_1v2$Results %>%
  mutate (
    dose = "First vs Second"
  )

step_rand_2v3 <- lmm_model("d2_v_d3", iter_vars_rand_2v3,"RGN21CD", england_clean_data )
step_rand_2v3 <- step_rand_2v3$Results %>%
  mutate (
    dose = "Second vs Third"
  )

stepwise_random_dropout <- rbind(step_rand_1v2,step_rand_2v3 )
stepwise_random_dropout <- stepwise_random_dropout %>%
  mutate(
    model = "Stepwise",
    effects = "Random"
  )


# Save as CSV -------------------------------------------------------------

stepwise_models <- rbind(stepwise_fixed,stepwise_random)
write.csv(stepwise_models, "../results_tables/stepwise_model_results_final.csv")


stepwise_models_dropout <- rbind(stepwise_fixed_dropout,stepwise_random_dropout)
write.csv(stepwise_models_dropout, "../results_tables/stepwise_between_doses_final.csv")


