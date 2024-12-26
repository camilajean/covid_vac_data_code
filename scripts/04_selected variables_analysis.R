# **Manually selected variables analysed with fixed and random effects**


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



library(lme4)
library(broom.mixed) # For tidy output of mixed models

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


# Load in Data ------------------------------------------------------------

england_clean_data <- read.csv("../data/england_clean_std.csv")



# Variables of interest ---------------------------------------------------

colnames(england_clean_data)

vars <- c(                                               
  "Pop_per_km2",                                              
  "prop_travelling_to_work",                                 
  "prop_not_in_work",                                 
  "prop_all_other_white",                                     
  "prop_mixed_multiple",                                    
  "prop_asian",                                     
  "prop_black_afr_car",                                       
  "prop_other",                                      
  "IMD_Average_score",                                        
  "prop_o65",
  "prop_u25",
  "Median_annual_income",                                     
  "Other_proportion",                                         
  "unringfenced_percapita",                                             
  "ASC_infection_control_fund_percapita",
  "contain_outbreak_management_percapita",
  "Income_score",                                             
  "Employment_score",                                         
  "Education_Skills_and_Training_score",                      
  "Health_Deprivation_and_Disability_score",                  
  "Crime_score",                                              
  "Barriers_to_Housing_and_Services_score",                   
  "Living_Environment_score"
)



# Script: Individual Doses Fixed Effects ----------------------------------

# ** First Dose 
fd_mv <- multivar_model("cumVaccPercentage_FirstDose", vars, england_clean_data)

firstdose_multivar <- fd_mv$Results
firstdose_multivar <- firstdose_multivar %>%
  mutate(
    dose = "First"
  )

# ** Second Dose
sd_mv <- multivar_model("cumVaccPercentage_SecondDose", vars, england_clean_data)

seconddose_multivar <- sd_mv$Results
seconddose_multivar <- seconddose_multivar %>%
  mutate(
    dose = "Second"
  )

# ** Third Dose
td_mv <- multivar_model("cumVaccPercentage_ThirdDose", vars, england_clean_data)

thirddose_multivar <- td_mv$Results
thirddose_multivar <- thirddose_multivar %>%
  mutate(
    dose = "Third"
  )


# ** Combine all results into one dataframe


simple_fixed <- rbind (firstdose_multivar,seconddose_multivar,thirddose_multivar)

simple_fixed <- simple_fixed %>%
  mutate (
    model = "Simple",
    effects = "Fixed"
  )

head(simple_fixed)



# Script: Individual Doses Random Effects ---------------------------------

# ** First Dose
fd_mv_rand <- lmm_model("cumVaccPercentage_FirstDose", vars,"RGN21CD", england_clean_data )

firstdose_multivar_rand <- fd_mv_rand$Results
firstdose_multivar_rand <- firstdose_multivar_rand %>%
  mutate (
    dose = "First"
  )

# ** Second Dose
sd_mv_rand <- lmm_model("cumVaccPercentage_SecondDose", vars,"RGN21CD", england_clean_data )

seconddose_multivar_rand <- sd_mv_rand$Results
seconddose_multivar_rand <- seconddose_multivar_rand %>%
  mutate (
    dose = "Second"
  )

# ** Third Dose

td_mv_rand <- lmm_model("cumVaccPercentage_ThirdDose", vars,"RGN21CD", england_clean_data )

thirddose_multivar_rand <- td_mv_rand$Results
thirddose_multivar_rand <- thirddose_multivar_rand %>%
  mutate (
    dose = "Third"
  )

# ** Combining into one
simple_random <- rbind(firstdose_multivar_rand, seconddose_multivar_rand, thirddose_multivar_rand)
simple_random <- simple_random %>%
  mutate(
    model = "Simple",
    effects = "Random"
  )

head(simple_random)




# Save Individual Doses CSV -----------------------------------------------

simple_models <- rbind(simple_fixed,simple_random)
write.csv(simple_models, "../results_tables/simple_model_results_final.csv")



# Script: Dropout Fixed ---------------------------------------------------

# ** First to Second
fd <- multivar_model("d1_v_d2", vars, england_clean_data)
firstdose_multivar <- fd$Results 

firstdose_multivar <- firstdose_multivar %>%
  mutate(
    dose = "First vs Second"
  )


# ** Second to Third
sd <- multivar_model("d2_v_d3", vars, england_clean_data)
seconddose_multivar <- sd$Results

seconddose_multivar <- seconddose_multivar %>%
  mutate(
    dose = "Second vs Third"
  )


# ** Combine into one dataset
simple_fixed <- rbind (firstdose_multivar,seconddose_multivar)

simple_fixed <- simple_fixed %>%
  mutate (
    model = "Simple",
    effects = "Fixed"
  )

head(simple_fixed)
subset(simple_fixed,simple_fixed$Variable == "prop_u25")



# Script: Dropout Random --------------------------------------------------

# ** First to Second
fd_rand <- lmm_model("d1_v_d2", vars,"RGN21CD", england_clean_data )

firstdose_multivar_rand <- fd_rand$Results
firstdose_multivar_rand <- firstdose_multivar_rand %>%
  mutate (
    dose = "First vs Second"
  )


# ** Second to Third
sd_rand <- lmm_model("d2_v_d3", vars,"RGN21CD", england_clean_data )

seconddose_multivar_rand <- sd_rand$Results
seconddose_multivar_rand <- seconddose_multivar_rand %>%
  mutate (
    dose = "Second vs Third"
  )

# ** Combine into a singe dataframe

simple_random <- rbind(firstdose_multivar_rand, seconddose_multivar_rand)
simple_random <- simple_random %>%
  mutate(
    model = "Simple",
    effects = "Random"
  )

head(simple_random)
subset(simple_random,simple_random$Variable == "prop_u25")
subset(simple_fixed,simple_fixed$Variable == "prop_u25")


# Save Dropout CSV --------------------------------------------------------

simple_models <- rbind(simple_fixed,simple_random)
write.csv(simple_models, "../results_tables/simple_between_doses_final.csv")




