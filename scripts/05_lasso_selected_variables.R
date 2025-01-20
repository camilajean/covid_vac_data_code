# **Lasso stability selected variables analysed with fixed and random effects**


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
suppressPackageStartupMessages(library(sharp))



# Clear environment and set working directory -----------------------------

rm(list = ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

set.seed(123)


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
          "Core_services_funding_percapita",                                    
          "Primary_care_funding_percapita",                                     
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



# Script: Lasso Stability Selection ---------------------------------------

england_clean_data_exlc_na <- na.omit(england_clean_data) # stability selection only works on compelete cases. droped 6 rows with NA

dropped_rows <- england_clean_data[!complete.cases(england_clean_data), ] # 3 LTLA's dropped as missing 

na_counts <- colSums(is.na(england_clean_data))

# ** Identify columns with NAs
columns_with_na <- na_counts[na_counts > 0]
print(columns_with_na)

# As stability cant handle missing data we exclude the columns which contain missing values

x_predictors <- england_clean_data_exlc_na[ , names(england_clean_data_exlc_na) %in% vars]
dim(x_predictors)

# ** First Dose
y_firstdose <- england_clean_data_exlc_na$cumVaccPercentage_FirstDose
str(y_firstdose)

out_fd <- VariableSelection(
  xdata = x_predictors, 
  ydata = y_firstdose,
  verbose = FALSE,
  penalty.factor = c(rep(1, ncol(x_predictors))),
  family = "gaussian")

plot(out_fd,cex.axis = 0.5, cex.lab = 0.7, cex.main = 0.8) 
CalibrationPlot(out_fd)

#png(filename = "out_fd_plot_adjusted.png", width = 1450, height = 1200, res = 300)
#par(mar = c(9, 4, 4, 2) + 0.1)
#plot(out_fd, cex.axis = 0.5, cex.lab = 0.7, cex.main = 0.8) 
#dev.off()


#png(filename = "calplot_fd.png", width = 2500, height = 2000, res = 300)
#par(mar = c(9, 9, 9, 9) + 0.1)
#CalibrationPlot(out_fd)
#dev.off()

# Selected variables first dose
selprop_fd <- data.frame(SelectionProportions(out_fd))
selprop_fd$variables <- rownames(selprop_fd)

hat_params_fd <- Argmax(out_fd)
print(paste0("lambda: ", hat_params_fd[1])) # lambda: 2.9642511375462"
print(paste0("pi: ", hat_params_fd[2])) # "pi: 0.81"


select_vars_fd <- selprop_fd[selprop_fd$SelectionProportions.out_fd. >= hat_params_fd[2],]




# ** Second Dose
y_seconddose <- england_clean_data_exlc_na$cumVaccPercentage_SecondDose

out_sd <- VariableSelection(
  xdata = x_predictors, 
  ydata = y_seconddose,
  verbose = FALSE,
  penalty.factor = c(rep(1, ncol(x_predictors))),
  family = "gaussian")

plot(out_sd)
CalibrationPlot(out_sd)


#png(filename = "out_sd_plot_adjusted.png", width = 1450, height = 1200, res = 300)
#par(mar = c(9, 4, 4, 2) + 0.1)
#plot(out_sd, cex.axis = 0.5, cex.lab = 0.7, cex.main = 0.8) 
#dev.off()


#png(filename = "calplot_sd.png", width = 2500, height = 2000, res = 300)
#par(mar = c(9, 9, 9, 9) + 0.1)
#CalibrationPlot(out_sd)
#dev.off()


# Selected variables second dose
selprop_sd <- data.frame(SelectionProportions(out_sd))
selprop_sd$variables <- rownames(selprop_sd)

hat_params_sd <- Argmax(out_sd)
print(paste0("lambda: ", hat_params_sd[1])) # lambda: 0.704633311983016"
print(paste0("pi: ", hat_params_sd[2])) # pi: 0.54


select_vars_sd <- selprop_fd[selprop_sd$SelectionProportions.out_sd. >= hat_params_sd[2],]






# ** Third Dose
y_thirddose <- england_clean_data_exlc_na$cumVaccPercentage_ThirdDose

out_td <- VariableSelection(
  xdata = x_predictors, 
  ydata = y_thirddose,
  verbose = FALSE,
  penalty.factor = c(rep(1, ncol(x_predictors))),
  family = "gaussian")

plot(out_td)
CalibrationPlot(out_td)


#png(filename = "out_td_plot_adjusted.png", width = 1450, height = 1200, res = 300)
#par(mar = c(9, 4, 4, 2) + 0.1)
#plot(out_td, cex.axis = 0.5, cex.lab = 0.7, cex.main = 0.8) 
#dev.off()


#png(filename = "calplot_td.png", width = 2500, height = 2000, res = 300)
#par(mar = c(9, 9, 9, 9) + 0.1)
#CalibrationPlot(out_td)
#dev.off()


# Selected variables third dose
selprop_td <- data.frame(SelectionProportions(out_td))
selprop_td$variables <- rownames(selprop_td)

hat_params_td <- Argmax(out_td)
print(paste0("lambda: ", hat_params_td[1])) # lambda:  0.498385941698144"
print(paste0("pi: ", hat_params_td[2])) # pi: 0.6


select_vars_td <- selprop_td[selprop_td$SelectionProportions.out_td. >= hat_params_td[2],]

stability_vars_doses <- unique(c(select_vars_fd$variables,select_vars_sd$variables,select_vars_td$variables))



# ** First v Second Dose
y_1v2 <- england_clean_data_exlc_na$d1_v_d2
str(y_1v2)

out_1v2 <- VariableSelection(
  xdata = x_predictors, 
  ydata = y_1v2,
  verbose = FALSE,
  penalty.factor = c(rep(1, ncol(x_predictors))),
  family = "gaussian")

plot(out_1v2)
CalibrationPlot(out_1v2)

#png(filename = "out_d1vd2.png", width = 1450, height = 1200, res = 300)
#par(mar = c(9, 4, 4, 2) + 0.1)
#plot(out_1v2, cex.axis = 0.5, cex.lab = 0.7, cex.main = 0.8) 
#dev.off()


#png(filename = "calplot_d1vd2.png", width = 2500, height = 2000, res = 300)
#par(mar = c(9, 9, 9, 9) + 0.1)
#CalibrationPlot(out_1v2)
#dev.off()

# Selected variables 1v2
selprop_1v2 <- data.frame(SelectionProportions(out_1v2))
selprop_1v2$variables <- rownames(selprop_1v2)

hat_params_1v2 <- Argmax(out_1v2)
print(paste0("lambda: ", hat_params_1v2[1])) # lambda: 0.11636521946672
print(paste0("pi: ", hat_params_1v2[2])) # pi: 0.56


select_vars_1v2 <- selprop_1v2[selprop_1v2$SelectionProportions.out_1v2. >= hat_params_1v2[2],]




# ** Second v Third Dose
y_2v3 <- england_clean_data_exlc_na$d2_v_d3

out_2v3 <- VariableSelection(
  xdata = x_predictors, 
  ydata = y_2v3,
  verbose = FALSE,
  penalty.factor = c(rep(1, ncol(x_predictors))),
  family = "gaussian")

plot(out_2v3)
CalibrationPlot(out_2v3)

#png(filename = "out_d2vd3.png", width = 1450, height = 1200, res = 300)
#par(mar = c(9, 4, 4, 2) + 0.1)
#plot(out_2v3, cex.axis = 0.5, cex.lab = 0.7, cex.main = 0.8) 
#dev.off()


#png(filename = "calplot_d2vd3.png", width = 2500, height = 2000, res = 300)
#par(mar = c(9, 9, 9, 9) + 0.1)
#CalibrationPlot(out_2v3)
#dev.off()

# Selected variables 2v3
selprop_2v3 <- data.frame(SelectionProportions(out_2v3))
selprop_2v3$variables <- rownames(selprop_2v3)

hat_params_2v3 <- Argmax(out_2v3)
print(paste0("lambda: ", hat_params_2v3[1])) # lambda: 0.679871071255746
print(paste0("pi: ", hat_params_2v3[2])) # pi: 0.82


select_vars_2v3 <- selprop_2v3[selprop_2v3$SelectionProportions.out_2v3. >= hat_params_sd[2],]

stability_vars_dropout <- unique(c(select_vars_1v2$variables,select_vars_2v3$variables))




# Script: Individual Doses Fixed Effects ----------------------------------

# ** First Dose
fd_lasso_simple <- multivar_model("cumVaccPercentage_FirstDose", stability_vars_doses, england_clean_data)

fd_lasso_simple <- fd_lasso_simple$Results
fd_lasso_simple <- fd_lasso_simple %>%
  mutate(
    dose = "First"
  )

# ** Second Dose
sd_lasso_simple <- multivar_model("cumVaccPercentage_SecondDose", stability_vars_doses, england_clean_data)

sd_lasso_simple <- sd_lasso_simple$Results
sd_lasso_simple <- sd_lasso_simple %>%
  mutate(
    dose = "Second"
  )

# ** Third Dose

td_lasso_simple <- multivar_model("cumVaccPercentage_ThirdDose", stability_vars_doses, england_clean_data)

td_lasso_simple <- td_lasso_simple$Results
td_lasso_simple <- td_lasso_simple %>%
  mutate(
    dose = "Third"
  )

# ** Combine into one DF

lasso_fixed <- rbind (fd_lasso_simple,sd_lasso_simple,td_lasso_simple)

lasso_fixed <- lasso_fixed %>%
  mutate (
    model = "Lasso",
    effects = "Fixed"
  )

head(lasso_fixed)

subset(lasso_fixed, lasso_fixed$Variable == "prop_u25")


# Script: Individual Doses Random Effects ---------------------------------

# ** First Dose
fd_lasso_rand <- lmm_model("cumVaccPercentage_FirstDose", stability_vars_doses,"RGN21CD", england_clean_data )

fd_lasso_rand <- fd_lasso_rand$Results
fd_lasso_rand <- fd_lasso_rand %>%
  mutate (
    dose = "First"
  )

# ** Second Dose
sd_lasso_rand <- lmm_model("cumVaccPercentage_SecondDose", stability_vars_doses,"RGN21CD", england_clean_data )

sd_lasso_rand <- sd_lasso_rand$Results
sd_lasso_rand <- sd_lasso_rand %>%
  mutate (
    dose = "Second"
  )

# ** Third Dose

td_lasso_rand <- lmm_model("cumVaccPercentage_ThirdDose", stability_vars_doses,"RGN21CD", england_clean_data )

td_lasso_rand <- td_lasso_rand$Results
td_lasso_rand <- td_lasso_rand %>%
  mutate (
    dose = "Third"
  )


# ** Combine into one DF

lasso_random <- rbind(fd_lasso_rand, sd_lasso_rand, td_lasso_rand)
lasso_random <- lasso_random %>%
  mutate(
    model = "Lasso",
    effects = "Random"
  )

head(lasso_random)
subset(lasso_random, lasso_random$Variable == "prop_u25")




# Save as CSV -------------------------------------------------------------

lasso_models <- rbind(lasso_fixed,lasso_random)
write.csv(lasso_models, "../results_tables/stability_model_results_final.csv")



# Script: Dropout Fixed Effects -------------------------------------------

# ** First to Second 
lasso_simple_1v2 <- multivar_model("d1_v_d2", stability_vars_dropout, england_clean_data)

lasso_simple_1v2 <- lasso_simple_1v2$Results
lasso_simple_1v2 <- lasso_simple_1v2 %>%
  mutate(
    dose = "First vs Second"
  )


# ** Second to Third
lasso_simple_2v3 <- multivar_model("d2_v_d3", stability_vars_dropout, england_clean_data)

lasso_simple_2v3 <- lasso_simple_2v3$Results
lasso_simple_2v3 <- lasso_simple_2v3 %>%
  mutate(
    dose = "Second vs Third"
  )


# ** Combine into one DF

lasso_fixed_dropout <- rbind (lasso_simple_1v2,lasso_simple_2v3)

lasso_fixed_dropout <- lasso_fixed_dropout %>%
  mutate (
    model = "Stability Lasso",
    effects = "Fixed"
  )

head(lasso_fixed_dropout)
subset(lasso_fixed_dropout,lasso_fixed_dropout$Variable == "prop_u25")



# Script: Dropout Random Effects ------------------------------------------

# ** First to Second
lasso_rand_1v2 <- lmm_model("d1_v_d2", stability_vars_dropout,"RGN21CD", england_clean_data )

lasso_rand_1v2 <- lasso_rand_1v2$Results
lasso_rand_1v2 <- lasso_rand_1v2 %>%
  mutate (
    dose = "First vs Second"
  )

# ** Second to Third

lasso_rand_2v3 <- lmm_model("d2_v_d3", stability_vars_dropout,"RGN21CD", england_clean_data )

lasso_rand_2v3 <- lasso_rand_2v3$Results
lasso_rand_2v3 <- lasso_rand_2v3 %>%
  mutate (
    dose = "Second vs Third"
  )

# ** Combine into one DF
lasso_random_dropout <- rbind(lasso_rand_1v2, lasso_rand_2v3)
lasso_random_dropout <- lasso_random_dropout %>%
  mutate(
    model = "Stability Lasso",
    effects = "Random"
  )

head(lasso_random_dropout)
subset(lasso_random_dropout,lasso_random_dropout$Variable == "prop_u25")
subset(lasso_random_dropout,lasso_random_dropout$Variable == "prop_o65")



# Save as CSV -------------------------------------------------------------
# ** Combine Fixed and Random
lasso_models <- rbind(lasso_fixed_dropout,lasso_random_dropout)
write.csv(lasso_models, "../results_tables/stability_between_doses_final.csv")




