
# **Data Standardisation and U25 Extraction Refactored Code**


# General Libraries -------------------------------------------------------

library(dplyr)
library(tidyr)
library(tidyverse)
library(readxl)
library(lme4)
library(nlme)
library(ggplot2)
library(broom.mixed)
library(table1)
library(gt)
library(patchwork)
library(sf)
library(spdep)
library(ggplot2)
library(gridExtra)
library(cowplot)
library(viridis)
library(ggpubr)

# Clear environment and set working directory -----------------------------

rm(list = ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


# Reusable Functions ------------------------------------------------------

## Function to Standardise Data Columns
standardise_columns <- function(data, columns) {
  data %>%
    mutate(across(
      all_of(columns),
      ~ (.-mean(.x, na.rm = TRUE)) / sd(.x, na.rm = TRUE)
    ))
}

## Function to Clean Column Names
clean_column_names <- function(df) {
  names(df) <- names(df) %>%
    str_replace_all(" ", "_") %>%
    str_replace_all(",", "") %>%
    str_replace_all("-", "") %>%
    str_replace_all("_\\(rate\\)", "") %>%
    str_replace_all("__Average", "")
  df
}




# Data --------------------------------------------------------------------

mye2_per <- read_excel(
  "../data/Source_files/ukpopestimatesmid2020on2021geography.xlsx",
  sheet = "MYE2 - Persons",
  skip = 7
)

cases_data <- read.csv("../data/Cases_Data.csv")

ltla_to_region <- read.csv("../data/LTLA_to_Region.csv")

iod_file_path <- "../data/Source_files/File_10_-_IoD2019_Local_Authority_District_Summaries__lower-tier__.xlsx"



# Script: U25 Data Extraction ---------------------------------------------

# Extract and Aggregate Relevant Columns
u25_selection <- mye2_per %>%
  select(1:30) %>%  # Adjust column indices to match actual data structure
  mutate(tot_u25 = rowSums(select(., 5:30), na.rm = TRUE))

# Calculate proportion of u25
prop_u25 <- u25_selection %>%
  mutate(prop_u25 = tot_u25 / `All ages`) %>%
  select(Code, prop_u25)




# Script: IoD Domains -----------------------------------------------------

# Extract required IoD data 
iod_sheets <- c("Income", "Employment", "Education", "Health", "Crime", "Barriers", "Living") # The sheets of interest


iod_data <- lapply(iod_sheets, function(sheet) {
  read_excel(iod_file_path, sheet = sheet) %>%
    select("Local Authority District code (2019)", contains("- Average score"))
})
names(iod_data) <- iod_sheets # Name each data frame in the list

merged_iod_data <- Reduce(function(x, y) merge(x, y, by = "Local Authority District code (2019)"), iod_data) # Combine all IoD data into one data frame



# Merge and Clean Data
region <- cases_data %>% left_join(ltla_to_region, by = c("areaCode" = "LAD21CD"))

region_IMD <- region %>%
  left_join(merged_iod_data, by = c("areaCode" = "Local Authority District code (2019)")) 

# Clean Column Names
region_IMD <- clean_column_names(region_IMD)

# Add Regional Names
region_IMD <- region_IMD %>% mutate(rgn_nm = case_when(
  RGN21CD == "E12000001" ~ "North East",
  RGN21CD == "E12000002" ~ "North West",
  RGN21CD == "E12000003" ~ "Yorkshire and The Humber",
  TRUE ~ "Other"
))




# Script: Merge U25 Data --------------------------------------------------

data_final <- left_join(region_IMD, prop_u25, by = c("areaCode" = "Code"))

# Script: Covert Funding to Per Capita ------------------------------------

# ** From data dictionary unringfenced to tax_income_guarantee_business_rates covid specific special emergency funding
# ** All in £ millions
# ** Focus on COMF, ASC, Unringfenced

# Wanted variables
funding_cols <- data_final %>%
  select(unringfenced:tax_income_guarantee_business_rates, Population)



cols_to_convert <- colnames(funding_cols)

for (col in cols_to_convert) {
  data_final[[paste0(col, "_percapita")]] <- (data_final[[col]]*1000000) / data_final$Population
}

# Summarise one of the new per capita columns
summary(data_final$unringfenced_percapita)




# Script: Limiting Area Codes ---------------------------------------------

# ** As seen in EDA (01_exploratory_analysis file) Scotland and Wales high missing
# ** Therefore only going to focus on England

england <- subset(data_final, grepl("^[E]", areaCode))



# Script: Managing Time Varying Covariates --------------------------------

# ** This analysis will only considers final recorded % of vaccine uptake
# ** Taking average of time varying covariates 

baseline_columns <- grep("_baseline$", names(england), value = TRUE) # time varying

missing_values_by_week <- region_IMD %>%
  group_by(Week) %>%
  summarise(
    Total_Missing_Values = sum(across(everything(), ~ sum(is.na(.))))
  ) # Identifying week with most complete data

# Print the resulting table
print(missing_values_by_week)
plot(missing_values_by_week) # The last 2 weeks high missingness compared to others
  # So we will take week 129 as our end point (16 Oct 2022)



england_final <- england %>%
  group_by(areaCode) %>%
  mutate(across(all_of(baseline_columns), ~ if_else(Week <= 129, mean(.x[Week <= 129], na.rm = TRUE), .x)))



# Script: Limiting Week ---------------------------------------------------

england_clean_data <- subset(england_final, england_final$Week == 129)

summary(england_clean_data)


# Script: Cleaning Region Names -------------------------------------------
# ** Name the regions for easy identification
england_clean_data <- england_clean_data[!is.na(england_clean_data$RGN21CD), ]
england_clean_data$rgn_nm <- ifelse(england_clean_data$RGN21CD == "E12000001", "North East",
                           ifelse(england_clean_data$RGN21CD == "E12000002","North West",
                                  ifelse(england_clean_data$RGN21CD == "E12000003", "Yorkshire and The Humber",
                                         ifelse(england_clean_data$RGN21CD == "E12000004","East Midlands",
                                                ifelse(england_clean_data$RGN21CD == "E12000005", "West Midlands",
                                                       ifelse(england_clean_data$RGN21CD == "E12000006","East of England",
                                                              ifelse(england_clean_data$RGN21CD == "E12000007","London",
                                                                     ifelse(england_clean_data$RGN21CD == "E12000008", "South East",
                                                                            ifelse(england_clean_data$RGN21CD=="E12000009","South West","Other")))))))))
# Script: Calculating Drop Out --------------------------------------------
# ** Vaccine uptake not independent from each other
# ** So also evaluating drop out from the first to second and second to third

england_clean_data$d1_v_d2 = ((england_clean_data$cumVaccPercentage_FirstDose - england_clean_data$cumVaccPercentage_SecondDose) / england_clean_data$cumVaccPercentage_FirstDose ) * 100
england_clean_data$d2_v_d3 = ((england_clean_data$cumVaccPercentage_SecondDose - england_clean_data$cumVaccPercentage_ThirdDose) / england_clean_data$cumVaccPercentage_SecondDose) * 100




# Save as CSV (No standardising) ------------------------------------------


write.csv(england_clean_data, "../data/england_clean_data.csv")




# Script: Standardising Data ----------------------------------------------

vars <- c("Population",                                               
          "Pop_per_km2",                                              
          "Median_age",                                             
          "prop_travelling_to_work",                                 
          "prop_not_in_work",                                 
          "prop_white_british",                                       
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

# Data frame with vars to standardise

england_clean_data <- read.csv("../data/england_clean_data.csv")

england_clean_std <- england_clean_data[ , names(england_clean_data) %in% vars]

summary(england_clean_std)



# Calculate mean and standard deviation for each column
summary_stats <- england_clean_std %>%
  summarise(across(everything(), list(mean = ~mean(.x, na.rm = TRUE), sd = ~sd(.x, na.rm = TRUE))))

# Pivot the data to have one column for mean, one for sd, and variables as rows
long_summary_stats <- summary_stats %>%
  pivot_longer(cols = everything(),
               names_to = "variable_stat",
               values_to = "value") %>%
  separate(variable_stat, into = c("variable", "stat"), sep = "_(?=[^_]+$)") %>%
  pivot_wider(names_from = stat, values_from = value)

# Display the result
View(long_summary_stats)

# Now standardise
england_clean_std <- england_clean_data %>% mutate_at(vars, ~(scale(.) %>% as.vector))


# Now do a manual check 
head(england_clean_data) 
# row 1, Hartlepool population = 93836 , so standardised = (93836	 - 1.842963e+05) / 1.239487e+05 = -0.7298205
head(england_clean_std) # a very close match 

#save as csv
write.csv(england_clean_std, "../data/england_clean_std.csv")

