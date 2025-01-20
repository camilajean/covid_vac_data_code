
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

ltla_to_region <- read.csv("../data/LTLA_to_Region.csv")

LAD_IoD19_Income <- read_excel("../data/Source_files/File_10_-_IoD2019_Local_Authority_District_Summaries__lower-tier__.xlsx", sheet = "Income")
LAD_IoD19_Employment <- read_excel("../data/Source_files/File_10_-_IoD2019_Local_Authority_District_Summaries__lower-tier__.xlsx", sheet = "Employment")
LAD_IoD19_Education <- read_excel("../data/Source_files/File_10_-_IoD2019_Local_Authority_District_Summaries__lower-tier__.xlsx", sheet = "Education")
LAD_IoD19_Health <- read_excel("../data/Source_files/File_10_-_IoD2019_Local_Authority_District_Summaries__lower-tier__.xlsx", sheet = "Health")
LAD_IoD19_Crime <- read_excel("../data/Source_files/File_10_-_IoD2019_Local_Authority_District_Summaries__lower-tier__.xlsx", sheet = "Crime")
LAD_IoD19_Barriers <- read_excel("../data/Source_files/File_10_-_IoD2019_Local_Authority_District_Summaries__lower-tier__.xlsx", sheet = "Barriers")
LAD_IoD19_Living <- read_excel("../data/Source_files/File_10_-_IoD2019_Local_Authority_District_Summaries__lower-tier__.xlsx", sheet = "Living")

LSOA_IoD19 <- read_excel("../data/File_5_-_IoD2019_Scores.xlsx", sheet = "IoD2019 Scores")

LSOA_Iod_Codes <- read.csv("../data/source_files/LSOA_(2021)_to_Built_Up_Area_to_Local_Authority_District_to_Region_(December_2022)_Lookup_in_England_and_Wales_v2.csv")


# Script: U25 Data Extraction ---------------------------------------------


# Extract and Aggregate Relevant Columns
u25_selection <- mye2_per %>%
  select(1:30) %>%  # Adjust column indices to match actual data structure
  mutate(tot_u25 = rowSums(select(., 5:30), na.rm = TRUE))


# Calculate proportion of u25
prop_u25 <- u25_selection %>%
  mutate(prop_u25 = tot_u25 / `All ages`) %>%
  select(Code, prop_u25)



# Merge files -------------------------------------------------------------

ltla_to_region <- ltla_to_region %>%
  select("LAD21CD","RGN21CD","RGN21NM")


region <- cases_data %>%
  left_join(ltla_to_region, by = c("areaCode" = "LAD21CD"))


data_final <- left_join(region, prop_u25, by = c("areaCode" = "Code"))

# Script: IoD Domains -----------------------------------------------------

# Most deprived have a higher score
LAD_IoD19_Barriers <- LAD_IoD19_Barriers[c("Local Authority District code (2019)","Barriers to Housing and Services - Average score")]
LAD_IoD19_Crime <- LAD_IoD19_Crime[c("Local Authority District code (2019)","Crime - Average score")]
LAD_IoD19_Education <- LAD_IoD19_Education[c("Local Authority District code (2019)","Education, Skills and Training - Average score")]
LAD_IoD19_Employment <- LAD_IoD19_Employment[c("Local Authority District code (2019)","Employment - Average score")]
LAD_IoD19_Health <- LAD_IoD19_Health[c("Local Authority District code (2019)","Health Deprivation and Disability - Average score")]
LAD_IoD19_Income <- LAD_IoD19_Income[c("Local Authority District code (2019)","Income - Average score")]
LAD_IoD19_Living <- LAD_IoD19_Living[c("Local Authority District code (2019)" ,"Living Environment - Average score")]

LSOA_IoD19 <- LSOA_IoD19[c("LSOA code (2011)","Local Authority District code (2019)")]

LSOA_Iod_Codes <- LSOA_Iod_Codes[c("LSOA21CD","LAD22CD")]

data_frames <- list(LAD_IoD19_Barriers, LAD_IoD19_Crime, LAD_IoD19_Education, 
                    LAD_IoD19_Employment, LAD_IoD19_Health, LAD_IoD19_Income, LAD_IoD19_Living)

# Merge all data frames on "Local Authority District code (2019)" using Reduce and merge
merged_data <- Reduce(function(x, y) merge(x, y, by = "Local Authority District code (2019)"), data_frames)

codes <- merge(LSOA_IoD19,LSOA_Iod_Codes, by.x = "LSOA code (2011)",by.y = "LSOA21CD", all.x = TRUE)

merge_data_codes <- merge(merged_data,codes, by.x = "Local Authority District code (2019)", by.y = "Local Authority District code (2019)", all.x = TRUE )

region_IMD <- merge(data_final, merge_data_codes, by.x = "areaCode", by.y = "LAD22CD", all.x = TRUE)

unmatched_area_codes <- setdiff(data_final$areaCode, merge_data_codes$LAD22CD)
print(unmatched_area_codes)


# Drop the specified columns
region_IMD <- region_IMD %>%
  select(-`LSOA code (2011)`, -`Local Authority District code (2019)`) 

# Remove identical rows
region_IMD <- region_IMD %>%
  distinct()

# View the cleaned dataframe
head(region_IMD)


# Limiting area code ------------------------------------------------------
# Limiting to only England to account for observation in EDA
region_IMD <- filter(region_IMD,str_starts(areaCode, "E"))



# Script: Managing Time Varying Covariates --------------------------------

# ** Baseline variables
# Step 1: Identify columns that end with '_baseline'
baseline_columns <- grep("_baseline$", names(region_IMD), value = TRUE)

# Step 2: Calculate the mean for each unique LTLA up to week 129
region_IMD_av <- region_IMD %>%
  group_by(areaCode) %>%
  mutate(across(all_of(baseline_columns), ~ if_else(Week <= 129, mean(.x[Week <= 129], na.rm = TRUE), .x)))





# Script: Limiting Week ---------------------------------------------------


week_129 <- region_IMD_av[region_IMD_av$Week == "129",]

unique(week_129$date_begin)




# Script: Cleaning Region Names -------------------------------------------

head(week_129)

colnames(week_129)
names(week_129) <- gsub(" ", "_", names(week_129))
names(week_129) <- gsub(",", "", names(week_129))
names(week_129) <- gsub("-", "", names(week_129))
names(week_129) <- gsub("_\\(rate\\)", "", names(week_129))
names(week_129) <- gsub("__Average", "", names(week_129))

# Display updated column names
print("Updated column names:")
print(names(week_129))

# Name the regions for easy identification
rgn_wk129 <- week_129[!is.na(week_129$RGN21CD), ]
rgn_wk129$rgn_nm <- ifelse(rgn_wk129$RGN21CD == "E12000001", "North East",
                           ifelse(rgn_wk129$RGN21CD == "E12000002","North West",
                                  ifelse(rgn_wk129$RGN21CD == "E12000003", "Yorkshire and The Humber",
                                         ifelse(rgn_wk129$RGN21CD == "E12000004","East Midlands",
                                                ifelse(rgn_wk129$RGN21CD == "E12000005", "West Midlands",
                                                       ifelse(rgn_wk129$RGN21CD == "E12000006","East of England",
                                                              ifelse(rgn_wk129$RGN21CD == "E12000007","London",
                                                                     ifelse(rgn_wk129$RGN21CD == "E12000008", "South East",
                                                                            ifelse(rgn_wk129$RGN21CD=="E12000009","South West","Other")))))))))


# Script: Covert Funding to Per Capita ------------------------------------

# ** From data dictionary unringfenced to tax_income_guarantee_business_rates covid specific special emergency funding
# ** All in £ millions
# ** Focus on COMF, ASC, Unringfenced

head(rgn_wk129)
colnames(rgn_wk129)

# Calculate funding per captia
# Get required columns: Population and funding cols
# Multiply funding cols x1,000,000 to get into true values
# Divide funding by population

check_fund <- data.frame(
  unringfenced = rgn_wk129$unringfenced,
  ASC_infection_control_fund = rgn_wk129$ASC_infection_control_fund,
  contain_outbreak_management = rgn_wk129$contain_outbreak_management,
  
  Core_services_funding = rgn_wk129$Core_services_funding, # already true value 
  Primary_care_funding = rgn_wk129$Primary_care_funding, # already true value 
  Specialised_services = rgn_wk129$Specialised_services, # already true value 
  CEV_fund = rgn_wk129$CEV_fund,
  welcome_back_fund = rgn_wk129$welcome_back_fund,
  DWP_winter_grant = rgn_wk129$DWP_winter_grant,
  omicron_support_fund = rgn_wk129$omicron_support_fund
)

summary(check_fund)


rgn_wk129 <- rgn_wk129 %>%
  mutate(
    unringfenced = unringfenced * 1000000,
    ASC_infection_control_fund = ASC_infection_control_fund * 1000000,
    contain_outbreak_management = contain_outbreak_management * 1000000,
    CEV_fund = CEV_fund * 1000000,
    welcome_back_fund = welcome_back_fund * 1000000,
    DWP_winter_grant = DWP_winter_grant * 1000000,
    omicron_support_fund = omicron_support_fund * 1000000
  )

summary(rgn_wk129$contain_outbreak_management)


# Calculate per capita

rgn_wk129 <- rgn_wk129 %>%
  mutate(
    unringfenced_percapita = unringfenced / Population,
    ASC_infection_control_fund_percapita = ASC_infection_control_fund / Population,
    contain_outbreak_management_percapita = contain_outbreak_management / Population,
    Core_services_funding_percapita = Core_services_funding / Population ,
    Primary_care_funding_percapita = Primary_care_funding / Population,
    Specialised_services_percapita = Specialised_services / Population,
    CEV_fund_percapita = CEV_fund / Population,
    welcome_back_fund_percapita = welcome_back_fund / Population,
    DWP_winter_grant_percapita = DWP_winter_grant / Population,
    omicron_support_fund_percapita = omicron_support_fund / Population
  )

summary(rgn_wk129$unringfenced_percapita)




# Save as CSV (No standardising) ------------------------------------------

write.csv(rgn_wk129, "../data/england_data_clean.csv")

# Script: Standardising Data ----------------------------------------------

# Define the variables to standardise
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
          "Living_Environment_score",
          "prop_u25")

# Data frame with vars to standardise

#england_clean_data <- read.csv( "../data/england_data_clean.csv")

england_clean_data <- rgn_wk129

england_clean_std <- england_clean_data[ , names(england_clean_data) %in% vars]


for (var in vars) {
  mean_val <- mean(england_clean_std[[var]], na.rm = TRUE)
  sd_val <- sd(england_clean_std[[var]], na.rm = TRUE)
  
  # Print the mean and standard deviation for each variable
  cat("Variable:", var, "\nMean:", mean_val, "\nSD:", sd_val, "\n\n")
}



# Create a copy of the original dataset to store the standardised values
standardised_data <- england_clean_data

# Iterate over the variables
for (var in vars) {
  # Calculate mean and standard deviation for the current variable
  mean_val <- mean(england_clean_data[[var]], na.rm = TRUE)
  sd_val <- sd(england_clean_data[[var]], na.rm = TRUE)

    standardised_data[[var]] <- (england_clean_data[[var]] - mean_val) / sd_val
 
}

# Manual check of data
head(standardised_data)
head(england_clean_data)

mean(england_clean_data$Population)
sd(england_clean_data$Population)

mean(england_clean_data$prop_u25)
sd(england_clean_data$prop_u25)

# Z-Score = (Value - Mean) / SD
# Hartlepool Population (93836 - 190766.5) / 129501.6 = -0.7298205  -0.74848830
# Hartlepool prop_u25 (0.3023147 - 0.2999043 ) / 0.03627863 = 0.06644132
# Exact match to 6dp




# Add dropoout ------------------------------------------------------------

standardised_data$d1_v_d2 = ((standardised_data$cumVaccPercentage_FirstDose - standardised_data$cumVaccPercentage_SecondDose) / standardised_data$cumVaccPercentage_FirstDose ) * 100
standardised_data$d2_v_d3 = ((standardised_data$cumVaccPercentage_SecondDose - standardised_data$cumVaccPercentage_ThirdDose) / standardised_data$cumVaccPercentage_SecondDose) * 100



#save as csv
write.csv(standardised_data, "../data/england_clean_std.csv")



