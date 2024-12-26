# **Exploratory Analysis and Filtering Code**


# General Libraries -------------------------------------------------------

library(ggplot2)
library(VIM)
library(dplyr)
library(tidyr)
library(reshape2)
library(RColorBrewer)


# Clear environment and set working directory -----------------------------

rm(list = ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Data --------------------------------------------------------------------
data <- read.csv("../data/Cases_Data.csv")


# Cumulative Vaccine Uptake -----------------------------------------------

# See maximum cumulative vaccination for each dose
summary_results <- data %>%
  summarise(across(starts_with("cumVaccPercentage"), summary))

View(summary_results)

# See at which week each vaccine reached its maximum
data_vaccine <- data %>%
  gather(key = "Type", value = "cumPercentage", cumVaccPercentage_FirstDose, cumVaccPercentage_SecondDose, cumVaccPercentage_ThirdDose)


data_mean <- data_vaccine %>%
  group_by(Week, Type) %>%
  summarise(MeanPercentage = mean(cumPercentage, na.rm = TRUE)) %>%
  ungroup()


ggplot(data_vaccine, aes(x = Week, y = cumPercentage, color = Type)) +
  geom_point() +
  scale_color_brewer(palette = "Set2", labels = c("First Dose", "Second Dose", "Third Dose")) +  
  theme_minimal() +
  labs(title = "Cumulative vaccine percentage WoW", x = "Week", y = "Percent") +
  geom_hline(yintercept = 80, linetype = "dashed", color = "red")


# Preprocess the data
cumulative_vaccine_data <- data %>%
  mutate(areaCode_group = substr(areaCode, 1, 1)) # Extract first letter of areaCode

# Plot the cumulative vaccine uptake first dose
ggplot(cumulative_vaccine_data, aes(x = Week, y = cumVaccPercentage_FirstDose, colour = areaCode_group, group = areaCode_group)) +
  geom_line(size = 1) +
  labs(
    title = "Cumulative Vaccine Uptake by Week",
    x = "Week",
    y = "Cumulative Vaccine Uptake",
    colour = "Area Code Group"
  ) +
  theme_minimal()

# Plot the cumulative vaccine uptake second dose
ggplot(cumulative_vaccine_data, aes(x = Week, y = cumVaccPercentage_SecondDose, colour = areaCode_group, group = areaCode_group)) +
  geom_line(size = 1) +
  labs(
    title = "Cumulative Vaccine Uptake by Week",
    x = "Week",
    y = "Cumulative Vaccine Uptake",
    colour = "Area Code Group"
  ) +
  theme_minimal()

# Plot the cumulative vaccine uptake third dose
ggplot(cumulative_vaccine_data, aes(x = Week, y = cumVaccPercentage_ThirdDose, colour = areaCode_group, group = areaCode_group)) +
  geom_line(size = 1) +
  labs(
    title = "Cumulative Vaccine Uptake by Week",
    x = "Week",
    y = "Cumulative Vaccine Uptake",
    colour = "Area Code Group"
  ) +
  theme_minimal()





# Missing Data ------------------------------------------------------------

#Exploring dimentions, missing values, duplicates
dim(data) 
str(data)

colSums(is.na(data)) 
sum(is.na(data)) #783934 missing values

length(unique(data$areaCode)) #356 unique LTAs
length(unique(data$CCG_2019_Code)) # 184 CCG
length(unique(data$Week)) # 130 weeks

# Total missing data

missing_percent <- sapply(data, function(x) mean(is.na(x)) * 100)

missing_percent <- missing_percent[missing_percent > 0]

missing_df <- data.frame(Variable = names(missing_percent), Percent = missing_percent)

dim(missing_df)


ggplot(missing_df, aes(x = Percent, y = Variable)) +
  geom_segment(aes(x = 0, xend = Percent, y = Variable, yend = Variable), color = "skyblue", size = 1) +  # Draw lines
  geom_point(aes(size = Percent), color = "blue") +  # Add points
  scale_size(range = c(2,2)) +  # Adjust size scale for visibility
  labs(title = "Percentage of Missing Values per Variable",
       x = "Percentage of Missing Data",
       y = "Variables") +
  theme_minimal() +
  theme(legend.position = "none")  # Remove legend


# Missing Data for E / S / W 
England <- subset(data, grepl("^[E]", areaCode))
length(unique(England$areaCode))
(sum(is.na(England)) / length(unlist(England)))*100 #0.04% missing


Scotland <-subset(data, grepl("^[S]", areaCode))
length(unique(Scotland$areaCode))
(sum(is.na(Scotland)) / length(unlist(Scotland)))*100 #60% missing  


Wales <- subset(data, grepl("^[W]", areaCode))
length(unique(Wales$areaCode))
(sum(is.na(Wales)) / length(unlist(Wales)))*100 #46% missing



# Calculate the sum of missing values for each week
missing_values_by_week <- data %>%
  group_by(Week) %>%
  summarise(
    Total_Missing_Values = sum(across(everything(), ~ sum(is.na(.))))
  )

# Print the resulting table
plot(missing_values_by_week)







