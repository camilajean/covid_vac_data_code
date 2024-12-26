# **Exploring the Data to be used in the analysis**


# General Libraries -------------------------------------------------------

library(dplyr)
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



# Load in Data ------------------------------------------------------------

england_clean_data <- read.csv("../data/england_clean_data.csv")


# Max Uptake Per Dose Density ---------------------------------------------

density_1 <- ggplot(england_clean_data) +
  geom_density(aes(x = cumVaccPercentage_FirstDose, fill = "First Dose"), alpha = 0.5, color = "black") +
  geom_density(aes(x = cumVaccPercentage_SecondDose, fill = "Second Dose"), alpha = 0.5, color = "black") +
  geom_density(aes(x = cumVaccPercentage_ThirdDose, fill = "Third Dose"), alpha = 0.5, color = "black") +
  labs(x = "Max Cumulative Vaccination (%)",
       y = "Density") +
  scale_fill_manual(values = c("First Dose" = "#ADD8E6", "Second Dose" = "#90EE90", "Third Dose" = "#FFB6C1")) +
  theme_minimal() +
  theme(legend.title = element_blank()) +
  facet_wrap("rgn_nm")

print(density_1)

ggsave("../figures/png/regional_density.png", width = 9,height = 7)
ggsave("../figures/tiff/regional_density.tiff", width = 9,height = 7)
ggsave("../figures/pdf/regional_density.pdf", width = 9,height = 7)



# Max Uptake Per Dose Maps ------------------------------------------------

LocalAuthorities = st_read("../data/LAD_may22/LAD_MAY_2022_UK_BFE_V3.shp") %>%
  filter(str_starts(LAD22CD, "E"))
LA_nb = poly2nb(LocalAuthorities, queen=TRUE)
summary(LA_nb)

combined <- england_clean_data %>%
  rename(LAD22CD = areaCode)

mapped_data <- LocalAuthorities %>%
  left_join(combined, by = "LAD22CD")


# Define the types you want to plot
types <- c("cumVaccPercentage_FirstDose", "cumVaccPercentage_SecondDose", "cumVaccPercentage_ThirdDose")

# Extract the legend from one of the plots
legend <- get_legend(
  ggplot(mapped_data, aes(fill = .data[[types[1]]])) +
    geom_sf(color = "black") +
    scale_fill_viridis_c(
      name = "Percentage", 
      option = "rocket",  # Use the "rocket" color palette
      limits = c(30, 100), 
      breaks = seq(30, 100, by = 10), 
      labels = scales::label_percent(scale = 1)  # Display as percentage, no degree symbol
    ) +
    theme(
      legend.position = "right",
      legend.key.size = unit(1.2, "cm"),  # Increase size of the legend keys
      legend.text = element_text(size = 10),  # Increase text size of the legend labels
      legend.title = element_text(size = 10, face = "bold")  # Increase title size and make it bold
    )
)

# Create the plots without legends

map_plots <- list()
for (t in types) {
  dose_plot <- ggplot() +
    geom_sf(data = LocalAuthorities, fill = "white", color = "black") +  # Base layer with all LTAs
    geom_sf(data = mapped_data, aes(fill = .data[[t]]), color = "black") +  # Overlay with vaccination data
    scale_fill_viridis_c(
      name = "Percentage", 
      option = "rocket",  # Use the "rocket" color palette
      limits = c(30, 100), 
      breaks = seq(30, 100, by = 10), 
      labels = scales::label_number(scale = 1, suffix = "%")  # Add percentage symbol, no degree
    ) +
    theme_bw() +
    theme(
      text = element_text(size = 8),
      legend.position = "none",
      plot.margin = unit(c(1, 1, 1, 1), "mm"),  # Reduce margins to bring labels closer
      axis.title.x = element_blank(),  # Remove x-axis label
      axis.title.y = element_blank(),  # Remove y-axis label
      axis.text.x = element_blank(),  # Remove x-axis text
      axis.text.y = element_blank()   # Remove y-axis text
    )
  
  # Store the plot in the list
  map_plots[[t]] <- dose_plot
}

map_1 <- map_plots$cumVaccPercentage_FirstDose
map_2 <- map_plots$cumVaccPercentage_SecondDose
map_3 <- map_plots$cumVaccPercentage_ThirdDose


# Isolating London

# Filter and merge Hackney and City of London
hackney_city <- LocalAuthorities %>%
  filter(LAD22NM %in% c("Hackney", "City of London")) %>%
  summarize(geometry = st_union(geometry),
            LAD22NM = "Hackney and City of London", 
            LAD22CD = "E09000012")  # Use Hackney's LAD code for simplicity

# Remove the old Hackney and City of London entries from the original dataset
LocalAuthorities_merged <- LocalAuthorities %>%
  filter(!LAD22NM %in% c("Hackney", "City of London")) %>%
  bind_rows(hackney_city)

# Filter data for the London region
london <- subset(england_clean_data, england_clean_data$RGN21NM == "london")

# Filter the shapefile for London using merged data
LocalAuthorities_lon <- LocalAuthorities_merged %>%
  filter(LAD22CD %in% london$areaCode)

# Create neighborhood structure after merging
LA_nb = poly2nb(LocalAuthorities_lon, queen=TRUE)

# Ensure the vaccination data matches the LAD24CD from the spatial data
combined_lon <- london %>%
  mutate(areaCode = ifelse(areaName == "Hackney and City of London", "E09000012", areaCode))

# Join the spatial data with the vaccination data
mapped_data_lon <- LocalAuthorities_lon %>%
  left_join(combined_lon, by = c("LAD22CD" = "areaCode"))

# Check the result to ensure the join was successful
print(mapped_data_lon)


map_plots_lon <- list()
for (t in types) {
  dose_plot <- ggplot() +
    geom_sf(data = LocalAuthorities_lon, fill = "white", color = "black") +  # Base layer with all LTAs
    geom_sf(data = mapped_data_lon, aes(fill = .data[[t]]), color = "black") +  # Overlay with vaccination data
    scale_fill_viridis_c(
      name = "Percentage", 
      option = "rocket",  # Use the "rocket" color palette
      limits = c(30, 100), 
      breaks = seq(30, 100, by = 10), 
      labels = scales::label_number(scale = 1, suffix = "%")  # Add percentage symbol, no degree
    ) +
    theme_bw() +
    theme(
      text = element_text(size = 8),
      legend.position = "none",
      plot.margin = unit(c(1, 1, 1, 1), "mm"),  # Reduce margins to bring labels closer
      axis.title.x = element_blank(),  # Remove x-axis label
      axis.title.y = element_blank(),  # Remove y-axis label
      axis.text.x = element_blank(),  # Remove x-axis text
      axis.text.y = element_blank()   # Remove y-axis text
    )
  
  # Store the plot in the list
  map_plots_lon[[t]] <- dose_plot
}


# Plot the vaccination data on the map
ggplot() +
  geom_sf(data = mapped_data_lon, aes(fill = cumVaccPercentage_FirstDose), color = "white") +
  scale_fill_viridis_c(
    name = "Percentage", 
    option = "rocket", 
    limits = c(30, 100), 
    breaks = seq(30, 100, by = 10), 
    labels = scales::label_number(scale = 1, suffix = "%"),
    na.value = "grey50"  # Ensure any missing data is filled with a neutral color
  ) +
  theme_bw() +
  theme(
    text = element_text(size = 8),
    legend.position = "none",
    plot.margin = unit(c(1, 1, 1, 1), "mm"),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank()
  )


lon_1 <- map_plots_lon$cumVaccPercentage_FirstDose
lon_2 <- map_plots_lon$cumVaccPercentage_SecondDose
lon_3 <- map_plots_lon$cumVaccPercentage_ThirdDose

# Merge maps 
plot_A <- ggarrange(map_1, lon_1, ncol = 2)

plot_B <- ggarrange(map_2, lon_2, ncol = 2)

plot_C <- ggarrange(map_3, lon_3, ncol = 2)

combined_plot <- ggarrange(
  plot_A, plot_B, plot_C,
  nrow = 3, 
  labels = c("A", "B", "C"),
  common.legend = TRUE,
  legend = "right",  # Position the legend on the right
  label.y = 1,  # Adjust the position of the labels
  font.label = list(size = 25, face = "bold")# Make the labels larger and bold
)

combined_plot_with_legend <- plot_grid(
  plot_grid(plot_A, plot_B, plot_C, nrow = 3, labels = c("A", "B", "C")),
  legend,
  ncol = 2,  # Place legend to the right
  rel_widths = c(2, 0.3)  # Adjust the relative widths, making the right side larger
)



# Save the combined plot to a file
ggsave("../figures/png/maps_comb.png", plot = combined_plot_with_legend, height = 9, width = 8)
ggsave("../figures/tiff/maps_comb.tiff", plot = combined_plot_with_legend, height = 9, width = 8)


# Load the PNG file
img <- png::readPNG("../figures/png/maps_comb.png")
pdf("../figures/pdf/maps_comb.pdf", width = 8, height = 6)  
grid.raster(img)
dev.off()

# Summary Stats -----------------------------------------------------------

summary(england_clean_data)


vars <- c(                                               
  "Pop_per_km2",                                              
  "prop_all_other_white",                                     
  "prop_mixed_multiple",                                    
  "prop_asian",                                     
  "prop_black_afr_car",                                       
  "prop_other",                                      
  "IMD_Average_score",                                        
  "prop_o65",
  "prop_u25",
  "Median_annual_income",
  "Median_age",
  "unringfenced",                                             
  "ASC_infection_control_fund",
  "contain_outbreak_management",
  "Income_score",                                             
  "Employment_score",                                         
  "Education_Skills_and_Training_score",                      
  "Health_Deprivation_and_Disability_score",                  
  "Crime_score",                                              
  "Barriers_to_Housing_and_Services_score",                   
  "Living_Environment_score",
  "cumVaccPercentage_FirstDose",
  "cumVaccPercentage_SecondDose",
  "cumVaccPercentage_ThirdDose" 
)


sum_stats <- england_clean_data %>% select(all_of(vars))
sum_stats$Median_annual_income <- sum_stats$Median_annual_income / 1000
sum_stats_long <- pivot_longer(sum_stats, cols = everything())

sum_stats_long <- sum_stats_long %>% 
  mutate(group = case_when(
    name == "Pop_per_km2" ~ "Population demographics",
    name == "prop_all_other_white" ~ "Population demographics",
    name == "prop_mixed_multiple" ~ "Population demographics",
    name == "prop_asian" ~ "Population demographics",
    name == "prop_black_afr_car" ~ "Population demographics",
    name == "prop_other" ~ "Population demographics",
    name == "IMD_Average_score" ~ "Index of Multiple Deprivation",
    name == "prop_o65" ~ "Population demographics",
    name == "prop_u25" ~ "Population demographics",
    name == "Median_annual_income" ~ "Population demographics",
    name == "Median_age" ~ "Population demographics",
    name == "unringfenced" ~ "Government funding",
    name == "ASC_infection_control_fund" ~ "Government funding",
    name == "contain_outbreak_management" ~ "Government funding",
    name == "Income_score" ~ "Index of Multiple Deprivation",
    name == "Employment_score" ~ "Index of Multiple Deprivation",
    name == "Education_Skills_and_Training_score" ~ "Index of Multiple Deprivation",
    name == "Health_Deprivation_and_Disability_score" ~ "Index of Multiple Deprivation",
    name == "Crime_score" ~ "Index of Multiple Deprivation",
    name == "Barriers_to_Housing_and_Services_score" ~ "Index of Multiple Deprivation",
    name == "Living_Environment_score" ~ "Index of Multiple Deprivation",
    name == "cumVaccPercentage_FirstDose" ~ "Cumulative vaccine uptake",
    name == "cumVaccPercentage_SecondDose" ~ "Cumulative vaccine uptake",
    name == "cumVaccPercentage_ThirdDose" ~ "Cumulative vaccine uptake",
    TRUE ~ name
  ))




sum_stats_long <- sum_stats_long %>%
  mutate(
    clean_name = case_when(
      name == "Pop_per_km2" ~ "Population per km2",
      name == "prop_all_other_white" ~ "Proportion all other white",
      name == "prop_mixed_multiple" ~ "Proportion mixed / multiple",
      name == "prop_asian" ~ "Proportion Asian",
      name == "prop_black_afr_car" ~ "Proportion Black / African / Caribbean",
      name == "prop_other" ~ "Proportion other ethnicity",
      name == "IMD_Average_score" ~ "IMD average score",
      name == "prop_o65" ~ "Proportion over 65",
      name == "prop_u25" ~ "Proportion under 25",
      name == "Median_annual_income" ~ "Median annual income (thousands)",
      name == "Median_age" ~ "Median age",
      name == "unringfenced" ~ "Unringfenced",
      name == "ASC_infection_control_fund" ~ "ASC infection control fund",
      name == "contain_outbreak_management" ~ "COMF",
      name == "Income_score" ~ "Income score",
      name == "Employment_score" ~ "Employment score",
      name == "Education_Skills_and_Training_score" ~ "Education skills & training score",
      name == "Health_Deprivation_and_Disability_score" ~ "Health deprivation score",
      name == "Crime_score" ~ "Crime score",
      name == "Barriers_to_Housing_and_Services_score" ~ "Barriers to housing & services",
      name == "Living_Environment_score" ~ "Living environment score",
      name == "cumVaccPercentage_FirstDose" ~ "First dose",
      name == "cumVaccPercentage_SecondDose" ~ "Second Dose",
      name == "cumVaccPercentage_ThirdDose" ~ "Third Dose",
      TRUE ~ name
    )
  )





summary_stats <- sum_stats_long %>%
  group_by(group,clean_name) %>%
  summarise(
    mean = mean(value, na.rm = TRUE),
    min = min(value, na.rm = TRUE),
    lower_quartile = quantile(value, 0.25, na.rm = TRUE),
    median = median(value, na.rm = TRUE),
    upper_quartile = quantile(value, 0.75, na.rm = TRUE),
    max = max(value, na.rm = TRUE)
  ) %>%
  ungroup()



summary_data <- summary_stats %>%
  mutate(panel = case_when(
    group == "Population demographics" ~ "Panel A: Population Demographics",
    group == "Index of Multiple Deprivation" ~ "Panel B: Index of Multiple Deprivation",
    group == "Government funding" ~ "Panel C: Government Funding",
    group == "Cumulative vaccine uptake" ~ "Panel D: Cumulative Vaccine Uptake",
    TRUE ~ "Other"
  ))

summary_data_clean <- summary_data %>%
  rename(
    Variable = clean_name,
    Mean = mean,
    Min = min,
    Median = median,
    Max = max,
    "Lower Quartile" = lower_quartile,
    "Upper Quartile" = upper_quartile
  )

summary_data_clean %>%
  select(panel, Variable, Min, `Lower Quartile`, Mean, Median, `Upper Quartile`, Max) %>%
  arrange(panel) %>%
  gt(groupname_col = "panel") %>%
  tab_header(
    title = "Summary Statistics by Group",
    subtitle = "Grouped by Panel"
  ) %>%
  fmt_number(
    columns = c(Min, `Lower Quartile`,Mean, Median, `Upper Quartile`, Max),
    decimals = 2
  )

write.csv(summary_data_clean, "../results_tables/sum_stats.csv")



