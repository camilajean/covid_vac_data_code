# **Visualising the results across all variable selection models**
# Focus on common significant variables 


# General Libraries -------------------------------------------------------

library(ggplot2)
library(readxl)
library(readr)
library(tidyr)
library(cowplot)
library(grDevices)
library(EpiForsk)
library(stringr)
library(dplyr)
library(ggpubr)
library(readr)
library(grid)




# Clear environment and set working directory -----------------------------

rm(list = ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))



# Load in Data ------------------------------------------------------------

simple_doses <- read_csv("../results_tables/simple_model_results_final.csv")
stab_doses <- read_csv("../results_tables/stability_model_results_final.csv")
step_doses <- read_csv("../results_tables/stepwise_model_results_final.csv")

simple_dropout <- read_csv("../results_tables/simple_between_doses_final.csv")
stab_dropout <- read_csv("../results_tables/stability_between_doses_final.csv")
step_dropout <- read_csv("../results_tables/stepwise_between_doses_final.csv")



# Merge and Save  ---------------------------------------------------------

doses <- rbind(simple_doses, stab_doses, step_doses)
write.csv(doses, "../results_tables/model_results_doses.csv")


dropout <- rbind(simple_dropout, stab_dropout, step_dropout)
write.csv(dropout, "../results_tables/model_results_dropout.csv")



# Forest Plot Doses -------------------------------------------------------

# Results plots

doses <- doses %>%
  mutate(
    clean_var = case_when (
      Variable == "Pop_per_km2" ~ "Population per km2",
      Variable == "prop_travelling_to_work" ~ "Proportion Travelling To Work",
      Variable == "prop_not_in_work" ~ "Proportion Not In Work",
      Variable == "prop_all_other_white" ~ "Proportion All Other White",
      Variable ==  "prop_mixed_multiple" ~ "Proportion Mixed",
      Variable == "prop_asian" ~ "Proportion Asian",
      Variable == "prop_black_afr_car" ~ "Proportion Black / African / Caribbean", 
      Variable == "prop_other" ~ "Proportion Other Ethnicity",
      Variable == "IMD_Average_score" ~ "IMD Average Score",
      Variable == "prop_o65" ~ "Proportion Over 65",
      Variable == "prop_u25" ~ "Proportion Under 25",
      Variable == "Median_annual_income" ~ "Median Annual Income",
      Variable == "unringfenced_percapita" ~ "Unringfenced Fund Per Capita",
      Variable == "ASC_infection_control_fund_percapita" ~ "ASC Infection Control Fund Per Capita",
      Variable == "contain_outbreak_management_percapita" ~ "COMF Per Capita",
      Variable == "Income_score" ~ "Income Score (IMD Domain)",
      Variable == "Employment_score" ~ "Employment Score (IMD Domain)",
      Variable == "Education_Skills_and_Training_score" ~ "Education Skills & Training Score (IMD Domain)",
      Variable == "Health_Deprivation_and_Disability_score" ~ "Health Deprivation & Disability Score (IMD Domain)",
      Variable == "Crime_score" ~ "Crime Score (IMD Domain)",
      Variable == "Barriers_to_Housing_and_Services_score" ~ "Barriers to Housing & Services Score (IMD Domain)",
      Variable == "Living_Environment_score" ~ "Living Environment Score (IMD Domain)",
      Variable == "Median_age" ~"Median Age",
      Variable == "mean_popden" ~ "Mean Population Density",
      Variable == "Population" ~ "Population",
      Variable == "no_jobs" ~ "Estimated Job Counts",
      Variable == "ASC_workforce_capacity" ~ "ASC Workforce Capacity",
      Variable == "ASC_rapid_testing" ~ "ASC Rapid Testing",
      Variable == "CEV_fund_percapita" ~ "CEV Fund Per Capita",
      Variable == "compliance_and_enforcement" ~ "Compliance & Enforcement",
      Variable == "rough_sleeping" ~ "Rough Sleeping",
      Variable == "next_steps_accommodation" ~"Next Steps Accomodation",
      Variable == "food_and_essential" ~ "Food & Essential",
      Variable == "DWP_winter_grant_percapita" ~ "DWP Winter Grant Per Capita",
      Variable == "leisure_recovery" ~ "Leisure Recovery",
      Variable == "Core_services_funding_percapita" ~ "Core Services Funding Per Capita",
      Variable == "Primary_care_funding_percapita" ~ "Primary Care Funding Per Capita",
      Variable == "Specialised_services" ~ "Specialised Services",
      TRUE ~ Variable
    ), 
    clean_mod = case_when(
      model == "Simple" ~ "Selected",
      model == "Lasso" ~ "Stability Lasso",
      TRUE ~ model
    ),
    clean_eff = case_when(
      effects == "Fixed" ~ "Fixed Effects Only",
      effects == "Random" ~ "With Random Effects",
      TRUE ~ effects
    ))


fd <- subset(doses, doses$dose == "First")
sd <- subset(doses, doses$dose == "Second")
td <- subset(doses, doses$dose == "Third")

shared_theme <- theme(
  legend.position = "none",
  plot.title = element_text(
    face = "bold", 
    family = "Arial",
    size = 15, 
    hjust = 0.5, 
    vjust = 1
  ),
  panel.border = element_blank(),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  axis.ticks.length = unit(0.20,"cm"),
  axis.title.x = element_text(
    margin = margin(15, 0, 0, 0), 
    size = 12, 
    face = "bold", 
    family = "Arial"
  ),
  axis.text.x = element_text(
    size = 12, 
    face = "plain", 
    family = "Arial", 
    margin = margin(5, 0, 0, 0)
  ),
  axis.line.x = element_line(size = 0.5),
  axis.title.y = element_blank(),
  axis.line.y = element_line(color = "white", size = 0.5),
  axis.ticks.y = element_blank()
)

#First Dose
# Prop all other white, Prop Asian, Prop B/A/C, Living Environment, Median age



filtered_fd <- fd %>% filter(Variable %in% c("Median_age", "prop_all_other_white", "prop_asian", "prop_black_afr_car","Living_Environment_score")) 

dodge_width <- 0.3

plot1 <- ggplot(filtered_fd, aes(y = reorder(clean_var, Beta), x = Beta, color = clean_eff)) +
  geom_point(aes(color = clean_eff), shape = 15, size = 3, position = position_dodge(width = dodge_width), show.legend = TRUE) +  
  geom_errorbarh(aes(xmin = CI_Lower, xmax = CI_Upper, color = clean_eff), height = 0, position = position_dodge(width = dodge_width), size = 2.5) +
  xlab("Coefficient (95% CI)") + 
  ylab(" ") + 
  scale_color_manual(values = c("Fixed Effects Only" = "dodgerblue2", "With Random Effects" = "darkorange2")) + 
  scale_x_continuous(limits = c(-7, 7), breaks = seq(-7, 7, 1)) +
  theme_bw() +
  shared_theme + 
  theme(
    panel.border = element_blank(),
    panel.background = element_blank(),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(), 
    axis.line = element_line(colour = "black"),
    axis.text.y = element_text(size = 15, colour = "black"),
    axis.text.x.bottom = element_blank(),
    axis.title.x = element_blank(),
    #axis.title.x = element_text(size = 12, colour = "black"),
    strip.background = element_blank(),
    strip.text = element_text(size = 15, face = "bold"),
    legend.position = "bottom",
    legend.text = element_text(size = 15),
    legend.title = element_blank()
  ) +
  facet_wrap(~clean_mod, scales = "free_x")+
  geom_vline(
    xintercept = 0,
    linetype = 2,
    color = "red"
  ) 

#ggsave("../figures/png/First_dose_Forest.png", plot = plot1,width = 12, height = 7)


#Second Dose
# Prop all other white, Prop Asian, Prop B/A/C, unringfenced, Living environment, Median age



filtered_sd <- sd %>% filter(Variable %in% c("Living_Environment_score", "Median_age", "prop_all_other_white", "prop_asian", "prop_black_afr_car","unringfenced_percapita")) 

dodge_width <- 0.3

plot2 <- ggplot(filtered_sd, aes(y = reorder(clean_var, Beta), x = Beta, color = clean_eff)) +
  geom_point(aes(color = clean_eff), shape = 15, size = 3, position = position_dodge(width = dodge_width), show.legend = TRUE) +  
  geom_errorbarh(aes(xmin = CI_Lower, xmax = CI_Upper, color = clean_eff), height = 0, position = position_dodge(width = dodge_width), size = 2.5) +
  xlab("Coefficient (95% CI)") + 
  ylab(" ") + 
  scale_color_manual(values = c("Fixed Effects Only" = "dodgerblue2", "With Random Effects" = "darkorange2")) + 
  scale_x_continuous(limits = c(-7, 7), breaks = seq(-7, 7, 1)) +
  theme_bw() +
  shared_theme + 
  theme(
    panel.border = element_blank(),
    panel.background = element_blank(),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(), 
    axis.line = element_line(colour = "black"),
    axis.text.y = element_text(size = 15, colour = "black"),
    axis.text.x.bottom = element_blank(),
    axis.title.x = element_blank(),
    #axis.title.x = element_text(size = 12, colour = "black"),
    strip.background = element_blank(),
    strip.text = element_blank(),
    #strip.text = element_text(size = 12, face = "bold"),
    legend.position = "bottom",
    legend.text = element_text(size = 15),
    legend.title = element_blank()
  ) +
  facet_wrap(~clean_mod, scales = "free_x")+
  geom_vline(
    xintercept = 0,
    linetype = 2,
    color = "red"
  ) 

#ggsave("../figures/png/Second_dose_Forest.png", plot = plot2,width = 12, height = 7)



#Third Dose
# Prop all other white, Prop Asian, Prop B/A/C, Crime score, Living environment, Median age



filtered_td <- td %>% filter(Variable %in% c("Crime_score","Living_Environment_score","Median_age", "prop_all_other_white", "prop_asian", "prop_black_afr_car")) 

dodge_width <- 0.3

plot3 <- ggplot(filtered_td, aes(y = reorder(clean_var, Beta), x = Beta, color = clean_eff)) +
  geom_point(aes(color = clean_eff), shape = 15, size = 3, position = position_dodge(width = dodge_width), show.legend = TRUE) +  
  geom_errorbarh(aes(xmin = CI_Lower, xmax = CI_Upper, color = clean_eff), height = 0, position = position_dodge(width = dodge_width), size = 2.5) +
  xlab("Coefficient (95% CI)") + 
  ylab(" ") + 
  scale_color_manual(values = c("Fixed Effects Only" = "dodgerblue2", "With Random Effects" = "darkorange2")) + 
  scale_x_continuous(limits = c(-7, 7), breaks = seq(-7, 7, 1)) +
  theme_bw() +
  shared_theme + 
  theme(
    panel.border = element_blank(),
    panel.background = element_blank(),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(), 
    axis.line = element_line(colour = "black"),
    axis.text.y = element_text(size = 15, colour = "black"),
    axis.text.x.bottom = element_text(size = 15, colour = "black"),
    axis.title.x = element_text(size = 15, colour = "black"),
    strip.background = element_blank(),
    strip.text = element_blank(),
    #strip.text = element_text(size = 12, face = "bold"),
    legend.position = "bottom",
    legend.text = element_text(size = 15),
    legend.title = element_blank()
  ) +
  facet_wrap(~clean_mod, scales = "free_x", ncol = 3) +
  coord_cartesian(xlim = c(-7, 7)) +
  geom_vline(
    xintercept = 0,
    linetype = 2,
    color = "red"
  ) 

#ggsave("../figures/png/Third_dose_Forest.png", plot = plot3,width = 12, height = 7)


plot1n2n3 <- ggarrange(
  plot1, plot2,plot3,
  labels = c("A", "B","C"),
  common.legend = TRUE,
  legend = "bottom",
  nrow = 3,
  ncol = 1,
  align = "v"
)



ggsave("../figures/png/doses_forest.png", plot = plot1n2n3, width = 12, height = 15)
ggsave("../figures/tiff/doses_forest.tiff", plot = plot1n2n3, width = 12, height = 15)



# Load the PNG file
img <- png::readPNG("../figures/png/doses_forest.png")
pdf("../figures/pdf/doses_forest.pdf", width = 8, height = 6)  
grid.raster(img)
dev.off()




# Forest Plot Dropout -----------------------------------------------------

dropout <- dropout %>%
  mutate(
    clean_var = case_when (
      Variable == "Pop_per_km2" ~ "Population per km2",
      Variable == "prop_travelling_to_work" ~ "Proportion Travelling To Work",
      Variable == "prop_not_in_work" ~ "Proportion Not In Work",
      Variable == "prop_all_other_white" ~ "Proportion All Other White",
      Variable ==  "prop_mixed_multiple" ~ "Proportion Mixed",
      Variable == "prop_asian" ~ "Proportion Asian",
      Variable == "prop_black_afr_car" ~ "Proportion Black / African / Caribbean", 
      Variable == "prop_other" ~ "Proportion Other Ethnicity",
      Variable == "IMD_Average_score" ~ "IMD Average Score",
      Variable == "prop_o65" ~ "Proportion Over 65",
      Variable == "prop_u25" ~ "Proportion Under 25",
      Variable == "Median_annual_income" ~ "Median Annual Income",
      Variable == "unringfenced_percapita" ~ "Unringfenced Fund Per Capita",
      Variable == "ASC_infection_control_fund_percapita" ~ "ASC Infection Control Fund Per Capita",
      Variable == "contain_outbreak_management_percapita" ~ "COMF Per Capita",
      Variable == "Income_score" ~ "Income Score (IMD Domain)",
      Variable == "Employment_score" ~ "Employment Score (IMD Domain)",
      Variable == "Education_Skills_and_Training_score" ~ "Education Skills & Training Score (IMD Domain)",
      Variable == "Health_Deprivation_and_Disability_score" ~ "Health Deprivation & Disability Score (IMD Domain)",
      Variable == "Crime_score" ~ "Crime Score (IMD Domain)",
      Variable == "Barriers_to_Housing_and_Services_score" ~ "Barriers to Housing & Services Score (IMD Domain)",
      Variable == "Living_Environment_score" ~ "Living Environment Score (IMD Domain)",
      Variable == "Median_age" ~"Median Age",
      Variable == "mean_popden" ~ "Mean Population Density",
      Variable == "Population" ~ "Population",
      Variable == "no_jobs" ~ "Estimated Job Counts",
      Variable == "ASC_workforce_capacity" ~ "ASC Workforce Capacity",
      Variable == "ASC_rapid_testing" ~ "ASC Rapid Testing",
      Variable == "CEV_fund_percapita" ~ "CEV Fund Per Capita",
      Variable == "compliance_and_enforcement" ~ "Compliance & Enforcement",
      Variable == "rough_sleeping" ~ "Rough Sleeping",
      Variable == "next_steps_accommodation" ~"Next Steps Accomodation",
      Variable == "food_and_essential" ~ "Food & Essential",
      Variable == "DWP_winter_grant_percapita" ~ "DWP Winter Grant Per Capita",
      Variable == "leisure_recovery" ~ "Leisure Recovery",
      Variable == "Core_services_funding_percapita" ~ "Core Services Funding Per Capita",
      Variable == "Primary_care_funding_percapita" ~ "Primary Care Funding Per Capita",
      Variable == "Specialised_services" ~ "Specialised Services",
      TRUE ~ Variable
    ), 
    clean_mod = case_when(
      model == "Simple" ~ "Selected",
      TRUE ~ model
    ),
    clean_eff = case_when(
      effects == "Fixed" ~ "Fixed Effects Only",
      effects == "Random" ~ "With Random Effects",
      TRUE ~ effects
    ))



fdvsd <- subset(dropout, dropout$dose == "First vs Second")
sdvtd <- subset(dropout, dropout$dose == "Second vs Third")




# Second Dose Droupout
# Pop per km2, Prop all other white, Prop Asian, Prop u25, Unringfenced, Living environment


filtered_sd_dropout <- fdvsd %>% filter(Variable %in% c("Pop_per_km2","prop_all_other_white", "prop_asian","prop_u25","Living_Environment_score","unringfenced_percapita" )) 

dodge_width <- 0.3

plot4 <- ggplot(filtered_sd_dropout, aes(y = reorder(clean_var, Beta), x = Beta, color = clean_eff)) +
  geom_point(aes(color = clean_eff), shape = 15, size = 3, position = position_dodge(width = dodge_width), show.legend = TRUE) +  
  geom_errorbarh(aes(xmin = CI_Lower, xmax = CI_Upper, color = clean_eff), height = 0, position = position_dodge(width = dodge_width), size = 2.5) +
  xlab("Coefficient (95% CI)") + 
  ylab(" ") + 
  scale_color_manual(values = c("Fixed Effects Only" = "dodgerblue2", "With Random Effects" = "darkorange2")) + 
  scale_x_continuous(limits = c(-5, 10), breaks = seq(-5, 10, 1)) +
  theme_bw() +
  shared_theme + 
  theme(
    panel.border = element_blank(),
    panel.background = element_blank(),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(), 
    axis.line = element_line(colour = "black"),
    axis.text.y = element_text(size = 15, colour = "black"),
    axis.text.x.bottom = element_blank(),
    #axis.title.x = element_text(size = 12, colour = "black"),
    axis.title.x = element_blank(),
    strip.background = element_blank(),
    strip.text = element_text(size = 15, face = "bold"),
    legend.position = "bottom",
    legend.text = element_text(size = 15),
    legend.title = element_blank()
  ) +
  facet_wrap(~clean_mod, scales = "free_x")+
  coord_cartesian(xlim = c()) +
  geom_vline(
    xintercept = 0,
    linetype = 2,
    color = "red"
  ) 

#ggsave("../figures/png/Second_dropout_Forest.png", plot = plot4,width = 12, height = 7)



# Third Dose Droupout
# Prop all other white, Prop Asian, Prop B/A/C, Prop o65


filtered_td_dropout <- sdvtd %>% filter(Variable %in% c("prop_all_other_white","prop_asian","prop_black_afr_car","prop_o65" )) 

dodge_width <- 0.3

plot5 <- ggplot(filtered_td_dropout, aes(y = reorder(clean_var, Beta), x = Beta, color = clean_eff)) +
  geom_point(aes(color = clean_eff), shape = 15, size = 3, position = position_dodge(width = dodge_width), show.legend = TRUE) +  
  geom_errorbarh(aes(xmin = CI_Lower, xmax = CI_Upper, color = clean_eff), height = 0, position = position_dodge(width = dodge_width), size = 2.5) +
  xlab("Coefficient (95% CI)") + 
  ylab(" ") + 
  scale_color_manual(values = c("Fixed Effects Only" = "dodgerblue2", "With Random Effects" = "darkorange2")) + 
  scale_x_continuous(limits = c(-5, 10), breaks = seq(-5, 10, 1)) +
  theme_bw() +
  shared_theme + 
  theme(
    panel.border = element_blank(),
    panel.background = element_blank(),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(), 
    axis.line = element_line(colour = "black"),
    axis.text.y = element_text(size = 15, colour = "black"),
    axis.text.x.bottom = element_text(size = 15, colour = "black"),
    axis.title.x = element_text(size = 15, colour = "black"),
    strip.background = element_blank(),
    strip.text = element_blank(),
    #strip.text = element_text(size = 12, face = "bold"),
    legend.position = "bottom",
    legend.text = element_text(size = 15),
    legend.title = element_blank()
  ) +
  facet_wrap(~clean_mod, scales = "free_x")+
  coord_cartesian(xlim = c()) +
  geom_vline(
    xintercept = 0,
    linetype = 2,
    color = "red"
  ) 

plot4n5 <- ggarrange(
  plot4, plot5,
  labels = c("A", "B"),
  common.legend = TRUE,
  legend = "bottom",
  nrow = 2,
  ncol = 1,
  align = "v"
)

print(plot4n5)
#ggsave("../figures/png/Third_dropout_Forest.png", plot = plot5,width = 12, height = 7)

ggsave("../figures/png/dropout_forest.png", plot = plot4n5, width = 13, height = 10)
ggsave("../figures/tiff/dropout_forest.tiff", plot = plot4n5, width = 13, height = 10)


# Load the PNG file
img2 <- png::readPNG("../figures/png/dropout_forest.png")
pdf("../figures/pdf/dropout_forest.pdf", width = 8, height = 6)  
grid.raster(img2)
dev.off()

