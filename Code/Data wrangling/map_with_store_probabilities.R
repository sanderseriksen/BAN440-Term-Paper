# Load required libraries
library(ggplot2)
library(dplyr)
library(stringr)
library(sf)
library(csmaps)

# Step 1: Use the existing municipality map data
municipalities <- nor_municip_map_b2024_default_sf 

# Step 3: Extract the last 4 digits from "location_code" to get the municipality code
municipalities <- municipalities %>%
  mutate(municip_code = as.integer(str_sub(location_code, -4)))  # Extract last 4 digits

# Step 4: Merge population data with the existing geographical dataset
predicted_data$Municipality_Code <- as.numeric(predicted_data$Municipality_Code)

predicted_data <- predicted_data %>%
  filter(Number_of_stores == 0) %>%
  mutate(prob_category = case_when(
    prob_cv >= 0 & prob_cv < 0.25 ~ "Low",
    prob_cv >= 0.25 & prob_cv < 0.5 ~ "Medium Low",
    prob_cv >= 0.5 & prob_cv < 0.75 ~ "Medium High",
    prob_cv >= 0.75 & prob_cv <= 1 ~ "High",
    TRUE ~ NA_character_
  ))

# Step 6: Merge population data with the map data
merged_prob_data <- municipalities %>%
  left_join(predicted_data, by = c("municip_code" = "Municipality_Code"))

# Step 7: Plot the heatmap with discrete color bins for population
ggplot(merged_prob_data) +
  geom_sf(aes(fill = prob_category), color = "white", size = 0.1) +
  scale_fill_manual(
    values = c("Low" = "darkred", "Medium Low" = "#FF6666", "Medium High" = "#90EE90", "High" = "darkgreen"),
    na.value = "grey",
    name = "Probability"
  ) +
  coord_sf() +
  theme_minimal() +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank()
  ) +
  labs(title = "Predicted Probability by 0 store municipalities",
       x = NULL, y = NULL)