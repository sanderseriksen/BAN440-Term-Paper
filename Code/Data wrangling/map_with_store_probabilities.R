# Load required libraries
library(ggplot2)
library(dplyr)
library(stringr)
library(sf)
library(csmaps)

# Use the existing municipality map data
municipalities <- nor_municip_map_b2024_default_sf 

# Extract the last 4 digits from "location_code" to get the municipality code
municipalities <- municipalities %>%
  mutate(municip_code = as.integer(str_sub(location_code, -4)))  # Extract last 4 digits

# Merge population data with the existing geographical dataset
predicted_data$Municipality_Code <- as.numeric(predicted_data$Municipality_Code)

# Add a "prob_category" for municipalities that have at least one store
predicted_data <- predicted_data %>%
  mutate(prob_category = case_when(
    Number_of_stores > 0 ~ "Has a store",
    prob_cv >= 0 & prob_cv < 0.25 ~ "Low",
    prob_cv >= 0.25 & prob_cv < 0.5 ~ "Medium Low",
    prob_cv >= 0.5 & prob_cv < 0.75 ~ "Medium High",
    prob_cv >= 0.75 & prob_cv <= 1 ~ "High",
    TRUE ~ NA_character_
  ))

# Set factor levels to control legend order
predicted_data$prob_category <- factor(predicted_data$prob_category,
                                       levels = c("High", "Medium High", "Medium Low", "Low", "Has a store"))

merged_prob_data <- municipalities %>%
  left_join(predicted_data, by = c("municip_code" = "Municipality_Code")) %>%
  mutate(prob_category = replace_na(prob_category, "Has a store"))  # Fill NAs with "Has a store"

# Plot
ggplot(merged_prob_data) +
  geom_sf(aes(fill = prob_category), color = "white", size = 0.1) +
  scale_fill_manual(
    values = c(
      "High" = "darkgreen",
      "Medium High" = "#90EE90",
      "Medium Low" = "#FF6666",
      "Low" = "darkred",
      "Has a store" = "grey"
    ),
    name = "Probability"
  ) +
  coord_sf() +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank()
  ) +
  labs(title = "Predicted Probability by Municipality",
       x = NULL, y = NULL)
