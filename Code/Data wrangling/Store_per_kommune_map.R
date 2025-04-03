# Load required libraries
library(ggplot2)
library(dplyr)
library(stringr)
library(sf)
library(csmaps)


# Use the existing municipality map data
municipalities <- nor_municip_map_b2024_default_sf 

# Read your store dataset
store_data_path <- "final_data_24.xlsx"
store_data <- readxl::read_excel(store_data_path)

# Extract the last 4 digits from "location_code" to get the municipality code
municipalities <- municipalities %>%
  mutate(municip_code = as.integer(str_sub(location_code, -4)))  # Extract last 4 digits

# Count the number of stores per municipality
store_counts <- store_data %>%
  group_by(Municipality_Code) %>%  
  summarise(num_stores = n())

# Changing to matching format
store_counts$Municipality_Code <- as.numeric(store_counts$Municipality_Code)

# Merge store counts with the geographical dataset
merged_data <- municipalities %>%
  left_join(store_counts, by = c("municip_code" = "Municipality_Code"))

# Assign store categories, treating NA as 0
merged_data <- merged_data %>%
  mutate(store_category = case_when(
    is.na(num_stores) ~ "0",
    num_stores == 1 ~ "1",
    num_stores %in% 2:3 ~ "2-3",
    num_stores %in% 4:5 ~ "4-5",
    num_stores %in% 6:9 ~ "6-9",
    num_stores >= 10 ~ "10+",
    TRUE ~ NA_character_
  ))

# Make sure factor levels are ordered correctly
merged_data$store_category <- factor(
  merged_data$store_category,
  levels = c("0", "1", "2-3", "4-5", "6-9", "10+")
)

# Plot the heatmap with discrete color bins
ggplot(merged_data) +
  geom_sf(aes(fill = store_category), color = "white", size = 0.1) +
  scale_fill_manual(
    values = c("0" = "grey", "1" = "yellow", "2-3" = "orange", "4-5" = "red", "6-9" = "purple", "10+" = "darkred"),
    name = "Number of Stores"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank()) + 
  labs(title = "Store Distribution by Municipality",
       x = NULL, y = NULL) 

    



