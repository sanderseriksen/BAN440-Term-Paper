# Load required libraries
library(ggplot2)
library(dplyr)
library(stringr)
library(sf)
library(csmaps)

# Step 1: Use the existing municipality map data
municipalities <- nor_municip_map_b2024_default_sf 

# Step 2: Read your store dataset
store_data_path <- "final_data_24.xlsx"
store_data <- readxl::read_excel(store_data_path)

# Step 3: Extract the last 4 digits from "location_code" to get the municipality code
municipalities <- municipalities %>%
  mutate(municip_code = as.integer(str_sub(location_code, -4)))  # Extract last 4 digits

# Step 4: Count the number of stores per municipality
store_counts <- store_data %>%
  group_by(Municipality_Code) %>%  
  summarise(num_stores = n())

# Changing to matching format
store_counts$Municipality_Code <- as.numeric(store_counts$Municipality_Code)

# Step 5: Create bins for store categories
store_counts <- store_counts %>%
  mutate(store_category = case_when(
    num_stores == 1 ~ "1",
    num_stores %in% 2:3 ~ "2-3",
    num_stores %in% 4:5 ~ "4-5",
    num_stores %in% 6:9 ~ "6-9",
    num_stores >= 10 ~ "10+",
    TRUE ~ NA_character_
  ))

# Step 6: Merge store counts with the existing geographical dataset
merged_data <- municipalities %>%
  left_join(store_counts, by = c("municip_code" = "Municipality_Code"))

# Step 7: Plot the heatmap with discrete color bins
ggplot(merged_data) +
  geom_sf(aes(fill = store_category), color = "white", size = 0.1) +
  scale_fill_manual(
    values = c("1" = "yellow", "2-3" = "orange", "4-5" = "red", "6-9" = "purple", "10+" = "darkred"),
    na.value = "grey",
    name = "Number of Stores"
  ) +
  theme_minimal() +
  labs(title = "Store Distribution by Municipality in Norway")




