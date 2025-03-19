### Vinmonopolet data wrangling ###

# Importing libraries
library(tidyverse)
library(readxl)
library(writexl)

# Set locale to UTF-8
Sys.setlocale("LC_ALL", "en_US.UTF-8")

# Define the path to your Excel file
file_path <- "Vinmonopolet_2024.xlsx"

# Get the names of all sheets in the Excel file
sheet_names <- excel_sheets(file_path)

# Read each sheet into a list of data frames, skipping the first row
list_of_dfs <- lapply(sheet_names, function(sheet) {
  read_excel(file_path, sheet = sheet, skip = 2)
})

# Combine all data frames into a single data frame
combined_data <- bind_rows(list_of_dfs)

# View the combined data frame
print(combined_data)

# Unique values in the first column
unique_values <- unique(combined_data$...1)

print(unique_values)

# Transforming to normal characters
combined_data$...1 <- iconv(combined_data$...1, from = "UTF-8", to = "ASCII//TRANSLIT")
combined_data$...1 <- trimws(combined_data$...1)

# Define the values to filter out
values_to_exclude <- c(
  "Svakvin", "Rodvin", "Hvitvin", "Musserende vin", "Rosevin", 
  "Perlende vin", "Aromatisert vin", "Sider", "Fruktvin", 
  "Brennevin", "Vodka", "Likor", "Whisky", "Akevitt", 
  "Brennevin, annet", "Gin", "Druebrennevin", 
  "Brennevin, noytralt < 37,5 %", "Rom", "Bitter", 
  "Fruktbrennevin", "Genever", "Ol", "Alkoholfritt", "Sterkvin", "Totalsum",
  "eLager"
)

# Column names of combined data
colnames(combined_data)

# Filter out the specified values from the first column
filtered_data <- combined_data %>%
  mutate("2024" = as.numeric(`2024`),
         "Store" = as.character(`...1`)) %>%
  filter(!.[[1]] %in% values_to_exclude) %>% 
  select("Store", "2024")

# View the filtered data
print(filtered_data)

# Export the filtered data to an Excel file
write_xlsx(filtered_data, "filtered_data.xlsx")

library(readxl)
library(dplyr)

# Load the postal code dataset
postal_data <- read_excel("dimpostnummer.xlsx")

# Rename columns to match their purpose in final_data
postal_data <- postal_data %>%
  rename(
    Postal_Code = `Postnummer`,           # Postal code
    Municipality_Code = `KommuneKode`,    # Municipality code
    Municipality_Name = `Kommune`,        # Municipality name
    Region_Code = `FylkeKode`,            # Region code
    Region_Name = `Fylke`,                # Region name
    Latitude = `Latitude`,                # Latitude
    Longitude = `Longitude`               # Longitude
  )

# Convert Postal_Code to numeric to ensure a proper join
postal_data <- postal_data %>%
  mutate(Postal_Code = as.numeric(Postal_Code))

final_data <- final_data %>%
  mutate(Postal_Code = as.numeric(Postal_Code))

# Merge final_data (stores & revenue) with postal_data (municipality info)
final_data <- final_data %>%
  left_join(postal_data, by = "Postal_Code")

# View merged dataset
head(final_data)

# Save final dataset
write_csv(final_data, "Vinmonopolet_Stores_Final_With_Municipality.csv")

# View final dataset
View(final_data)

# Remove rows where "2024" column is NA
final_data <- final_data %>%
  drop_na(`2024`)  # Drop rows where "2024" column is NA

# Count NA values in Store_ID column
na_count_store_id <- sum(is.na(final_data$Store_ID))

# Print the result
print(paste("Number of NA values in Store_ID column:", na_count_store_id))

# Identify row indices with NA in Store_ID
na_rows_indices <- which(is.na(final_data$Store_ID))

# Print row indices
print(na_rows_indices)
# Filter rows where Store_ID is NA
rows_with_na <- final_data %>% filter(is.na(Municipality_Code))

# View the rows with NA values in Store_ID
print(rows_with_na)

# Extract stores with NA Store_ID
na_store_names <- rows_with_na$Store
print(na_store_names)  # Check which stores are problematic
# Convert both datasets to lowercase and trim spaces for better matching
store_data_clean <- store_data_clean %>%
  mutate(Store_Name = str_trim(str_to_lower(Store_Name)))

# Find potential matches in store_data_clean
matches <- store_data_clean %>%
  filter(str_detect(Store_Name, paste(na_store_names, collapse = "|")))

# View matches
print(matches)


# Export the final data to an Excel file
write_xlsx(final_data, "final_data.xlsx")


