
# --------------------------- API INTEGRATION ---------------------------

# Load necessary libraries
library(httr)
library(jsonlite)
library(dplyr)
library(readr)


# Define API URL
url <- "https://apis.vinmonopolet.no/stores/v0/details"

# Define your subscription key (replace with your actual key)
subscription_key <- "3b5b02c6793240fe9e6cb6d176e110e0"  

# Send GET request with subscription key in header
response <- GET(url, 
                add_headers(
                  Accept = "application/json",
                  `Ocp-Apim-Subscription-Key` = subscription_key  # API authentication
                ))


# Check response status
if (status_code(response) == 200) {
  # Convert API response to JSON and store it
  data <- content(response, as = "text", encoding = "UTF-8")
  store_data <- fromJSON(data)
  
  # View first few rows
  print(head(store_data))
} else {
  print(paste("Error:", status_code(response)))
}
# Transforming to normal characters
store_data$storeName <- iconv(store_data$storeName, from = "UTF-8", to = "ASCII//TRANSLIT")
store_data$storeName <- trimws(store_data$storeName)

# --------------------------- Combine API with Vinmonopol Data ---------------------------

# Load necessary libraries
library(dplyr)
library(stringr)
library(tidyr) # Load tidyr for unnesting
library(tidyverse)

# Ensure store_data_clean is correctly formatted
store_data_clean <- store_data %>%
  unnest_wider(address) %>%  # Expands nested address fields
  select(
    storeId,
    storeName,
    status,
    postalCode,
    city,
    gpsCoord
  ) %>%
  rename(
    Store_ID = storeId,
    Store_Name = storeName,
    Store_Status = status,
    Postal_Code = postalCode,
    City = city,
    GPS_Coordinates = gpsCoord
  )



# Transforming to normal characters
store_data_clean$Store_Name <- iconv(store_data_clean$Store_Name, from = "UTF-8", to = "ASCII//TRANSLIT")
store_data_clean$Store_Name <- trimws(store_data_clean$Store_Name)


# Standardize store names to improve matching
filtered_data <- filtered_data %>%
  mutate(Store = str_trim(str_to_lower(Store)))  # Trim spaces and convert to lowercase

store_data_clean <- store_data_clean %>%
  mutate(Store_Name = str_trim(str_to_lower(Store_Name)))  # Trim spaces and convert to lowercase

# Merge filtered_data (sales) with store_data_clean (store details)
final_data <- filtered_data %>%
  left_join(store_data_clean, by = c("Store" = "Store_Name"))  # Match by store name

# Check merged data
head(final_data)



