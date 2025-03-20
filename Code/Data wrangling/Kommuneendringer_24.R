# Laste inn n??dvendige pakker
library(readxl)
library(dplyr)
library(stringr)
library(openxlsx)

# Laste inn kommuneendringer og oppdatert final data fra Excel
kommuneendringer_df <- read_excel("Kommuneendringer_24.xlsx")
oppdatert_final_data_df <- read_excel("final_data_20.xlsx")

# Gi passende kolonnenavn til kommuneendringer_df
colnames(kommuneendringer_df) <- c("New_Municipality", "Old_Municipality")

# Funksjon for ?? fjerne spesialtegn og ekstra mellomrom fra kommunenavn
clean_name <- function(name) {
  name <- str_replace_all(name, "[^[:alnum:][:space:]]", "") # Fjern spesialtegn
  name <- str_trim(name) # Fjern ledende og etterf??lgende mellomrom
  return(name)
}

# Ekstraher kommunenummer og kommunenavn fra begge kolonner og rengj??r navnene
kommuneendringer_df <- kommuneendringer_df %>%
  mutate(
    New_Code = str_extract(New_Municipality, "\\d+"),
    New_Name = clean_name(str_extract(New_Municipality, "-\\s*(.*)")),
    Old_Code = str_extract(Old_Municipality, "\\d+"),
    Old_Name = clean_name(str_extract(Old_Municipality, "-\\s*(.*)"))
  )

# Fjerne mellomrom fra kommunenavn i oppdatert_final_data_df
oppdatert_final_data_df <- oppdatert_final_data_df %>%
  mutate(Municipality_Name = clean_name(Municipality_Name))

# Fjerne duplikater i kommuneendringer_df basert p?? Old_Name, beholde siste rad
kommuneendringer_df <- kommuneendringer_df %>%
  group_by(Old_Name) %>%
  slice_tail(n = 1) %>%
  ungroup()

# Sl?? sammen dataene basert p?? rensede kommunenavn
merged_df <- oppdatert_final_data_df %>%
  left_join(kommuneendringer_df %>% select(Old_Name, New_Code), 
            by = c("Municipality_Name" = "Old_Name")) %>%
  mutate(Municipality_Code = ifelse(!is.na(New_Code), New_Code, Municipality_Code)) %>%
  select(-New_Code)

# Lagre den oppdaterte dataen i en ny Excel-fil
write.xlsx(merged_df, "final_data_24.xlsx")

print("Oppdateringen er fullf??rt, og filen er lagret som 'final_data_24.xlsx'")
