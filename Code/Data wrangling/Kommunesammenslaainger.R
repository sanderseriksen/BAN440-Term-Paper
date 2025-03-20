library(readxl)
library(dplyr)
library(tidyr)
library(openxlsx)
library(stringr)

# Filstier
final_data_path <- "final_data_18.xlsx"
kommunesammenslaainger_path <- "Kommunesammenslaainger.xlsx"


# Lese inn data
final_data <- read_excel(final_data_path)
kommunesammenslaainger <- read_excel(kommunesammenslaainger_path)

# Skriv ut kolonnenavn for ?? kontrollere strukturen
cat("Kolonnenavn i kommunesammensl??inger:\n")
print(colnames(kommunesammenslaainger))

# Rengj??ring av sammensl??ingsdata
kommunesammenslaainger <- kommunesammenslaainger %>%
  mutate(ny_kode = sub(" - .*$", "", as.character(`43831`)),
         ny_navn = sub("^[0-9]+ - ", "", as.character(`43831`)))

# H??ndtering av gamle kommunekoder
kommunesammenslaainger <- kommunesammenslaainger %>%
  rowwise() %>%
  mutate(gamle_koder = list(str_extract_all(as.character(`43466`), "[0-9]+ - [^0-9]+")[[1]])) %>%
  unnest(gamle_koder) %>%
  mutate(gammel_kode = sub(" - .*$", "", gamle_koder))

# Lage en mapping mellom gamle og nye kommuner
mapping <- kommunesammenslaainger %>%
  select(gammel_kode, ny_kode, ny_navn) %>%
  distinct()

# Oppdatere kommunekoder og -navn i final_data
final_data <- final_data %>%
  left_join(mapping, by = c("Municipality_Code" = "gammel_kode")) %>%
  mutate(Municipality_Code = ifelse(is.na(ny_kode), Municipality_Code, ny_kode),
         Municipality_Name = ifelse(is.na(ny_navn), Municipality_Name, ny_navn)) %>%
  select(-ny_kode, -ny_navn)

# Lagre oppdatert data
write.xlsx(final_data, "final_data_20.xlsx.")



