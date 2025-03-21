# Laste inn nødvendige biblioteker
library(readxl)
library(dplyr)
library(stringr)
library(writexl)

# Lese inn data fra Excel-filer
data_df <- read_excel("final_data_20.xlsx")
kommuneendringer_df <- read_excel("Kommuneendringer_24.xlsx")

# Rydde opp i kolonnenavnene ved å bruke de riktige kolonnenavnene
colnames(kommuneendringer_df) <- c("New_Code", "Old_Codes")

# Splitte gamle kommunenummer (dersom flere gamle kommuner er adskilt med mellomrom)
kommuneendringer_df$Old_Codes <- str_split(kommuneendringer_df$Old_Codes, " ")

# Lage en oppslagsliste for gamle koder til nye koder (ensidig mapping)
kommune_mapping <- setNames(rep(kommuneendringer_df$New_Code, 
                                times = sapply(kommuneendringer_df$Old_Codes, length)),
                            unlist(kommuneendringer_df$Old_Codes))

# Oppdatere både Municipality_Code og Municipality_Name i data_df
data_df <- data_df %>%
  rowwise() %>%
  mutate(
    new_val = if (Municipality_Code %in% names(kommune_mapping)) kommune_mapping[[Municipality_Code]] else NA_character_,
    Municipality_Code = if (!is.na(new_val)) substr(new_val, 1, 4) else Municipality_Code,
    Municipality_Name = if (!is.na(new_val)) str_trim(str_remove(new_val, "^[0-9]{4}\\s*-\\s*")) else Municipality_Name
  ) %>%
  ungroup() %>%
  select(-new_val)

# Hardkode rad 121 til at "Municipality_Code" blir 1580 og "Municipality_Name" blir Haram
data_df[121, "Municipality_Code"] <- "1580"
data_df[121, "Municipality_Name"] <- "Haram"

# Lagre den oppdaterte filen
write_xlsx(data_df, "final_data_24.xlsx")