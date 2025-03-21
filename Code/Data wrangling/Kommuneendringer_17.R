### Kommuneendringer 2017 ###

# Laste inn nødvendige biblioteker
library(readxl)
library(dplyr)
library(stringr)
library(writexl)

# Lese inn data fra Excel-filer
data_df <- read_excel("final_data.xlsx")
kommuneendringer_df <- read_excel("Kommuneendringer_17.xlsx")

# Rydde opp i kolonnenavnene ved å bruke de riktige kolonnenavnene
colnames(kommuneendringer_df) <- c("New_Codes", "Old_Codes")

# Splitte gamle kommunenummer (dersom flere gamle kommuner er adskilt med mellomrom)
kommuneendringer_df$Old_Codes <- str_split(kommuneendringer_df$Old_Codes, " ")

# Trekke ut de fire første sifrene fra hvert element i Old_Codes
old_codes_numeric <- lapply(kommuneendringer_df$Old_Codes, function(x) substr(x, 1, 4))

# Lage en oppslagsliste for gamle koder til nye koder (ensidig mapping)
# Bruker de rene numeriske kodene (f.eks. "0706") som nøkler
kommune_mapping <- setNames(rep(kommuneendringer_df$New_Codes, times = sapply(old_codes_numeric, length)),
                            unlist(old_codes_numeric))

# Oppdatere både Municipality_Code og Municipality_Name i data_df.
# Her fjerner vi kommunenummeret fra new_val for å beholde kun bokstavene (navnet).
data_df <- data_df %>%
  rowwise() %>%
  mutate(
    new_val = if (Municipality_Code %in% names(kommune_mapping)) kommune_mapping[[Municipality_Code]] else NA_character_,
    Municipality_Code = if (!is.na(new_val)) substr(new_val, 1, 4) else Municipality_Code,
    Municipality_Name = if (!is.na(new_val)) {
      # Fjerner de fire første sifrene og bindestreken, slik at kun navnet (bokstavene) beholdes
      str_trim(str_remove(new_val, "^[0-9]{4}\\s*-\\s*"))
    } else {
      Municipality_Name
    }
  ) %>%
  ungroup() %>%
  select(-new_val)

# Lagre den oppdaterte filen
write_xlsx(data_df, "final_data_17.xlsx")
