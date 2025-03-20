# Laste inn n??dvendige biblioteker
library(readxl)
library(dplyr)
library(stringr)
library(writexl)

# Lese inn data fra Excel-filer
data_df <- read_excel("final_data.xlsx")
kommuneendringer_df <- read_excel("Kommuneendringer_2018.xlsx")

# Rydde opp i kolonnenavnene ved ?? bruke de riktige kolonnenavnene
colnames(kommuneendringer_df) <- c("New_Code", "Old_Codes")

# Splitte gamle kommunenummer (dersom flere gamle kommuner er adskilt med mellomrom)
kommuneendringer_df$Old_Codes <- str_split(kommuneendringer_df$Old_Codes, " ")

# Lage en oppslagsliste for gamle koder til nye koder (ensidig mapping)
kommune_mapping <- setNames(rep(kommuneendringer_df$New_Code, times = sapply(kommuneendringer_df$Old_Codes, length)),
                            unlist(kommuneendringer_df$Old_Codes))

# Oppdatere kommune-koder i data_df
data_df$Municipality_Code <- sapply(data_df$Municipality_Code, function(x) {
  if (x %in% names(kommune_mapping)) {
    new_code <- kommune_mapping[[x]]
    # S??rg for at kun de f??rste 4 sifrene beholdes
    return(substr(new_code, 1, 4))  
  } else {
    return(x)  # Behold gamle verdier hvis ikke funnet
  }
})

# Lagre den oppdaterte filen
write_xlsx(data_df, "final_data_18.xlsx")

