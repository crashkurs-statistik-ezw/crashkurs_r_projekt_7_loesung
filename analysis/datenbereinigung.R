# 7.1 Setup -------------------------------------------------------------

# 7.1.1 Projekt aufsetzen
# * Setze die Ordnerstruktur für dieses Paket auf. Orientiere dich dabei an den
#   Projekten 1 bis 6
# * Lade den Datensatz aus OSF (https://osf.io/cwq6u/) in den richtigen Ordner

# 7.1.2 Pakete laden
# Lade die Pakete janitor und haven
library(janitor)
library(haven)

# 7.2 Daten einlesen ------------------------------------------------------

# 7.2.1
# * Lese den zuvor heruntergeladenen Datensatz ein und speichere ihn unter 
#   einem sinnvollen Namen
motivation_data <- read_sav("data/motivation_data.sav")


# 7.3 Daten bereinigen ---------------------------------------------

# 7.3.1
# * Wandle die Variablennamen mit clean_names in snake case um
# * Kodiere die Variable practice_type mit case_when um und überschreibe sie:
#   1 -> retrieval, 2 -> restudy
# * Speichere den bereinigten Datensatz als neue Variable
motivation_data_cleaned <- motivation_data %>% 
  clean_names(case = "snake") %>%
  mutate(
    practice_type = case_when(
      practice_type == 1 ~ "retrieval",
      practice_type == 2 ~ "restudy"
    ))


# 7.3 Datenexport ---------------------------------------------------------

# 7.3.1
# * Exportiere den Datensatz in den richtigen Ordner
# * Speichere die Daten sowohl als csv-Datei als auch als sav-Datei
write_csv(motivation_data_cleaned, "data/export/motivation_data_cleaned.csv")
write_sav(motivation_data_cleaned, "data/export/motivation_data_cleaned.csv")
