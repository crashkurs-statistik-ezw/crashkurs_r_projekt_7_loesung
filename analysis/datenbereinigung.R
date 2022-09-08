# 2.0 Setup -------------------------------------------------------------
# 2.0.1
# (Installiere und) lade die Pakete janitor und haven
library(janitor)
library(haven)

# 2.1 Daten einlesen ------------------------------------------------------

# 2.1.0
# * Lese den Datensatz data/motivation_data.sav ein
# * Speichere den Datensatz in der Variable student_data
motivation_data <- read_sav("data/motivation_data.sav")


# 2.2 Daten bereinigen ---------------------------------------------

# 2.2.0
# * Wandle die Variablennamen mit clean_names in snake case um
# * Kodiere die Variable practice_type mit case_when um: 1 -> retrieval,
#   2 -> restudy
#   Speichere die umkodierte Variable unter dem gleichen Variablennamen
# * Verbinde beide Bereinigungsschritte mit dem Pipe-Operator
# * Speichere den bereinigten Datensatz in der Variable motivation_data_cleaned
motivation_data_cleaned <- motivation_data %>% 
  clean_names(case = "snake") %>%
  mutate(
    practice_type = case_when(
      practice_type == 1 ~ "retrieval",
      practice_type == 2 ~ "restudy"
    ))


# 2.3 Datenexport ---------------------------------------------------------

# 2.3.0
# * Exportiere den Datensatz in den Ordner data/cleaned
# * Speichere die Daten sowohl als csv-Datei als auch als sav-Datei
write_csv(motivation_data_cleaned, "data/export/motivation_data_cleaned.csv")
write_sav(motivation_data_cleaned, "data/export/motivation_data_cleaned.csv")
