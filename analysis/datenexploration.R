# Beantworte folgende Fragen zum Datensatz. Berichte die Ergebnisse auch in 
# ganzen Sätzen.

# * Wie viele Probanden waren jeweils in den beiden Versuchsbedingungen?
# * Wie viele Fremdsprachen beherrschten die Probanden durchschnittlich?
# * Wie viel Zeit verbrachten die Teilnehmenden der beiden Versuchsbedingungen
#   jeweils mit der Powerpointpräsentation? Stelle das Ergebnis auch in einem
#   Balkendiagramm dar.
# * Unterscheiden sich die Gruppen in ihrem Interesse an Fremdsprachen? Stelle
#   das Ergebnis auch in zwei Boxplots dar.
# * Verbrachten diejenigen, die mehr Interesse an Schwedisch bzw. an
#   Fremdsprachen haben, längere Zeit mit der Powerpointpräsentation? Stelle
#   dies in einem Punktdiagramm dar
# * Schnitten diejenigen, die mindestens drei Fremdsprachen beherrschen, besser
#   im Abschlusstest ab, als diejenigen, die weniger als drei Fremdsprachen
#   sprechen?
# * Schnitten die Teilnehmenden aus der Retrieval-Gruppe besser im Abschlusstest
#   ab als die Teilnehmenden aus der Restudy-Gruppe?
# * Visualisiere für die Retrieval-Gruppe mit einem geeigneten geom, inwiefern
#   sich ihre Leistung beim Abruf der Vokabeln über die drei Testzeitpunkte 
#   verbessert hat.

motivation_data_cleaned %>% 
  count(practice_type)

mean(motivation_data_cleaned$number_languages, na.rm = TRUE)

motivation_data_cleaned %>% 
  group_by(practice_type) %>% 
  summarise(
    mean_powerpoint = mean(time_free_choice_period)
  )

motivation_data_cleaned %>% 
  group_by(practice_type) %>% 
  summarise(
    mean_interest_lang = mean(interest_foreign_lang)
  )

motivation_data_cleaned %>%
  mutate(
    min_3_lang = case_when(
      number_languages > 2 ~ "yes",
      number_languages <= 2 ~ "no"
    )
  ) %>% 
  group_by(min_3_lang) %>% 
  summarise(
    mean_final= mean(final_test)
  )

motivation_data_cleaned %>% 
  group_by(practice_type) %>% 
  summarise(
    mean_final = mean(final_test)
  )

motivation_data_cleaned %>% 
  filter(practice_type == "retrieval") %>% 
  summarise(
    mean_cycle1 = mean(retrieval_practice_cycle1),
    mean_cycle2 = mean(retrieval_practice_cycle2),
    mean_final = mean(final_test)
  )
