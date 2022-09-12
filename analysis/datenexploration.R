# 7.4 Datenexploration ---------------------------------------------------------
# Beantworte folgende Fragen zum Datensatz. Berichte die Ergebnisse auch in 
# ganzen Sätzen.

# 7.4.1
# * Wie viele Probanden waren jeweils in den beiden Versuchsbedingungen?
motivation_data_cleaned %>% 
  count(practice_type)

# 7.4.2
# * Wie viele Fremdsprachen beherrschten die Probanden durchschnittlich?
mean(motivation_data_cleaned$number_languages, na.rm = TRUE)

# 7.4.3
# * Schnitten die Teilnehmenden aus der Retrieval-Gruppe besser im Abschlusstest
#   ab als die Teilnehmenden aus der Restudy-Gruppe?
motivation_data_cleaned %>% 
  group_by(practice_type) %>% 
  summarise(
    mean_final = mean(final_test)
  )

# 7.4.4
# * Wie viel Zeit verbrachten die Teilnehmenden der beiden Versuchsbedingungen
#   jeweils mit der Powerpointpräsentation? Stelle das Ergebnis auch in einem
#   Balkendiagramm dar und speichere die Visualisierung im richtigen Ordner.
motivation_data_cleaned %>% 
  group_by(practice_type) %>% 
  summarise(
    mean_powerpoint = mean(time_free_choice_period)
  ) %>% 
ggplot(aes(x = practice_type, y = mean_powerpoint)) +
  geom_col(width = 0.40) +
  labs(
    x = "condition",
    y = "time in sec"
  )

# 7.4.5
# * Unterscheiden sich die Gruppen in ihrem Interesse an Fremdsprachen? Stelle
#   das Ergebnis auch in zwei Boxplots dar.
motivation_data_cleaned %>% 
  group_by(practice_type) %>% 
  summarise(
    mean_interest_lang = mean(interest_foreign_lang)
  )

motivation_data_cleaned %>% 
  ggplot(aes(x = practice_type, y = interest_foreign_lang)) +
  geom_boxplot() +
labs(
  x = "condition",
  y = "language interest"
)

# 7.4.6
# * Visualisiere für die Retrieval-Gruppe mit einem geeigneten geom, inwiefern
#   sich ihre Leistung beim Abruf der Vokabeln über die drei Testzeitpunkte 
#   verbessert hat.
motivation_data_cleaned %>% 
  filter(practice_type == "retrieval") %>% 
  summarise(
    mean_cycle1 = mean(retrieval_practice_cycle1),
    mean_cycle2 = mean(retrieval_practice_cycle2),
    mean_final = mean(final_test)
  )

# 7.4.7
# * Schnitten diejenigen, die mindestens drei Fremdsprachen beherrschen, besser
#   im Abschlusstest ab, als diejenigen, die weniger als drei Fremdsprachen
#   sprechen?
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

# 7.4.8
# * Verbrachten diejenigen, die mehr Interesse an Schwedisch bzw. an
#   Fremdsprachen haben, längere Zeit mit der Powerpointpräsentation? Stelle
#   dies in einem Punktdiagramm dar und speichere die Visualisierung ab