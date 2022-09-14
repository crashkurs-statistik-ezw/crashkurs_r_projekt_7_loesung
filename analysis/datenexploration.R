# 7.4 Datenexploration ---------------------------------------------------------
# Beantworte folgende Fragen zum Datensatz. Berichte die Ergebnisse auch in 
# ganzen Sätzen.

# 7.4.1
# Wie viele Probanden waren jeweils in den beiden Versuchsbedingungen?
motivation_data_cleaned %>% 
  count(practice_type)

# 7.4.2
# Wie viele Fremdsprachen beherrschten die Probanden durchschnittlich?
mean(motivation_data_cleaned$number_languages, na.rm = TRUE)

# 7.4.3
# Schnitten die Teilnehmenden aus der Retrieval-Gruppe besser im Abschlusstest
# ab als die Teilnehmenden aus der Restudy-Gruppe?
motivation_data_cleaned %>% 
  group_by(practice_type) %>% 
  summarise(
    mean_final = mean(final_test)
  )

# 7.4.4
# Wie viel Zeit verbrachten die Teilnehmenden der beiden Versuchsbedingungen
# jeweils mit der Powerpointpräsentation? Stelle das Ergebnis auch in einem
# Balkendiagramm dar und speichere die Visualisierung im richtigen Ordner.
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

ggsave("images/balkendiagramm_condition_powerpoint.png", width = 8,
       height = 5, dpi = 300)

# 7.4.5
# Unterscheiden sich die Gruppen in ihrem Interesse an Fremdsprachen? Stelle
# das Ergebnis auch in zwei Boxplots dar.
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

ggsave("images/boxplots_language_interest.png", width = 8,
       height = 5, dpi = 300)

# 7.4.6
# Visualisiere für die Retrieval-Gruppe mit einem geeigneten geom, inwiefern
# sich ihre Leistung beim Abruf der Vokabeln über die drei Testzeitpunkte 
# verbessert hat.
motivation_data_cleaned %>% 
  select(practice_type, retrieval_practice_cycle1, retrieval_practice_cycle2,
         final_test) %>%
  filter(practice_type == "retrieval") %>% 
  pivot_longer(
    cols = c(retrieval_practice_cycle1, retrieval_practice_cycle2,
             final_test),
    names_to = "timepoint",
    values_to = "value"
  ) %>% 
  group_by(timepoint) %>% 
  summarise(
    mean = mean(value, na.rm = TRUE)
  ) %>% 
  ggplot(
    aes(x = factor(timepoint, level = c('retrieval_practice_cycle1',
                                        'retrieval_practice_cycle2',
                                        'final_test')), 
        y = mean)) +
  geom_col(fill = "grey10", alpha = .7, width = 0.8) +
  scale_y_continuous(expand = expansion(0)) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  labs(
    x     = "Zeitpunkt",
    y     = "Richtige Vokabeln (in %)"
  )

ggsave("images/balkendiagramm_retrieval_zeitpunkte.png", width = 8,
       height = 5, dpi = 300)


# 7.4.7
# Schnitten diejenigen, die mindestens drei Fremdsprachen beherrschen, besser
# im Abschlusstest ab, als diejenigen, die weniger als drei Fremdsprachen
# sprechen?
motivation_data_cleaned %>%
  mutate(
    min_3_lang = case_when(
      number_languages > 2 ~ "yes",
      number_languages <= 2 ~ "no"
    )
  ) %>% 
  group_by(min_3_lang) %>% 
  summarise(
    mean_final = mean(final_test)
  )

# 7.4.8
# Verbrachten diejenigen, die mehr Interesse an Schwedisch bzw. an
# Fremdsprachen haben, durchschnittlich längere Zeit mit der 
# Powerpointpräsentation? Stelle dies in zwei Visualisierungen dar und speichere
# sie ab
motivation_data_cleaned %>% 
  group_by(interest_swedish) %>% 
  summarise(
    mean_powerpoint = mean(time_free_choice_period)
  ) %>% 
  ggplot(aes(x = interest_swedish, y = mean_powerpoint)) +
  geom_col()

ggsave("images/balkendiagramm_interest_swedish_powerpoint.png", width = 8,
       height = 5, dpi = 300)

motivation_data_cleaned %>% 
  group_by(interest_foreign_lang) %>% 
  summarise(
    mean_powerpoint = mean(time_free_choice_period)
  ) %>% 
  ggplot(aes(x = interest_foreign_lang, y = mean_powerpoint)) +
  geom_col()

ggsave("images/balkendiagramm_interest_languages_powerpoint.png", width = 8,
       height = 5, dpi = 300)
