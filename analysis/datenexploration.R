
# 7.4 Setup ---------------------------------------------------------------

# 7.4.1 Lade den gereinigten Datensatz
motivation <- read_csv("data/cleaned/motivation_data_cleaned.csv")


# 7.5 Datenexploration ---------------------------------------------------------
# Beantworte folgende Fragen zum Datensatz. Berichte die Ergebnisse in 
# ganzen Sätzen.


# 7.5.1 Probanden zählen
# Wie viele Probanden waren jeweils in den beiden Versuchsbedingungen?
motivation %>% 
  count(practice_type)


# 7.5.2 Durchschnittliche Anzahl Fremdsprachen
# Wie viele Fremdsprachen beherrschten die Probanden durchschnittlich?
mean(motivation$number_languages, na.rm = TRUE)


# 7.5.3 Gruppenvergleich Abschlusstest
# Schnitten die Teilnehmenden aus der Retrieval-Gruppe besser im Abschlusstest
# ab als die Teilnehmenden aus der Restudy-Gruppe?
motivation %>% 
  group_by(practice_type) %>% 
  summarise(
    mean_final = mean(final_test)
  )


# 7.6 Datenvisualisierung -------------------------------------------------

# 7.6.4 Gruppenvergleich Powerpoint
# * Wie viel Zeit verbrachten die Teilnehmenden der beiden Versuchsbedingungen
#   jeweils mit der Powerpointpräsentation? Stelle das Ergebnis in einem
#   Balkendiagramm dar und speichere die Visualisierung im richtigen Ordner
# * Berechne die Höhe der Balken mit stat_summary und füge dem Balkendiagramm
#   Fehlerbalken mit stat_summary hinzu, die eine Standardabweichung darstellen
# * Brauchst du Hilfe? Nutze den [ggplot-Cheatsheet]
#   (https://ggplot2.tidyverse.org/)
motivation %>%
  ggplot(aes(x = practice_type, y = time_free_choice_period)) +
  stat_summary(geom = "bar", fun = "mean", width = .7, alpha = .8) +
  stat_summary(geom = "errorbar", fun.data = mean_sdl,
             fun.args = list(mult = 1), width = .1) +
  stat_summary(geom = "point", fun = "mean") +
  scale_y_continuous(expand = expansion(c(0, 0.05))) +
  labs(
    x = "condition",
    y = "time in sec"
  )

ggsave("images/balkendiagramm_condition_powerpoint.png", width = 8,
       height = 5, dpi = 300)


# 7.6.5 Gruppenvergleich Interesse
# Unterscheiden sich die Gruppen in ihrem Interesse an Fremdsprachen? Stelle
# das Ergebnis auch in zwei Boxplots dar.
motivation %>% 
  group_by(practice_type) %>% 
  summarise(
    mean_interest_lang = mean(interest_foreign_lang)
  )

motivation %>% 
  ggplot(aes(x = practice_type, y = interest_foreign_lang)) +
  geom_boxplot(fill = "grey70") +
  labs(
    x = "condition",
    y = "language interest"
  )

ggsave("images/boxplots_language_interest.png", width = 8,
       height = 5, dpi = 300)


# 7.5.6 Entwicklung Retrieval-Gruppe
# * Visualisiere für die Retrieval-Gruppe mit einem geeigneten geom, inwiefern
#   sich ihre Leistung beim Abruf der Vokabeln über die drei Testzeitpunkte 
#   verbessert hat
# * Brauchst du Hilfe?
#   Orientiere dich an Aufgabe 3.7.1 aus Crashkurs 3
#   Tipps, um die Daten in ein langes Format zu bringen: ?pivot_longer, 
#   https://r4ds.had.co.nz/tidy-data.html#pivoting
motivation %>% 
  # select(practice_type, retrieval_practice_cycle1, retrieval_practice_cycle2,
  #        final_test) %>%
  filter(practice_type == "retrieval") %>% 
  pivot_longer(
    cols = c(retrieval_practice_cycle1, retrieval_practice_cycle2,
             final_test),
    names_to = "timepoint",
    values_to = "value"
  ) %>% 
  ggplot(
    aes(x = factor(timepoint, level = c('retrieval_practice_cycle1',
                                        'retrieval_practice_cycle2',
                                        'final_test')), 
        y = value)) +
  stat_summary(geom = "bar", fill = "grey10", alpha = .7, width = 0.8,
               fun = "mean") +
  stat_summary(geom = "errorbar", fun.data = mean_sdl,
               fun.args = list(mult = 1), width = .1) +
  scale_y_continuous(expand = expansion(0)) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  labs(
    x     = "Zeitpunkt",
    y     = "Richtige Vokabeln (in %)"
  )

ggsave("images/balkendiagramm_retrieval_zeitpunkte.png", width = 8,
       height = 5, dpi = 300)


# 7.5.7 Zusammenhang Sprachen und Abschlusstest
# Schnitten diejenigen, die mindestens drei Fremdsprachen beherrschen, besser
# im Abschlusstest ab, als diejenigen, die weniger als drei Fremdsprachen
# sprechen?
motivation %>%
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


# 7.6.8 Zusammenhang Interesse und Powerpoint
# Verbrachten diejenigen, die mehr Interesse an Schwedisch bzw. an
# Fremdsprachen haben, durchschnittlich längere Zeit mit der 
# Powerpointpräsentation? Stelle dies in zwei Visualisierungen dar und speichere
# sie ab
motivation %>% 
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
