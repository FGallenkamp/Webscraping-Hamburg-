# Scraping poll data from wahlrecht.de
## ---------------------------------------------------------------------- ##

# Install/load packages
## ---------------------------------------------------------------------- ##
if(!require("pacman")) install.packages("pacman")
p_load(tidyverse, lubridate, rvest)


# Scrape and clean poll results
## ---------------------------------------------------------------------- ##
vote.table <- read_html("https://www.wahlrecht.de/umfragen/allensbach.htm") %>%
  html_table(".wilko", fill = TRUE, header = TRUE) %>%
  .[[2]] %>%
  .[4:69, c(1, 3:9)] %>%
  rename("Zeitpunkt" = 1) %>%
  mutate(Zeitpunkt = parse_datetime(Zeitpunkt, format = "%d.%m.%Y")) %>%
  mutate_at(vars(2:8), str_extract, pattern = "[:digit:]+,[:digit:]?") %>%
  mutate_if(is.character, str_extract, pattern = "[:digit:]?.[:digit:]?") %>%
  mutate_if(is.character, str_replace, pattern = ",", replacement = ".") %>%
  mutate_if(is.character, as.numeric) %>%
  gather(party, vote, -Zeitpunkt)

# Plot 
## ---------------------------------------------------------------------- ##
ggplot(subset(vote.table, party != "Sonstige"),
       aes(x = Zeitpunkt, y = vote, colour = party)) +
  geom_line(size = 1.5) +
  scale_color_manual(values = c("#009EE0", "#000000", "#FFED00", "#64A12D",
                                "#BE3075", "#EB001F"),
                     guide = guide_legend(title = NULL)) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  labs(title = "Sonntagsfrage: Wenn am nächsten Sonntag Bundestagswahl wäre...",
       x = "", y = "", caption = "Quelle: wahlrecht.de (Allensbach)") +
  theme_minimal()
