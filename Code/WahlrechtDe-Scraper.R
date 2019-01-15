# Scraping poll data from wahlrecht.de
## ---------------------------------------------------------------------- ##

# Install/load packages
## ---------------------------------------------------------------------- ##
if(!require("pacman")) install.packages("pacman")
p_load(tidyverse, lubridate, rvest)

# Results of "Sonntagsfrage" for a particular polling institute

# Scrape and clean poll results
## ---------------------------------------------------------------------- ##
vote.table <- read_html("https://www.wahlrecht.de/umfragen/allensbach.htm") %>%
  html_nodes(css = ".wilko") %>%
  html_table(fill = TRUE, header = TRUE) %>%
  .[[1]] %>%
  .[4:nrow(.), c(1, 3:9)] %>%
  rename("Zeitpunkt" = 1) %>%
  mutate(Zeitpunkt = parse_datetime(Zeitpunkt, format = "%d.%m.%Y")) %>%
  mutate_if(is.character, str_extract, pattern = "[:digit:]+,?[:digit:]?") %>%
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

# Gathering results of all polling institutes
## ---------------------------------------------------------------------- ##

# (1) Get links to all subpages (for each polling institute)
institute.href <- read_html("https://www.wahlrecht.de/umfragen/index.htm") %>%
  html_nodes(".in a") %>%
  html_attr("href") %>%
  tibble(institute = str_extract(., "[:alpha:]+(?=.)"),
         link = paste0("https://www.wahlrecht.de/umfragen/", .)) %>%
  rename("href" = 1)

# For filtering
parties <- c("CDU/CSU", "SPD", "GRÜNE", "FDP", "LINKE", "AfD") 

# (2) Create a function that grabs the tables 
poll_scraper <- function(x) {
  read_html(x) %>%
    html_nodes(".wilko") %>%
    html_table(fill = TRUE, header = TRUE) %>%
    .[[1]] %>%
    .[min(                                                                         # Which row is the first one with a date? 
      which(
        str_detect(
          string = .[,1], "[:digit:]+.?[:digit:]+.?[:digit:]+") == TRUE)):nrow(.),
      c(1, which(colnames(.) %in% parties))] %>%                                   # Columns that entail date + "parties"
    rename("Zeitpunkt" = 1) %>%
    mutate(Zeitpunkt = parse_datetime(Zeitpunkt, format = "%d.%m.%Y")) %>%
    mutate_if(is.character, str_extract, pattern = "[:digit:]+,?[:digit:]?") %>%
    mutate_if(is.character, str_replace, pattern = ",", replacement = ".") %>%
    mutate_if(is.character, as.numeric) %>%
    gather(party, vote, -Zeitpunkt)
}

# (3) Grab all polls of all institute
polls <- map(institute.href$link, poll_scraper) %>% 
  setNames(institute.href$institute) # Name the list

# (4) Name lists and combine in a data set
polls.df <- polls %>%
  bind_rows(.id = "institute")

# Plot 
## ---------------------------------------------------------------------- ##
ggplot(subset(polls.df, year(Zeitpunkt) > 2014),
       aes(x = Zeitpunkt, y = vote, colour = party)) +
  geom_line(size = 1.5) +
  facet_wrap(~institute) +
  scale_color_manual(values = c("#009EE0", "#000000", "#FFED00", "#64A12D",
                                "#BE3075", "#EB001F"),
                     guide = guide_legend(title = NULL)) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  labs(title = "Sonntagsfrage: Wenn am nächsten Sonntag Bundestagswahl wäre...",
       x = "", y = "", caption = "Quelle: wahlrecht.de (Allensbach)") +
  theme_minimal()
