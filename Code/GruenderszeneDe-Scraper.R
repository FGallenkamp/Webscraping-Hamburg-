# Gründerszene.de: Scraper

# Load/install packages
### ------------------------------------------------------------------------ ###
if(!require("pacman")) install.packages("pacman")
p_load(tidyverse, rvest, robotstxt, ggmap, rnaturalearth, ggrepel)

# Schema / Toolbox
### ------------------------------------------------------------------------ ###

# 1. Internetseite kennenlernen
# 2. Import von HTML-Dateien
# 3. Information isolieren
# 4. Iteration (loops)

### Beispiel 1: Welche StartUps sind in der Datenbank gelistet? 

# (1) Seite kennenlernen
### ------------------------------------------------------------------------ ###

# Robotstxt / Terms of Service (ToS)
paths_allowed(
  paths  = c("/Unternehmen", "/Investoren", "koepfe"), 
  domain = "https://www.gruenderszene.de/datenbank/", 
  bot    = "*"
)

# Terms of Use
# https://www.gruenderszene.de/p/impressum?ref=footer

###
# Seite "ausprobieren"
# Indem wir uns durch die Seite bewegen und hierbei die URL beobachten, verstehen
# wir die verschiedenen Bezüge und die dahinterliegende Struktur der Webseite. Wie
# in der Präsentation hervorgehoben, erfolgt das Web Scraping häufig über die 
# "Manipulation" der URL. 
###

# Ausgangsadresse
url <- "https://www.gruenderszene.de/datenbank/unternehmen/found/"

# Auch hier lässt sich die URL nutzen. Die Unternehmen sind von A-Z geordnet, sodass 
# wir die Ausgangsurl damit ergänzen, um auf einzelne Seiten navigieren zu können. 
scrape_over <- paste0(url, letters) # letters ist eingebautes R-Objekt, s. ?letters

# Namen der StartUp-Unternehmen
# Welche StartUp-Unternehmen sind in der Datenbank gelistet?

# Schritt (2) - (4)

# (2) Import von HTML-Dateien
### ------------------------------------------------------------------------ ###

# Im zweiten Schritt machen wir die Webseite in R verfügbar. Hierfür verwenden wir
# die Funktion 'read_html' (?read_html) aus dem Paket 'rvest'. 

# Erste Seite der Datenbank (Unternehmen mit "A") 
orgsA <- read_html(scrape_over[1])

# (3) Informationen isolieren
### ------------------------------------------------------------------------ ###

# Wir könnten nun die Namen der Unternehmen isolieren 

# Unternehmen, die mit "A" beginnen
orgsA %>%
  html_nodes(css = "#startlist-wrapper a") %>% # CSS-Selektor
  html_text()                                  # Als Text

# (4) Iteration (loops)
### ------------------------------------------------------------------------ ###

# Dies ließe sich auch gleich für alle Unternehmen von A-Z durchführen.
# Schritt 4: Iteration (loops)

# Loops werden in R mit "for" (base-R) oder "map" (purrr) eingesetzt, 
# siehe: https://r4ds.had.co.nz/iteration.html

# Alle Seiten sind im Objekt "scrape_over" über welches wir iterieren

### for-loop:
orgs.vec <- vector(mode = "list", length = length(scrape_over)) # aus Speichergründen

for (i in seq_along(scrape_over[1:10])) {           # Testen anhand weniger Seiten
  orgs.vec[[i]] <- read_html(scrape_over[i]) %>%
    html_nodes("#startlist-wrapper a") %>%
    html_text()
  
  print(paste0("Scrape Seite ", i, " von ", length(scrape_over)))
  
  Sys.sleep(sample(seq(1, 4, 0.5), 1)) # "friendly" scraping / crawl delay
}

### purrr::map

# Function to apply on each element of scrape_over
name_function <- function(x) {
  read_html(x) %>%
    html_nodes("#startlist-wrapper a") %>%
    html_text()
}

# Run the loop using map
orgs <- map(scrape_over, ~ {
  Sys.sleep(sample(seq(1, 4, 0.5), 1))
  name_function(.x)
}) 

# als Vector
orgs.vec <- flatten_chr(orgs)

# Distribution of first letters; just to show the result 
tibble(
  letter = letters,
  value = lengths(orgs)
) %>%
  ggplot() +
  geom_bar(aes(x = letter, y = value), stat = "identity")

## Informationen über einzelne Unternehmen sammeln
### ------------------------------------------------------------------------ ###

###
# Zu den einzelnen Unternehmen gibt es weitere Informationen, die für die Forschung
# eine größere Relevanz besitzen (Kurzbeschreibung, Team). Hierfür betrachten wir 
# wieder die URL, welche zu den einzelnen Seiten führt:
#
# Konstanter Teil: https://www.gruenderszene.de/datenbank/unternehmen/
# Variabler Teil: (z.B.) autohaus24-de
###

# Leider zeigt sich, dass der Unternehmensname, welchen wir im ersten Schritt 
# erhalten haben, nicht mit dem Link übereinstimmt. Es lässt sich jedoch das so-
# genannte href-Attribut abfragen, das Links beinhaltet.

# Analog zum ersten Schritt
linksA <- read_html(scrape_over[1]) %>%   # (A) HTML in R abbilden
  html_nodes("#startlist-wrapper a") %>%  # (B) Information isolieren
  html_attr("href")                       # (C) Attribut abfragen

# Wir ergänzen nun noch den konstanten Teil der URL:
linksA <- paste0("https://www.gruenderszene.de", linksA)
linksA[15] # Im Browser stichprobenartig prüfen.
linksA[36]

# Das Vorgehen anhand einer einzelnen Seite lässt sich wieder in eine Schleife 
# überführen.

# Funktion
link_function <- function(x) {
  read_html(x) %>%
    html_nodes("#startlist-wrapper a") %>%
    html_attr("href") 
}

# Map
org.links <- map(scrape_over, ~ {
  Sys.sleep(sample(seq(1, 4, 0.5), 1))
  link_function(.x)
}) 

# Als Vector
org.links <- flatten_chr(org.links)

# Relevante Informationen umfassen:
### ------------------------------------------------------------------------ ###

# Zum Vorgehen:
# "It’s hard to describe exactly what a functional style is, but generally I think
# it means decomposing a big problem into smaller pieces, then solving each piece 
# with a function or combination of functions. When using a functional style, you
# strive to decompose components of the problem into isolated functions that operate
# independently. Each function taken by itself is simple and straightforward to 
# understand; complexity is handled by composing functions in various ways."
# - Hadley Wickham: https://adv-r.hadley.nz/fp.html#functional-style

# Startup-Kurzbeschreibung: 
# - .profile-description > p:nth-child(2)
# Gründung:
# - .founded
# Aktivität:
# - .icon-active
# Team: 
# - .head-information a
# Adresse: 
# - .contact-data > ul:nth-child(1) > li:nth-child(3) > p:nth-child(2)
# Finanzierungsart: 
# - .company-finance > table:nth-child(1) > tbody:nth-child(1) > tr:nth-child(1) > td:nth-child(2)

# Wir nehmen alle Informationen auf

# !!! read_html ausgliedern !!!

# - Startup-Kurzbeschreibung
description_fun <- function(html){
  html_nodes(html, ".profile-description > p:nth-child(2)") %>%
    html_text()
}

# - Gründung
founded_fun <- function(html){
  html_nodes(html, ".founded") %>%
    html_text()
}

# - Aktivität
active_fun <- function(html){
  html_nodes(html, ".icon-active") %>%
  html_text()
}

# - Addresse
address_fun <- function(html){
  html_nodes(html, ".contact-data > ul:nth-child(1) > li:nth-child(3) > p:nth-child(2)") %>%
  html_text()
}

# - Team 
team_fun <- function(html){
    html_nodes(html, ".head-information a") %>%
    html_attr("href") %>%
    list()
}

# Einige Seiten verfügen nicht über alle Informationen. Dies führt dazu, dass die 
# gesamte Seite als "character(0)" angezeigt wird. Wir benutzen eine Funktion 
# "check_empty", die prüft, ob dies der Fall ist und statt "character(0)" ein 
# missing einfügt, sodass die restlichen Informationen zur Verfügung stehen. 

# Nutzt purrr::is_empty
check_empty <- function(x){
  ifelse(is_empty(x), NA_real_, x)
}

### Die einzelnen Funktionen kombinieren
startup_scraper <- function(x){
  
  html <- read_html(paste0("https://www.gruenderszene.de", x))
  
  organization <- str_extract(x, "(?<=unternehmen/).+")
  
  description <- description_fun(html) %>%
    check_empty()
  
  founded <- founded_fun(html) %>%
    check_empty()
  
  active <- active_fun(html) %>%
    check_empty()
  
  address <- address_fun(html) %>%
    check_empty()
  
  team <- team_fun(html) %>%
    check_empty()
  
  df <- tibble(organization, description, founded, active, address, team)
}

# Exemplarisch einhundert zufällige Startups
startup.df <- map_dfr(
  sample(org.links, 100), ~{
    Sys.sleep(sample(seq(0, 4, 0.5), 1))
    startup_scraper(.x)
})

# Wo befinden sich die Startups?
startup.location <- startup.df %>%
  mutate(location = str_extract(address, "(?<=[:digit:] - )[^[:digit:]]+")) %>%
  group_by(location) %>%
  summarise(count = n()) %>%
  filter(!is.na(location) & count > 1)

# Anzahl der Startups nach Ort 
ggplot(startup.location) +
  geom_bar(aes(x = fct_reorder(location, count), y = count), 
           stat = "identity")

# Auf einer geographischen Karte (package: rnaturalearth)
base.map <- ne_countries(returnclass = "sf") %>%
  filter(region_un == "Europe")

# Geocode (package: ggmap)
startup.location <- mutate_geocode(startup.location, location, "latlon", 
                                   source = "dsk")

# Map (could need refinement ofc)
ggplot(base.map) +
  geom_sf() +
  geom_text_repel(data = startup.location, aes(x = lon, y = lat, label = location),
             color = "black") +
  geom_point(data = startup.location, aes(x = lon, y = lat, size = count),
             color = "black", position = "jitter") +
  coord_sf(xlim = c(5, 17), ylim = c(55, 47)) +
  labs(title = "StartUps im deutschsprachigen Raum",
       subtitle = "Basierend auf einem zufälligen Sample (N = 100)",
       x = "", y = "", caption = "Quelle: Gründerszene.de") +
  theme_void()