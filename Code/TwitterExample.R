# (Very) Basic twitter analysis using rtweet
# More information available: https://rtweet.info/

# Install and load packages
## ---------------------------------------------------------------------- ##
if (!require("pacman")) install.packages("pacman")
p_load(rtweet, tidyverse, tidytext)

# Load the auth_keys (Authentication)
# Install and load packages
keys <- readLines(
  file("C:\\Users\\guelzauf\\Seafile\\Meine Bibliothek\\Projekte\\twitter_apikey.txt")
)

# Authenticate 
create_token(
  app = "FabianFuchsius",
  consumer_key = keys[[1]],
  consumer_secret = keys[[2]]
)

# Tweets with the hashtag "#brexit"
## ---------------------------------------------------------------------- ##
brexit.tweet <- search_tweets(
  "#brexit",
  n = 1000,
  include_rts = FALSE
)

# Histogram of tweet lengths
# tokenize using tidytext (https://www.tidytextmining.com/index.html)
brexit.tidy <- brexit.tweet %>%
  unnest_tokens(word, text) %>%
  group_by(status_id) %>%
  summarize(text.length = n()) %>%
  ggplot() + 
  geom_histogram(aes(x = text.length))
