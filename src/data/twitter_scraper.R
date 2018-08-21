# ----------------------------- #
# Title: Twitter scraper        #
# Author: Charles Demontigny    #
# Date: 2018-08-19              #
# ----------------------------- #

# Import packages --------------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load("tidyverse", "rtweet", "tidytext", "twitteR")

# Helper functions -------------------------------------------------------------
clean_tweets = function(tweets_df){

  # Remove words starting with #
  tweets_df = tweets_df %>% mutate(sentences = gsub("#\\w+ *", "", sentences))

  # Remove words starting with @
  tweets_df = tweets_df %>% mutate(sentences = gsub("@\\w+ *", "", sentences))

  # Remove strings starting with https://
  tweets_df = tweets_df %>% mutate(sentences = gsub("(f|ht)tp(s?)://(.*)[.][a-z]+", "", sentences))

  # Remove strings starting with /
  tweets_df = tweets_df %>% mutate(sentences = gsub("/\\w+ *", "", sentences))

  # Remove all non-latin symbols
  tweets_df = tweets_df %>% mutate(sentences = iconv(sentences, "UTF-8", "latin1", sub=""))

  # Remove numbers
  tweets_df = tweets_df %>% mutate(sentences = gsub('[0-9]+', '', sentences))

  # Remove ":"
  tweets_df = tweets_df %>%
    mutate(sentences = str_replace_all(string = sentences, pattern = ":", replacement = ""))

  # Remove \n
  tweets_df = tweets_df %>%
    mutate(sentences = str_replace_all(string = sentences, pattern = "\n", replacement = ""))

  # Remove RT :
  tweets_df = tweets_df %>%
    mutate(sentences = str_replace_all(string = sentences, pattern = "RT", replace = ""))

  # Remove white space from start and end of strings
  tweet_df = tweets_df %>%
    mutate(sentences = str_trim(sentences))

  # Remove comma
  tweets_df = tweets_df %>%
    mutate(sentences = str_replace_all(string = sentences, pattern = ",", replacement = ""))

  # Remove sentences with less than 40 characters --------------------------------
  char = (nchar(tweets_df$sentences) >= 40)
  tweets_df = tweets_df[char, ]

  # Cut strings to have exactly 40 characters ------------------------------------
  tweets_df = tweets_df %>% mutate(sentences = str_sub(string = sentences, start = 1, end = 40))

}

# Main function ----------------------------------------------------------------
scrape_tweets = function(saving_path, n, french_key_word = 'Paris', spanish_key_word = 'Madrid',
                         english_key_word = 'London'){

  # Setup twitter access
  access_token = "983053340413186049-tNf2koGExSoWAZ2obLPpBppPOYe9hV2"
  access_secret = "DyRkqUZdDiIoD91owToR0Jdql26duLN7vGoNIPHZmaMGv"
  consumer_key = "32P9ZgljaP4wCx4EYmk9CUihf"
  consumer_secret = "86e4Y3lTY5u1kBVwePPgixfs42NzTqqku1uItMK1ZXQGGt5fgV"

  setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

  # Search tweets
  francais = searchTwitter(french_key_word, n=n,lang="fr")
  espagnol = searchTwitter(spanish_key_word, n = n, lang = "es")
  anglais = searchTwitter(english_key_word, n = n, lang = "en")
  tweets = list(francais, anglais, espagnol)

  # Reshape tweets into DataFrames
  tweets_df = map(tweets, twListToDF) %>%
    map(~select(.x, sentences = text)) %>%
    map2(c("fr", "en", "es"), ~mutate(.x, language = .y)) %>%
    bind_rows %>%
    as_data_frame %>%
    select(language, sentences)

  # Clean tweets
  tweets_df = clean_tweets(tweets_df)

  # Write csv
  if(!dir.exists(saving_path)){
    dir.create("data")
    dir.create("data/raw")
  }

  write_delim(x = tweets_df, path = paste0(saving_path, "/tweets_", Sys.Date(), ".csv"), delim = ",")

}

# Execute main function --------------------------------------------------------
scrape_tweets("data/raw", n = 1000)
