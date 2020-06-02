#!/usr/bin/env Rscript --vanilla
# set expandtab ft=R ts=4 sw=4 ai fileencoding=utf-7
#
# Author: JR
# Maintainer(s): JR
# License: GPL V.02
#
# -----------------------------------------------------------
# dcblackoutinvestigation_public/analyze/src/analyze.R

# import the data into R and send to clean task

pacman::p_load("tidyverse", "here", "assertr", 
               "janitor", "lubridate", "tidytext", 
               "stopwords", "hms")
# import data
files <- list(
  auth = here::here("dcblackoutinvestigation_public/analyze/input/authors_clean_df.csv"),
  blackout = here::here("dcblackoutinvestigation_public/analyze/input/blackout_clean_df.csv"),
  
  auth_tokens = here("dcblackoutinvestigation_public/report/input/auth_tokens.csv"),
  auth_users = here("dcblackoutinvestigation_public/report/input/authors_frequsers.csv"), 
  auth_bigrams = here("dcblackoutinvestigation_public/report/input/authors_freqbigrams.csv"),
  
  blackout_tokens = here("dcblackoutinvestigation_public/report/input/blackout_tokens.csv"),
  blackout_users = here("dcblackoutinvestigation_public/report/input/blackout_frequsers.csv"), 
  blackout_bigrams = here("dcblackoutinvestigation_public/report/input/blackout_freqbigrams.csv"),
  blackout_first = here("dcblackoutinvestigation_public/report/input/blackout_freqfirst.csv"),
  blackout_second = here("dcblackoutinvestigation_public/report/input/blackout_freqsecond.csv")
)

stopifnot(is_empty(files) != TRUE & length(files) == 10)

# call lists of language-specific stop-words from stopwords package
# can add more languages as needed
stopwords_smart <- stopwords(source = "smart")
stopwords_en <- stopwords("en")
stopwords_ro <- stopwords("romanian")
stopwords_ru <- stopwords("russian")
stopwords_sp <- stopwords("spanish")
stopwords_pg <- stopwords("portuguese")
stopwords_fr <- stopwords("french")
stopwords_de <- stopwords("german")

# words I identified from initial runs as not of interest, feel free to 
# delete them from the list to include them in the results again
specific_swords <- c("twitter", "twitter.com", "pic.twitter.com",
                     "minecraft", "gaming.youtube.com", "giveaway", 
                     "vgotrading", "tho", "it's", "i'm", "https", "watchgamestv", 
                     "vgogiveaway", "youtu.be", "ta")

# remove useless bigrams, mostly TSwift related but other phrases
bad_bigrams <- c("cruel summer", "good music", "album sales", "singles sales", 
                 "music video", "track baby", "miss americana", "taylor swift", 
                 "rt follow", "tweet follow", "subscribe keyword", "stupid love", 
                 "it's gonna", "enter follow", "www.youtube.com watch", 
                 "social media", "ich bin", "ist ein", "rtlike follow", 
                 "rtlike subscribe", "road didn't", "let's make", "makes good")

# filter out taylor swift related usernames 
tay_names <- c("taylorswift13", "taylorvotestats", "tayiorvotestats")

#create %notin%
"%notin%" <- Negate("%in%")

## Read in data, use a loop to accomodate new sheets

# remove variables with no info in columns
# clean up tweets (source ref: ( Hicks , 2014) and ref: (Stanton 2013))
auth_df <- tibble(read_csv(files$auth,
                             col_names = TRUE, 
                             na = c("", "[]"))) %>%
  mutate(clean_tweet = gsub("&amp", "", tweet), 
         clean_tweet = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", clean_tweet),
         clean_tweet = gsub("@\\w+", "", clean_tweet),
         clean_tweet = gsub("[[:punct:]]", "", clean_tweet),
         clean_tweet = gsub("[[:digit:]]", "", clean_tweet),
         clean_tweet = gsub("http\\w+", "", clean_tweet),
         clean_tweet = gsub("[ \t]{2,}", "", clean_tweet),
         clean_tweet = gsub("^\\s+|\\s+$", "", clean_tweet),
         clean_tweet = gsub("\\\\", "", clean_tweet),
         tweet_txt = as.character(clean_tweet),
         date_rec = ymd(date), 
         time_rec = as_hms(time)) %>% 
  filter(clean_tweet %notin% specific_swords) %>%
  select( - c(cashtags, near, geo, 
              source, retweet_id, retweet_date, 
              translate,trans_src, trans_dest))

stopifnot(ncol(auth_df) == 29 & nrow(auth_df) == 1863)

############################################################################
# auth.csv #

auth_tokens <- auth_df %>%
  unnest_tokens(bigram, tweet_txt, token = "ngrams", n = 2) %>%
  filter(bigram %notin% bad_bigrams) %>%
  separate(bigram, into = c("first","second"), sep = " ", remove = FALSE) %>%
  filter(first %notin% c(specific_swords, stopwords_smart, stopwords_en, 
                         stopwords_ro,  stopwords_ru, stopwords_sp, 
                         stopwords_pg, stopwords_fr, stopwords_de), 
         second %notin% c(specific_swords, stopwords_smart, stopwords_en, 
                          stopwords_ro,  stopwords_ru, stopwords_sp, 
                          stopwords_pg, stopwords_fr, stopwords_de)) %>%
  filter(str_detect(first, "[a-z]") &
           str_detect(second, "[a-z]")) %>%
  drop_na(bigram) %>%
  filter(username %notin% tay_names)

write_delim(auth_tokens, files$auth_tokens, delim = "|")

# most active usernames (appearing more than 7 times) in authors dataset
users_auth <- auth_tokens %>%
  count(username, sort = TRUE) %>%
  mutate(username = reorder(username, n)) %>%
  filter(n > 7) %>%
  write_delim(files$auth_users, delim = "|")

# characteristics of most common usernames
#########################################

# sort by user, create new object to work from
auth_tokens_u <- auth_tokens

# most commonly used bigrams
bigrams_auth <- auth_tokens_u %>%
  count(bigram, sort = TRUE) %>%
  arrange(desc(n)) %>%
  filter(n > 3)

stopifnot(ncol(bigrams_auth) == 2 & nrow(bigrams_auth) == 32)

write_delim(bigrams_auth, files$auth_bigrams, delim = "|")

##############################################################################

blackout_df <- tibble(read_csv(files$blackout, 
                               col_names = TRUE, na = c("", "[]"))) %>%
  mutate(clean_tweet = gsub("&amp", "", tweet), 
         clean_tweet = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", clean_tweet),
         clean_tweet = gsub("@\\w+", "", clean_tweet),
         clean_tweet = gsub("[[:punct:]]", "", clean_tweet),
         clean_tweet = gsub("[[:digit:]]", "", clean_tweet),
         clean_tweet = gsub("http\\w+", "", clean_tweet),
         clean_tweet = gsub("[ \t]{2,}", "", clean_tweet),
         clean_tweet = gsub("^\\s+|\\s+$", "", clean_tweet),
         clean_tweet = gsub("\\\\", "", clean_tweet),
         tweet_txt = as.character(clean_tweet),
         date_rec = ymd(date), 
         time_rec = as_hms(time)) %>% 
  filter(clean_tweet %notin% specific_swords) %>%
  select( - c(cashtags, near, geo, 
              source, retweet_id, retweet_date, 
              translate,trans_src, trans_dest))

stopifnot(ncol(blackout_df) == 29 & nrow(blackout_df) == 16299)

blackout_tokens <- blackout_df %>%
  unnest_tokens(bigram, tweet_txt, token = "ngrams", n = 2) %>%
  filter(bigram %notin% bad_bigrams) %>%
  separate(bigram, into = c("first","second"), sep = " ", remove = FALSE) %>%
  filter(first %notin% c(specific_swords, stopwords_smart, stopwords_en, 
                         stopwords_ro,  stopwords_ru, stopwords_sp, 
                         stopwords_pg, stopwords_fr, stopwords_de), 
         second %notin% c(specific_swords, stopwords_smart, stopwords_en, 
                          stopwords_ro,  stopwords_ru, stopwords_sp, 
                          stopwords_pg, stopwords_fr, stopwords_de)) %>%
  filter(str_detect(first, "[a-z]") &
           str_detect(second, "[a-z]")) %>%
  drop_na(bigram) %>%
  filter( username %notin% tay_names)

write_delim(blackout_tokens, files$blackout_tokens, delim = "|")

# plot most active usernames (appearing more than 55 times) in blackout dataset
users_blackout <- blackout_tokens %>%
  count(username, sort = TRUE) %>%
  mutate(username = reorder(username, n)) %>%
  filter(n > 55)
stopifnot(ncol(users_blackout) == 2 & nrow(users_blackout) == 63)

write_delim(users_blackout, files$blackout_users, delim = "|")

# characteristics of most common usernames
#########################################
# create new df
blackout_tokens_u <- blackout_tokens

# most commonly used bigrams
bigrams_blackout <- blackout_tokens_u %>%
  count(bigram, sort = TRUE) %>%
  arrange(desc(n)) %>%
  filter(n > 60)
stopifnot(ncol(bigrams_blackout) == 2 & nrow(bigrams_blackout) == 54)
write_delim(bigrams_blackout, files$blackout_bigrams, delim = "|")

# most commonly used first terms 
f_blackout <- blackout_tokens_u %>%
  count(first, sort = TRUE) %>%
  arrange(desc(n)) %>%
  filter(n > 100)

write_delim(f_blackout, files$blackout_first, delim = "|")

# most commonly used second terms 
s_blackout <- blackout_tokens_u %>%
  count(second, sort = TRUE) %>%
  arrange(desc(n)) %>%
  filter(n > 90)

write_delim(s_blackout, files$blackout_second, delim = "|")

# done 
