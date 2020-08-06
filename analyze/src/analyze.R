#!/usr/bin/env Rscript --vanilla
# set expandtab ft=R ts=4 sw=4 ai fileencoding=utf-7
#
# Author: JR
# Maintainer(s): JR
# License: GPL V.3.0
#
# -----------------------------------------------------------
# dcblackoutinvestigation_public/analyze/src/analyze.R

# import the data into R and send to clean task

pacman::p_load("tidyverse", "here", "assertr", 
               "janitor", "lubridate", "tidytext", 
               "stopwords", "hms")
# import data
files <- list(
  auth_tok = 
    here("analyze/input/authors_tokens_df.csv"),
  blackout_tok = 
    here("analyze/input/blackout_tokens_df.csv"),
  
  auth_users = 
    here("report/input/authors_frequsers.csv"), 
  auth_freq = 
    here("report/input/authors_freqterms.csv"),
  
  blackout_users = 
    here("report/input/blackout_frequsers.csv"), 
  blackout_freq = 
    here("report/input/blackout_freqterms.csv")
)

stopifnot(is_empty(files) != TRUE & length(files) == 6)

auth_tokens <- tibble(read_delim(files$auth_tok, col_names = TRUE, delim = "|"))

auth_tokens <- auth_tokens %>%
  verify(ncol(auth_tokens) == 15 & nrow(auth_tokens) == 10357)

blackout_tokens <- tibble(read_delim(files$blackout_tok, col_names = TRUE, delim = "|"))

blackout_tokens <- blackout_tokens %>%
  verify(ncol(blackout_tokens) == 15 & nrow(blackout_tokens) == 156037)

############################################################################
# auth.csv #

# most active usernames (appearing more than 3 times/day) in authors dataset
# over time
users_auth <- auth_tokens %>%
  group_by(date_rec, time_rec) %>%
  count(username, sort = TRUE) %>%
  mutate(username = reorder(username, n))

users_auth <- users_auth %>%
  verify(ncol(users_auth) == 4 & nrow(users_auth) == 1618) %>%
  write_delim(files$auth_users, delim = "|")

# characteristics of most common usernames
#########################################

# most commonly used terms of all usernames in auth datasets which appear more than once
terms_auth <- auth_tokens %>%
  group_by(date_rec, username, time_rec) %>%
  count(word, sort = TRUE) %>%
  mutate(word = as.character(word)) %>%
  arrange(desc(n))

terms_auth  <-terms_auth %>%
  verify(ncol(terms_auth) == 5 & nrow(terms_auth) == 9065) %>%
  write_delim(files$auth_freq, delim = "|")

##############################################################################

# plot most active usernames (tweeting more than 5 times/day) in blackout dataset
# since all tweets are from 6/01, data are not grouped by date

users_blackout <- blackout_tokens %>%
  group_by(time_rec) %>%
  count(username, sort = TRUE) %>%
  mutate(username = reorder(username, n)) %>%
  filter(n > 5) 

users_blackout <- users_blackout %>%
  verify(ncol(users_blackout) == 3 & nrow(users_blackout) == 10755) %>%
  write_delim(files$blackout_users, delim = "|")

# most commonly used terms of all usernames in auth datasets by time
terms_blackout <- blackout_tokens %>%
  group_by(username, time_rec) %>%
  count(word, sort = TRUE) %>%
  mutate(word = as.character(word)) %>%
  arrange(desc(n))

terms_blackout <- terms_blackout %>%
  verify(ncol(terms_blackout) == 4 & nrow(terms_blackout) == 150264) %>%
  write_delim(files$blackout_freq, delim = "|")

# done 
