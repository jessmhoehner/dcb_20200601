#!/usr/bin/env Rscript --vanilla
# set expandtab ft=R ts=4 sw=4 ai fileencoding=utf-7
#
# Author: JR
# Maintainer(s): JR
# License: GPL V.02
#
# -----------------------------------------------------------
# dcblackoutinvestigation_public/report/src/report.R

# import the data into R and send to clean task

pacman::p_load("tidyverse", "here", "assertr")

# import data
files <- list(
  auth_tokens =
    here("dcblackoutinvestigation_public/report/input/auth_tokens.csv"), 
  auth_users = 
    here("dcblackoutinvestigation_public/report/input/authors_frequsers.csv"), 
  auth_bigrams = 
    here("dcblackoutinvestigation_public/report/input/authors_freqbigrams.csv"),
  
  blackout_tokens = 
    here("dcblackoutinvestigation_public/report/input/blackout_tokens.csv"),
  blackout_users = 
    here("dcblackoutinvestigation_public/report/input/blackout_frequsers.csv"), 
  blackout_bigrams = 
    here("dcblackoutinvestigation_public/report/input/blackout_freqbigrams.csv"),
  blackout_first = 
    here("dcblackoutinvestigation_public/report/input/blackout_freqfirst.csv"),
  blackout_second = 
    here("dcblackoutinvestigation_public/report/input/blackout_freqsecond.csv"),
  
  auth_users_plot = 
    here("dcblackoutinvestigation_public/report/output/authors_frequsers.png"), 
  auth_timehist_plot = 
    here("dcblackoutinvestigation_public/report/output/authors_timehist.png"), 
  auth_bigrams_plot = 
    here("dcblackoutinvestigation_public/report/output/authors_freqbigrams.png"),
  
  blackout_users_plot = 
    here("dcblackoutinvestigation_public/report/output/blackout_frequsers.png"), 
  blackout_timehist_plot = 
    here("dcblackoutinvestigation_public/report/output/blackout_timehist.png"), 
  blackout_bigrams_plot = 
    here("dcblackoutinvestigation_public/report/output/blackout_freqbigrams.png"), 
  blackout_first_plot = 
    here("dcblackoutinvestigation_public/report/output/blackout_freqfirst.png"), 
  blackout_second_plot = 
    here("dcblackoutinvestigation_public/report/output/blackout_freqsecond.png") 
  
)

stopifnot(is_empty(files) != TRUE & length(files) == 16)

# authors.csv 

# read in data 

users_auth <- as.data.frame(read_delim(files$auth_users, delim="|"))

# histogram of numbers of tweets
users_auth %>%
  ggplot(aes(reorder(username, n), n, username)) +
  geom_col() +
  coord_flip() +
  labs(title = "Usernames with > 7 tweets in authors dataset", 
       y = "count of number of tweets", 
       x = "number of tweets")

ggsave(files$auth_users_plot, 
       plot = last_plot(), 
       device = NULL,
       path = NULL,
       scale = 1,
       width = NA,
       height = NA,
       units = "in",
       dpi = 300,
       limitsize = TRUE)

# for each dataset, when was each of the most frequent usernames most active?
# did they post all at once or over the course of a day?

auth_tokens <- as.data.frame(read_delim(files$auth_tokens, delim="|")) %>%
  select(date_rec, time_rec, username, clean_tweet)

# top 5 users 
users_auth_top5 <- users_auth$username[1:5]

auth_tweets <- bind_rows(auth_tokens) %>%
  filter(username %in% users_auth_top5)

ggplot(auth_tweets, aes(time_rec, fill = username)) + 
  geom_histogram(position = "identity", bins = 100) +
  theme_minimal() +
  labs(title = "Distribution of Tweets of 5 Most Active Users in Authors dataset", 
       y = "count of number of tweets", 
       x = "Time")

ggsave(files$auth_timehist_plot, 
       plot = last_plot(), 
       device = NULL,
       path = NULL,
       scale = 1,
       width = NA,
       height = NA,
       units = "in",
       dpi = 300,
       limitsize = TRUE)
  
# plot most frequent bigrams (>3 times)

bigrams_auth <- as.data.frame(read_delim(files$auth_bigrams, delim="|"))

bigrams_auth %>%
  ggplot(aes(reorder(bigram, n), n, bigram)) +
  geom_col() +
  xlab(NULL) +
  theme_minimal() +
  coord_flip() +
  labs(title = "Bigrams appearing in > 3 tweets in authors dataset", 
       y = "Number of Occurances", 
       x = "Bigram")

ggsave(files$auth_bigrams_plot, 
       plot = last_plot(), 
       device = NULL,
       path = NULL,
       scale = 1,
       width = NA,
       height = NA,
       units = "in",
       dpi = 300,
       limitsize = TRUE)

################################################################################

# blackout.csv

users_blackout <- as.data.frame(read_delim(files$blackout_users, delim="|"))

# histogram of numbers of tweets
users_blackout %>%
  ggplot(aes(reorder(username, n), n, username)) +
  coord_flip() + 
  geom_col() +
  theme_minimal() +
  labs(title = "Usernames with > 55 tweets in blackout dataset", 
       y = "Username", 
       x = "number of tweets")

ggsave(files$blackout_users_plot, 
       plot = last_plot(), 
       device = NULL,
       path = NULL,
       scale = 1,
       width = NA,
       height = NA,
       units = "in",
       dpi = 300,
       limitsize = TRUE)

blackout_tokens <- as.data.frame(read_delim(files$blackout_tokens, delim="|"))%>%
  select(date_rec, time_rec, username, clean_tweet)

# top 10 users, noticed steep drop off after 10
users_blackout_top5 <- users_blackout$username[1:10]

blackout_tweets <- bind_rows(blackout_tokens) %>%
  filter(username %in% users_blackout_top5)

ggplot(blackout_tweets, aes(time_rec, fill = username)) + 
  geom_histogram(position = "identity", bins = 100) +
  theme_minimal() +
  labs(title = "Distribution of Tweets of 10 Most Active Users in Blackout dataset", 
       y = "count of number of tweets", 
       x = "Time")

ggsave(files$blackout_timehist_plot, 
       plot = last_plot(), 
       device = NULL,
       path = NULL,
       scale = 1,
       width = NA,
       height = NA,
       units = "in",
       dpi = 300,
       limitsize = TRUE)

# plot bigrams

bigrams_blackout <- as.data.frame(read_delim(files$blackout_bigrams, delim="|"))

bigrams_blackout %>%
  ggplot(aes(reorder(bigram, n), n, bigram)) +
  geom_col() +
  xlab(NULL) +
  theme_minimal() +
  coord_flip() +
  labs(title = "Bigrams appearing in > 60 tweets in blackout dataset", 
       y = "Number of Occurances", 
       x = "Bigram")

ggsave(files$blackout_bigrams_plot, 
       plot = last_plot(), 
       device = NULL,
       path = NULL,
       scale = 1,
       width = NA,
       height = NA,
       units = "in",
       dpi = 300,
       limitsize = TRUE)

# plot freq first terms

f_blackout <- as.data.frame(read_delim(files$blackout_first, delim="|"))

f_blackout %>%
  ggplot(aes(reorder(first, n), n, first)) +
  geom_col() +
  xlab(NULL) +
  theme_minimal() +
  coord_flip() +
  labs(title = "First terms appearing in > 100 tweets in blackout dataset", 
       y = "Number of Occurances", 
       x = "Word")

ggsave(files$blackout_first_plot, 
       plot = last_plot(), 
       device = NULL,
       path = NULL,
       scale = 1,
       width = NA,
       height = NA,
       units = "in",
       dpi = 300,
       limitsize = TRUE)

# plot freq second terms

s_blackout <- as.data.frame(read_delim(files$blackout_second, delim="|"))

s_blackout %>%
  ggplot(aes(reorder(second, n), n, second)) +
  geom_col() +
  xlab(NULL) +
  theme_minimal() +
  coord_flip() +
  labs(title = "Second terms appearing in > 90 tweets in blackout dataset", 
       y = "Number of Occurances", 
       x = "Word")

ggsave(files$blackout_second_plot, 
       plot = last_plot(), 
       device = NULL,
       path = NULL,
       scale = 1,
       width = NA,
       height = NA,
       units = "in",
       dpi = 300,
       limitsize = TRUE)

# done
