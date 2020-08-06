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

pacman::p_load("tidyverse", "here", "assertr", "lubridate")

# import data
files <- list(
  auth_users = 
    here("report/input/authors_frequsers.csv"), 
  auth_freq = 
    here("report/input/authors_freqterms.csv"),
  
  blackout_users = 
    here("report/input/blackout_frequsers.csv"), 
  blackout_freq = 
    here("report/input/blackout_freqterms.csv"),
  
  auth_users_plot = 
    here("report/output/authors_frequsers.png"),
  auth_daybar_plot = 
    here("report/output/authors_daybar.png"),
  auth_timehist_plot = 
    here("report/output/authors_timehist.png"), 
  auth_bigrams_plot = 
    here("report/output/authors_freqbigrams.png"),
  
  blackout_users_plot = 
    here("report/output/blackout_frequsers.png"), 
  blackout_timehist_plot = 
    here("report/output/blackout_timehist.png"), 
  blackout_bigrams_plot = 
    here("report/output/blackout_freqbigrams.png"), 
  blackout_first_plot = 
    here("report/output/blackout_freqfirst.png"), 
  blackout_second_plot = 
    here("report/output/blackout_freqsecond.png") 
  
)

stopifnot(is_empty(files) != TRUE & length(files) == 12)

# authors.csv 

# read in data 

users_auth <- as.data.frame(read_delim(files$auth_users, delim="|"))

# histogram of numbers of tweets

u_df <- as.data.frame(users_auth %>%
  select(-c(date_rec, time_rec)) %>%
  gather(key = "username", value = "n")%>%
  pivot_wider(names_from = "username", values_from = "n", values_fn = sum) %>%
  t())

# need to to this seperately because of row.names 
u_tweets <- u_df %>%
  mutate(username = row.names(u_df), 
         n_tweets = V1, 
         name = factor(username, levels = names(sort(table(username), 
                                                     decreasing = TRUE))))

(ut_plot  <- u_tweets %>%
  filter(n_tweets > 20) %>%
  ggplot(aes(reorder(username, n_tweets), n_tweets, username)) +
  geom_col() +
  coord_flip() +
  labs(title = "Usernames with > 20 tweets in authors dataset", 
       y = "count of number of tweets", 
       x = "number of tweets"))

ggsave(files$auth_users_plot, 
       plot = last_plot(),
       units = "in",
       dpi = 300,
       limitsize = TRUE)

# how long had these most active accounts been active?
# has their activity changed over time?
# tweets by day, colored by 10 most active users over time

u_active <- u_tweets %>%
  filter(n_tweets > 55) %>%
  select(username)

users_auth %>%
  filter(date_rec > "2020-03-20" ) %>%
  filter(username %in% u_active$username) %>%
  ggplot(aes(date_rec, n, fill = username)) +
  geom_col() +
  scale_x_date(date_breaks = "1 day", date_labels = "%m-%d") +
  scale_y_continuous(breaks = c(0, 100, 200, 300, 400, 500, 600, 700, 800, 
                                900, 1000, 1100, 1200, 1300, 1400, 1500)) +
  theme_minimal() +
  theme(axis.text.x=element_text(angle=90, hjust=1)) +
  labs(title = "Number of Tweets Over Time by 10 Most Active Usernames", 
       y = "Number of tweets", 
       x = "Date")

ggsave(files$auth_daybar_plot, 
       plot = last_plot(), 
       device = NULL,
       path = NULL,
       scale = 1,
       width = NA,
       height = NA,
       dpi = 300,
       limitsize = TRUE)
  
# in fact, rather than just being active during this campaign, two of these 
# usernames have been active since March and May 2020 with hundreds of tweets per day


# for each dataset, when was each of the most frequent usernames most active
# during the speciifc campaign?
# did they post all at once or over the course of a day?

users_auth %>%
  filter(date_rec == "2020-05-31" | date_rec == "2020-06-01") %>%
  filter(username %in% u_active$username) %>%
  unite("datetime", date_rec:time_rec, sep = ":") %>%
  mutate(datetime = ymd_hms(datetime)) %>%
  ggplot(aes(datetime, n, color = username, fill = username)) +
  geom_col() +
  scale_x_datetime(breaks = "1 hour", labels = waiver()) +
  theme_minimal() +
  theme(axis.text.x=element_text(angle=90, hjust=1)) +
  ylim(0, 50) +
  labs(title = "Number of Tweets Over Time by 10 Most Active Usernames", 
       y = "Number of tweets", 
       x = "Time")

ggsave(files$auth_timehist_plot, 
       plot = last_plot(), 
       device = NULL,
       path = NULL,
       scale = 1,
       width = NA,
       height = NA,
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
