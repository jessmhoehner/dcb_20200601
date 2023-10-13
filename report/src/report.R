#!/usr/bin/env Rscript --vanilla
# set expandtab ft=R ts=4 sw=4 ai fileencoding=utf-8
#
# Author: JR
# Maintainer(s): JR
# License: GPL V.3.0
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
  blackout_tokens = 
    here("analyze/input/blackout_tokens_df.csv"),
  
  auth_users_plot = 
    here("report/output/authors_frequsers.png"),
  auth_daybar_plot = 
    here("report/output/authors_daybar.png"),
  auth_timehist_plot = 
    here("report/output/authors_timehist.png"), 
  auth_word_plot = 
    here("report/output/authors_freqbigrams.png"),
  
  blackout_users_plot = 
    here("report/output/blackout_frequsers.png"), 
  blackout_timehist_plot = 
    here("report/output/blackout_timehist.png"), 
  blackout_word_plot = 
    here("report/output/blackout_freqbigrams.png")
  
)

stopifnot(is_empty(files) != TRUE & length(files) == 11)

# authors.csv 

users_auth <- as.data.frame(read_delim(files$auth_users, delim="|"))

# numbers of tweets over time

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
                                                     decreasing = TRUE)))) %>%
  head(10)

(ut_plot  <- u_tweets %>%
  filter(n_tweets > 20) %>%
  ggplot(aes(reorder(username, n_tweets), n_tweets, username)) +
  geom_col() +
  coord_flip() +
  labs(title = "10 users with with > 20 tweets in authors dataset", 
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
  scale_fill_viridis_d("Username:") +
  scale_x_date(date_breaks = "1 week", date_labels = "%m-%d") +
  scale_y_continuous(breaks = c(0, 100, 200, 300, 400, 500, 600, 700, 800, 
                                900, 1000, 1100, 1200, 1300, 1400, 1500)) +
  theme_classic() +
 # theme(axis.text.x=element_text(angle=90, hjust=1)) +
  labs(title = "Number of Tweets Per Day by 10 Most Active Usernames",
       subtitle = glue::glue("Note the surge in activity around the campaign and consistent activity 
                   even months prior from the most active account"),
       y = NULL,
       x = NULL) +
  theme(
    legend.position = "top",
    panel.spacing = unit(.5, "picas"),
    axis.text.x = element_text(size = 8))

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
  scale_fill_viridis_d() +
  scale_color_viridis_d() +
  scale_x_datetime(breaks = "3 hours", labels = waiver()) +
  theme_classic() +
  theme(axis.text.x=element_text(angle=90, hjust=1)) +
  ylim(0, 50) +
  labs(title = "Tweets Over Time by 3 Most Active Usernames", 
       y = NULL, 
       x = NULL) +
  theme(
    legend.position = "top",
    panel.spacing = unit(.5, "picas"),
    axis.text.x = element_text(size = 8))

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

# numbers of tweets over time

u_df2 <- as.data.frame(users_blackout %>%
                        select(-c(time_rec)) %>%
                        gather(key = "username", value = "n")%>%
                        pivot_wider(names_from = "username", 
                                    values_from = "n", values_fn = sum) %>%
                        t())

# need to to this seperately because of row.names 
u_tweets2 <- u_df2 %>%
  mutate(username = row.names(u_df2), 
         n_tweets = V1, 
         name = factor(username, levels = names(sort(table(username), 
                                                     decreasing = TRUE))))

(ut_plot2  <- u_tweets2 %>%
    filter(n_tweets > 100) %>%
    ggplot(aes(reorder(name, n_tweets), n_tweets, name)) +
    geom_col() +
    coord_flip() +
    labs(title = "Usernames with > 100 tweets in blackout dataset", 
         y = "count of number of tweets", 
         x = "number of tweets"))

# ggsave(files$blackout_users_plot, 
#        plot = last_plot(), 
#        device = NULL,
#        path = NULL,
#        scale = 1,
#        width = NA,
#        height = NA,
#        units = "in",
#        dpi = 300,
#        limitsize = TRUE)

# how long had these most active accounts been active?
# has their activity changed over time?
# tweets by day, colored by most active users over time

u_active2 <- u_tweets2 %>%
 filter(n_tweets > 200) %>%
 select(name)

names(users_blackout$time_rec) <- users_blackout$time_rec

users_blackout %>%
 filter(username %in% u_active2$name) %>%
 mutate(time_rec = hms::as_hms(time_rec)) %>%
 ggplot(aes(time_rec, n, fill = username)) +
 geom_col() +
 scale_x_time(name = waiver(),
              breaks = "1 hour",
              labels = waiver()) +
 theme_minimal() +
 theme(axis.text.x=element_text(angle=90, hjust=1)) +
 labs(title = "Number of Tweets Over Time by 11 Most Active Usernames",
      y = "Number of tweets",
      x = "Time")

#ggsave(files$auth_daybar_plot,
#       plot = last_plot(),
#       device = NULL,
#       path = NULL,
#       scale = 1,
#       width = NA,
#      height = NA,
#       dpi = 300,
#       limitsize = TRUE)


blackout_tokens <- as.data.frame(read_delim(files$blackout_freq, delim="|")) %>%
  select(date_rec, time_rec, username, clean_tweet)

#top 10 users, noticed steep drop off after 10
users_blackout_top5 <- users_blackout$username[1:10]

blackout_tweets <- bind_rows(blackout_tokens) %>%
  filter(username %in% users_blackout_top5)

blackout_tweets %>%
  ggplot(aes(time_rec, fill = username)) +
  geom_histogram(position = "identity", bins = 100) +
  theme_classic() +
  scale_fill_viridis_d() +
  labs(title = "Distribution of Tweets of 10 Most Active Users in Blackout dataset",
       y = NULL,
       x = NULL) +
  theme(legend.position = "none",
         panel.spacing = unit(.5, "picas"),
         axis.text.x = element_text(size = 8))

#ggsave(files$blackout_timehist_plot,
#       plot = last_plot(),
#       device = NULL,
#       path = NULL,
#       scale = 1,
#       width = NA,
#       height = NA,
#       units = "in",
#       dpi = 300,
#       limitsize = TRUE)

# plot bigrams

terms_blkout <- as.data.frame(read_delim(files$blackout_freq, delim="|")) %>%
  arrange(n)

t_df2 <- as.data.frame(terms_blkout %>%
                        select(-c(time_rec, username)) %>%
                        gather(key = "word", value = "n")%>%
                        pivot_wider(names_from = "word", values_from = "n", values_fn = sum) %>%
                        t())

# need to to this seperately because of row.names 
t_df2 <- t_df2 %>%
  mutate(word = row.names(t_df2), 
         n_tweets = V1, 
         word= factor(word, levels = names(sort(table(word), 
                                                     decreasing = TRUE))))
t_df2 %>%
  filter(n > 4) %>%
  ggplot(aes(reorder(word, n_tweets), n_tweets, word)) +
  geom_col() +
  xlab(NULL) +
  theme_minimal() +
  coord_flip() +
  labs(title = "Words appearing in > 4 tweets in blackout dataset", 
       y = "Number of Occurances", 
       x = "Word")

ggsave(files$blackout_word_plot, 
       plot = last_plot(), 
       device = NULL,
       path = NULL,
       scale = 1,
       width = NA,
       height = NA,
       units = "in",
       dpi = 300,
       limitsize = TRUE)

# stopped here for the day

# done
