#!/usr/bin/env Rscript --vanilla
# set expandtab ft=R ts=4 sw=4 ai fileencoding=utf-8
#
# Author: JR
# Maintainer(s): JR
# License: GPL V.3.0
#
# -----------------------------------------------------------
# dcblackoutinvestigation_public/clean/src/clean.R

# import the data into R and send to clean task

pacman::p_load("tidyverse", "here", "assertr", "janitor", 
               "tidytext", "lubridate", "hms", "stopwords")

files <- list(
  auth = here::here("clean/input/authors.csv"),
  blackout = here::here("clean/input/blackout.csv"))

stopifnot(is_empty(files) != TRUE & length(files) == 2)
## Read in data, use a loop to accomodate new sheets

## creates a list of all files as connections
fileslist <- list(files$auth, files$blackout)

# will change as more sheets are added
stopifnot(length(fileslist) == 2)

# iterates over list of files, cleans the names of the columns
# keeps only columns of interest
# checks for numcol

cleanlist <- lapply(fileslist, function(x) {

# clean up for username by time and date analysis
  
# filter out taylor swift related usernames 
tay_names <- c("taylorswift13", "taylorvotestats", 
                 "tayiorvotestats")

# clean up for bigram analysis

# call lists of language-specific stop-words from stopwords package
# can add more languages as needed
iso_ro <- stopwords("ro", source = "stopwords-iso")
iso_ru <- stopwords("ru", source = "stopwords-iso")
iso_en <- stopwords("en", source = "stopwords-iso")
iso_sp <- stopwords("es", source = "stopwords-iso")
iso_pt <- stopwords("pt", source = "stopwords-iso")
iso_fr <- stopwords("fr", source = "stopwords-iso")
iso_de <- stopwords("de", source = "stopwords-iso")

nltk_ro <- stopwords("ro", source = "nltk")
nltk_ru <- stopwords("ru", source = "nltk")
nltk_en <- stopwords("en", source = "nltk")
nltk_sp <- stopwords("es", source = "nltk")
nltk_pt <- stopwords("pt", source = "nltk")
nltk_fr <- stopwords("fr", source = "nltk")
nltk_de <- stopwords("de", source = "nltk")

snowball_ro <- stopwords("ro", source = "snowball")
snowball_ru <- stopwords("ru", source = "snowball")
snowball_en <- stopwords("en", source = "snowball")
snowball_sp <- stopwords("es", source = "snowball")
snowball_pt <- stopwords("pt", source = "snowball")
snowball_fr <- stopwords("fr", source = "snowball")
snowball_de <- stopwords("de", source = "snowball")

# words I identified from initial runs as not of interest, feel free to 
# delete them from the list to include them in the results again
specific_swords <- c("twitter", "twitter.com", "pic.twitter.com",
                     "minecraft", "gaming.youtube.com", "giveaway", 
                     "vgotrading", "tho", "it's", "i'm", "https", "watchgamestv", 
                     "vgogiveaway", "youtu.be", "ta", "#WeMissYouTaylor",
                     "#NYCPROTEST")


x_df <- as.data.frame(read_csv(x, col_names = TRUE)) %>%
  clean_names() %>%
  select(c("id", "date", "time", "timezone", "username", "name", 
          "tweet", "retweets_count", "likes_count", "hashtags", 
          "link", "retweet", "reply_to")) %>%
  mutate(tweet = as.character(str_to_lower(tweet)), 
         tweet = gsub('[[:punct:] ]+',' ', tweet),
         name = gsub('[[:punct:] ]+',' ', name),
         date_rec = ymd(date), 
         time_rec = as_hms(time)) %>%
  rename(tweet_txt = tweet) %>%
  filter(!username %in% tay_names, 
         is.double(id) == TRUE, 
         is.character(tweet_txt) == TRUE, 
         is.Date(date_rec) == TRUE, 
         is.difftime(time_rec))

  x_df <- x_df  %>%
    verify(ncol(x_df) == 15)
  
})

# error parsing cashapp tags but we won't be using them anyways

stopifnot(length(cleanlist) == 2)

# add unique names to each df for easy export later on

df_names <- c("authors", "blackout")

names(cleanlist) <- df_names

## using cleanlist, we extract each df and save and export result 
## to the analysis task

#start i loop
for (i in seq_along(cleanlist)) {
  
  df <- as.data.frame(pluck(cleanlist, i))
  
  write_delim(df, 
  quote = FALSE, 
  path = here(paste("analyze/input/",names(cleanlist)[i],"_clean_df.csv",
                    sep = "")), 
  delim = "|")

  #message to let the user know that each iteration has completed
  print(paste0("Cleaning for dataset ",names(cleanlist)[i]," has completed successfully."))
  
} # close i loop

# done

