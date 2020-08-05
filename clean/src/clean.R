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
               "tidytext", "lubridate", "hms")

files <- list(
  auth = here("clean/input/authors.csv"),
  blackout = here("clean/input/blackout.csv"))

stopifnot(is_empty(files) != TRUE & length(files) == 2)
## Read in data, use a loop to accomodate new sheets

# will change as more sheets are added
stopifnot(length(files) == 2)

# add unique names to each df for easy export later on

df_names <- c("authors", "blackout")

names(files) <- df_names

# iterates over list of files, cleans the names of the columns
# keeps only columns of interest
# checks for numcol
# remove variables with no info in columns
# clean up tweets (source ref: ( Hicks , 2014) and ref: (Stanton 2013))

cleanlist <- lapply(files, function(x) {
  
# clean up for username by time and date analysis
  
# filter out taylor swift related usernames 
tay_names <- c("taylorswift13", "taylorvotestats", 
                 "tayiorvotestats")

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
})

#message to update user
print(paste0("Cleaning for datasets have completed successfully."))

#start i loop
for (i in seq_along(cleanlist)) {
  
df <- as.data.frame(pluck(cleanlist, i))
  
write_delim(df, 
              quote = FALSE, 
              path = here(paste("analyze/input/",names(cleanlist)[i],"_clean_df.csv",
                                sep = "")), delim = "|")
  
#message to let the user know that each iteration has completed
print(paste0("Export for dataset ",names(cleanlist)[i]," has completed successfully."))

# clean up for bigram analysis
"%nin%" <- Negate("%in%")

# call lists of language-specific stop-words from stopwords package
# can add more languages as needed, these were all found in the data
# throughout the analysis process

# words I identified from initial runs as not of interest, feel free to 
# delete them from the list to include them in the results again
# there's some strnge apostrophe's keeping in otherwise removed stopwords
specific_swords <- as.character(c("twitter", "twitter.com", "pic.twitter.com",
                     "minecraft", "gaming.youtube.com", "giveaway", 
                     "vgotrading", "tho", "it's", "i'm", "https", "watchgamestv", 
                     "vgogiveaway", "youtu.be", "ta", "#WeMissYouTaylor",
                     "#NYCPROTEST", "rt", "music", "iheartawards", 
                     "femaleartistoftheyear", "missamericana", "billy", "ray", 
                     "kanye", "taylor","y'alll", "you're", "youtube", "youtu", 
                     "there's", "we'll", "ur", "we're", "yup", "yous", "youtubers", 
                     "yep", "yeah", "yea", "youtuber", "tweet", "tweeted", "they're", 
                     "i'm", "	don't", "i'm", "y'all", "don't", "it's", "won't","they're", 
                     "don't", "didn't", "doesn't", "can't", "isn't", "wouldn't", "swift", 
                     "guys", "hey", "gratis", "minecraftpremiumaccount", "user", 
                     "account", "minecraftpremiumgenerator2015", "rockstarsupport", 
                     "subscribe", "stream", "follow", "ain't"))

  y_df <- df %>%
    unnest_tokens(word, tweet_txt) %>%
    mutate(word = as.character(str_to_lower(word))) %>%
    anti_join(get_stopwords("ro", source = "stopwords-iso")) %>%
    anti_join(get_stopwords("ru", source = "stopwords-iso")) %>%
    anti_join(get_stopwords("en", source = "stopwords-iso"))  %>% 
    anti_join(get_stopwords("es", source = "stopwords-iso"))  %>%
    anti_join(get_stopwords("pt", source = "stopwords-iso"))  %>%
    anti_join(get_stopwords("fr", source = "stopwords-iso"))  %>%
    anti_join(get_stopwords("de", source = "stopwords-iso"))  %>%
    anti_join(get_stopwords("nl", source = "stopwords-iso"))  %>%
    anti_join(get_stopwords("pl", source = "stopwords-iso"))  %>%
    anti_join(get_stopwords("ro", source = "nltk"))  %>%
    anti_join(get_stopwords("ru", source = "nltk"))  %>%
    anti_join(get_stopwords("en", source = "nltk"))  %>%
    anti_join(get_stopwords("es", source = "nltk"))  %>%
    anti_join(get_stopwords("pt", source = "nltk"))  %>%
    anti_join(get_stopwords("fr", source = "nltk"))  %>%
    anti_join(get_stopwords("de", source = "nltk"))  %>%
    anti_join(get_stopwords("nl", source = "nltk"))  %>%
    anti_join(get_stopwords("ro", source = "snowball"))  %>%
    anti_join(get_stopwords("ru", source = "snowball"))  %>%
    anti_join(get_stopwords("en", source = "snowball"))  %>%
    anti_join(get_stopwords("es", source = "snowball"))  %>%
    anti_join(get_stopwords("pt", source = "snowball"))  %>%
    anti_join(get_stopwords("fr", source = "snowball"))  %>%
    anti_join(get_stopwords("de", source = "snowball"))  %>%
    anti_join(get_stopwords("nl", source = "snowball"))  %>%
    anti_join(get_stopwords("ja", source = "stopwords-iso"))  %>%
    anti_join(get_stopwords("ja", source = "marimo"))  %>%
    mutate(word = as.character(word)) %>%
    filter(!word %in% specific_swords) %>%
    drop_na(word)
  
  y_df <- y_df  %>%
    verify(ncol(y_df) == 15) %>%
    write_delim(quote = FALSE, 
                path = here(paste("analyze/input/",names(cleanlist)[i],"_tokens_df.csv",
                                  sep = "")), 
                delim = "|")
  
#message to let the user know that each iteration has completed
print(paste0("Tokens created for ",names(cleanlist)[i],"."))
  
} # end i loop

# done

