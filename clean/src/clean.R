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
print(paste0("First cleaning for datasets have completed successfully."))

#start i loop
for (i in seq_along(cleanlist)) {
  
df <- as.data.frame(pluck(cleanlist, i))
  
write_delim(df, 
              quote = FALSE, 
              path = here(paste("analyze/input/",names(cleanlist)[i],"_clean1_df.csv",
                                sep = "")), delim = "|")
  
#message to let the user know that each iteration has completed
print(paste0("Export for dataset ",names(cleanlist)[i]," has completed successfully."))

# clean up for bigram analysis

# call lists of language-specific stop-words from stopwords package
# can add more languages as needed, these were all found in the data
# throughout the analysis process
iso_ro <- stopwords("ro", source = "stopwords-iso")
iso_ru <- stopwords("ru", source = "stopwords-iso")
iso_en <- stopwords("en", source = "stopwords-iso")
iso_sp <- stopwords("es", source = "stopwords-iso")
iso_pt <- stopwords("pt", source = "stopwords-iso")
iso_fr <- stopwords("fr", source = "stopwords-iso")
iso_de <- stopwords("de", source = "stopwords-iso")
iso_nl <- stopwords("nl", source = "stopwords-iso")

nltk_ro <- stopwords("ro", source = "nltk")
nltk_ru <- stopwords("ru", source = "nltk")
nltk_en <- stopwords("en", source = "nltk")
nltk_sp <- stopwords("es", source = "nltk")
nltk_pt <- stopwords("pt", source = "nltk")
nltk_fr <- stopwords("fr", source = "nltk")
nltk_de <- stopwords("de", source = "nltk")
nltk_nl <- stopwords("nl", source = "nltk")

snowball_ro <- stopwords("ro", source = "snowball")
snowball_ru <- stopwords("ru", source = "snowball")
snowball_en <- stopwords("en", source = "snowball")
snowball_sp <- stopwords("es", source = "snowball")
snowball_pt <- stopwords("pt", source = "snowball")
snowball_fr <- stopwords("fr", source = "snowball")
snowball_de <- stopwords("de", source = "snowball")
snowball_nl <- stopwords("nl", source = "snowball")

# words I identified from initial runs as not of interest, feel free to 
# delete them from the list to include them in the results again
specific_swords <- c("twitter", "twitter.com", "pic.twitter.com",
                     "minecraft", "gaming.youtube.com", "giveaway", 
                     "vgotrading", "tho", "it's", "i m", "https", "watchgamestv", 
                     "vgogiveaway", "youtu be", "ta", "#WeMissYouTaylor",
                     "#NYCPROTEST", "rt", "music", "iheartawards", 
                     "femaleartistoftheyear", "missamericana", "billy", "ray", 
                     "kanye", "taylor")
  
# remove useless bigrams
bad_bigrams <- c("cruel summer", "good music", "album sales", "singles sales", 
                 "music video", "track baby", "miss americana", "taylor swift", 
                 "taylorswift", "rt follow", "tweet follow", "subscribe keyword", 
                 "stupid love", "it s gonna", "enter follow", "www.youtube.com watch", 
                 "social media", "ich bin", "ist ein", "rtlike follow", 
                 "rtlike subscribe", "road didn't", "let's make", "makes good", 
                 "The Weeknd", "pic twitter", "premium generator", "hey guys", 
                 "steam wallet", "wallet gift", "gift card", "kinda stream",
                 "afternoon stream", "xbox doe", "cheap fut18", "fut18 coins", 
                 "500k fifa18", "stay tuned", "op instagram", "gaming youtube", 
                 "10x key", "livestream chat", "personal address", "address book")
  
  y_df <- df %>%
    unnest_tokens(bigram, tweet_txt, token = "ngrams", n = 2) %>%
    filter(!bigram %in% bad_bigrams) %>%
    separate(bigram, into = c("first","second"), sep = " ", remove = FALSE) %>%
    filter(str_detect(first, "[a-z]") & str_detect(second, "[a-z]")) %>%
    filter(!first %in% c(iso_ro, iso_ru, iso_en , iso_sp, iso_pt, iso_fr,
                           iso_de, nltk_ro, nltk_ru, nltk_en, nltk_sp, nltk_pt, 
                           nltk_fr, nltk_de, snowball_ro, snowball_ru, snowball_en,
                           snowball_sp, snowball_pt, snowball_fr, snowball_de, 
                         specific_swords), 
           !second %in% c(iso_ro, iso_ru, iso_en , iso_sp, iso_pt, iso_fr,
                            iso_de, nltk_ro, nltk_ru, nltk_en, nltk_sp, nltk_pt, 
                            nltk_fr, nltk_de, snowball_ro, snowball_ru, snowball_en,
                            snowball_sp, snowball_pt, snowball_fr, snowball_de, 
                          specific_swords)) %>%
    drop_na(bigram)
  
  y_df <- y_df  %>%
    verify(ncol(y_df) == 17) %>%
    write_delim(quote = FALSE, 
                path = here(paste("analyze/input/",names(cleanlist)[i],"_clean2_df.csv",
                                  sep = "")), 
                delim = "|")
  
#message to let the user know that each iteration has completed
print(paste0("Second cleaning for ",names(cleanlist)[i]," has completed successfully."))
  
} # end i loop

# done

