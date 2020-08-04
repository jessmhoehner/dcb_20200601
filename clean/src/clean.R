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
               "tidytext")

files <- list(
  auth = here::here("clean/input/authors.csv"),
  blackout = here::here("clean/input/blackout.csv"))

stopifnot(is_empty(files) != TRUE & length(files) == 2)
## Read in data, use a loop to accomodate new sheets

## creates a list of all files as connections
fileslist <- list(files$auth, files$blackout)

# will change as more sheets are added
stopifnot(length(fileslist) == 2)

# iterates over list of files, cleans the names of the columns, checks for numcol
cleanlist <- lapply(fileslist, function(x) {
  
  x_df <- tibble(read_csv(x, 
                          col_names = TRUE, 
                          col_types = 
                            cols(
                              cashtags = col_skip(), 
                              near = col_skip(),
                              geo = col_skip(), 
                              source = col_skip(), 
                              retweet_id = col_skip(), 
                              retweet_date = col_skip(), 
                              translate = col_skip(),
                              trans_src = col_skip(), 
                              trans_dest = col_skip(), 
                              conversation_id = col_skip()),
                          trim_ws = TRUE,
                          na = c("", "[]", "NA"), 
                          guess_max = 1500)) %>%
    clean_names()
  
  x_df  %>%
    verify(ncol(x_df) == 24)
  
})

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
  path = here(paste("analyze/input/",names(cleanlist)[i],"_clean_df.txt", sep = "")), 
  delim = "|")

  #message to let the user know that each iteration has completed
  print(paste0("Cleaning for dataset ",names(cleanlist)[i]," has completed successfully."))
  
} # close i loop

# done
