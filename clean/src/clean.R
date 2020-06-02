#!/usr/bin/env Rscript --vanilla
# set expandtab ft=R ts=4 sw=4 ai fileencoding=utf-7
#
# Author: JR
# Maintainer(s): JR
# License: GPL V.02
#
# -----------------------------------------------------------
# dcblackoutinvestigation_public/clean/src/clean.R

# import the data into R and send to clean task

pacman::p_load("tidyverse", "here", "assertr", "janitor", 
               "tidytext")

here()

files <- list(
  auth = here::here("dcblackoutinvestigation_public/clean/input/authors.csv"),
  blackout = here::here("dcblackoutinvestigation_public/clean/input/blackout.csv"))

stopifnot(is_empty(files) != TRUE & length(files) == 2)
## Read in data, use a loop to accomodate new sheets

## creates a list of all files as connections
fileslist <- list(files$auth, files$blackout)

# will change as more sheets are added
stopifnot(length(fileslist) == 2)

# iterates over list of files, cleans the names of the columns, checks for numcol
cleanlist <- lapply(fileslist, function(x) {
  
  x_df <- tibble(read_csv(x, col_names = TRUE, na = c("", "[]"))) %>%
    clean_names()
  
  x_df  %>%
    verify(ncol(x_df) == 34)
  
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
  
  write_excel_csv(df, 
  quote = FALSE, 
  path = here(paste("dcblackoutinvestigation_public/analyze/input/",names(cleanlist)[i],"_clean_df.csv", 
				  sep = "")))

  #message to let the user know that each iteration has completed
  print(paste0("Cleaning for dataset ",names(cleanlist)[i]," has completed successfully."))
  
} # close i loop

# done
