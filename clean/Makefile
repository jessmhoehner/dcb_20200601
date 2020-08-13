# -*- coding: utf-8 -*- 
#
#Author: JR
#Maintainer: JR
#License:  2020, EICC, GPL v3 or later
# 
# dcblackoutinvestigation_public/clean/Makefile
#----------------------------------------------------
#

HERE := $(shell Rscript --vanilla -e 'write(here::here(), stdout())')

clean_data = \
				$(HERE)/analyze/input/authors_clean_df.csv \
				$(HERE)/analyze/input/blackout_clean_df.csv \
				$(HERE)/analyze/input/authors_tokens_df.csv \
				$(HERE)/analyze/input/blackout_tokens_df.csv \

.PHONY:	all clean

all: $(clean_data)

clean:
		rm $(HERE)/analyze/input/*
		
$(clean_data): \
				src/clean.R \
				$(HERE)/clean/input/authors.csv \
				$(HERE)/clean/input/blackout.csv 
			Rscript --vanilla $<
	
# done.
