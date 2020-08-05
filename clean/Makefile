# -*- coding: utf-8 -*- 
#
#Author: JR
#Maintainer: JR
#License:  2019, EICC, GPL v3 or later
# 
# dcblackoutinvestigation_public/clean/src/clean.R
#----------------------------------------------------
#

HERE := $(shell Rscript --vanilla -e 'write(here::here(), stdout())')

clean_data = \
				$(HERE)/analyze/input/authors_clean1_df.csv \
				$(HERE)/analyze/input/blackout_clean1_df.csv \
				$(HERE)/analyze/input/authors_clean2_df.csv \
				$(HERE)/analyze/input/blackout_clean2_df.csv \

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
