# -*- coding: utf-8 -*- 
#
#Author: JR
#Maintainer: JR
#License:  2019, EICC, GPL v3 or later
# 
# dcblackoutinvestigation_public/analyze/src/analyze.R
#----------------------------------------------------
#

HERE := $(shell Rscript --vanilla -e 'write(here::here(), stdout())')

report_data = \
				$(HERE)/report/input/auth_tokens.csv \
				$(HERE)/report/input/authors_frequsers.csv \
				$(HERE)/report/input/authors_freqbigrams.csv \
				$(HERE)/report/input/blackout_tokens.csv.csv \
				$(HERE)/report/input/blackout_frequsers.csv \
				$(HERE)/report/input/blackout_freqbigrams.csv \
				$(HERE)/report/input/blackout_freqfirst.csv \
				$(HERE)/report/input/blackout_freqsecond.csv \
				
				

.PHONY:	all clean

all: $(report_data)

report:
		rm $(HERE)/report/input/*
		
$(report_data): \
				src/analyze.R \
				$(HERE)/analyze/input/authors_clean_df.csv \
				$(HERE)/analyze/input/blackout_clean_df.csv 
			Rscript --vanilla $<
	
# done.
