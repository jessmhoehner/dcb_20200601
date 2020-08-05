# -*- coding: utf-8 -*- 
#
#Author: JR
#Maintainer: JR
#License:  2020, EICC, GPL v3 or later
# 
# dcblackoutinvestigation_public/analyze/src/analyze.R
#----------------------------------------------------
#

HERE := $(shell Rscript --vanilla -e 'write(here::here(), stdout())')

report_data = \
				$(HERE)/report/input/authors_frequsers.csv \
				$(HERE)/report/input/authors_freqterms.csv \
				$(HERE)/report/input/blackout_frequsers.csv \
				$(HERE)/report/input/blackout_freqterms.csv \
				
		
.PHONY:	all clean

all: $(report_data)

report:
		rm $(HERE)/report/input/*
		
$(report_data): \
				src/analyze.R \
				$(HERE)/analyze/input/authors_tokens_df.csv \
				$(HERE)/analyze/input/blackout_tokens_df.csv 
			Rscript --vanilla $<
	
# done.
