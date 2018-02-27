#Title: 'getting_the_data.R'
#Purpose: Get some public data for the R workshop, tidy it up and make it
#easy to understand
#Author: Charlotte Woolley
#Date: 27/02/2018

#Libraries needed:

        library(tidyverse)
        
#This data was downloaded from the UK data archive on 16/02/2018
#The data is from Study Number 8207 - Harmonised Height, Weight and BMI in Five 
#Longitudinal Cohort Studies: National Child Development Study, 
#1970 British Cohort Study and Millennium Cohort Study
        
#read in data

        dat <- read_tsv("~/Desktop/Data and spreadsheets/UKDA-8207-tab/tab/ncds_bcs70_mcs_closer_wp1.tab")

#This data contains repeated measures: take a cross sectional sample of the data when
#people are aged 30. Also rename some columns more intuitively and cut out columns we don't need

        dat2 <- dat %>%
                rename(ID = CLOSERID,
                       weight = wt,
                       height = ht) %>%
                mutate(sex = case_when(sex == '1' ~ 'Male', 
                                       sex == '2' ~ 'Female'),
                       weight_method = case_when(wtself == '1' ~ 'study_measured', 
                                                 wtself == '2' ~ 'self_measured'),
                       height_method = case_when(htself == '1' ~ 'study_measured', 
                                                 htself == '2' ~ 'self_measured')) %>%
                filter(visitage == 30 & !is.na(weight) & !is.na(height)) %>%
                select(ID, sex, weight, weight_method, height, height_method, bmi)


        write_csv(dat2, "data_R_workshop_27_02_2018.csv")        
        