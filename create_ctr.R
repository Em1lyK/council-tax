#emily keenan
#11/05/2023
#building 2023-24 ctr for 
#this is me making changes

# IMPORTING LIBRARIES #

library(tidyverse)
library("readODS")
library(janitor)

# reading in relevant data sets

setwd("D:\\Users\\emily.keenan\\OneDrive - MHCLG\\Documents\\GitHub\\council-tax")

# specific sheets: billing and precepting ctr data
cts_loc <- "input/CTR_Table_9_2023-24 (10).ods"

ctr_billing_raw <-read_ods(cts_loc, sheet = "Data_Billing", skip = 4)

ctr_precepting_raw <- read_ods(cts_loc, sheet = "Data_Precepting", skip = 5)


##Isolate relevant rows 
#tidy column names 

ctr_billing_raw <- ctr_billing_raw %>% 
  clean_names()

ctr_precepting_raw <- ctr_precepting_raw %>% 
  clean_names()

#billing authorities: rename authority, tstb and band d columns
#used column numbers because the names are horrifying

ctr_billing <- ctr_billing_raw %>%
  rename(ecode = e_code,
         onscode = ons_code,
         tstb2223 = x7_council_tax_base_for_council_tax_setting_purposes_previous_year, 
         tstb2324 = x7_council_tax_base_for_council_tax_setting_purposes_current_year,
         avebandd_expp2223 = x9_average_band_d_2_adult_equivalent_council_tax_including_adult_social_care_precept_and_excluding_local_precepts_previous_year, 
         avebandd_expp2324 = x9_average_band_d_2_adult_equivalent_council_tax_including_adult_social_care_precept_and_excluding_local_precepts_current_year
  )

#isolate relevant columns

ctr_billing <- ctr_billing %>% 
  select(ecode:class, tstb2223:tstb2324, avebandd_expp2223:avebandd_expp2324)



#precepting authorities: rename authority, tstb and band d columns

ctr_precepting <- ctr_precepting_raw %>% 
  rename(ecode = e_code,
         onscode = ons_code,
         tstb2223 = x2_council_tax_base_for_the_major_precepting_authoritys_area_for_precept_purposes_after_council_tax_reduction_scheme_to_1_decimal_place_previous_year, 
         tstb2324 = x2_council_tax_base_for_the_major_precepting_authoritys_area_for_precept_purposes_after_council_tax_reduction_scheme_to_1_decimal_place_current_year,
         avebandd_expp2223 = x3_average_band_d_2_adult_equivalent_council_tax_of_major_precepting_authority_line_1_line_2_previous_year, 
         avebandd_expp2324 = x3_average_band_d_2_adult_equivalent_council_tax_of_major_precepting_authority_line_1_line_2_current_year
  )

#isolate relevant columns

ctr_precepting <- ctr_precepting %>% 
  select(ecode:class, tstb2223:tstb2324, avebandd_expp2223:avebandd_expp2324)

##combing the billing and precepting data 
ctr <- rbind(ctr_billing, ctr_precepting)

ctr <- ctr[c(1:6, 303:316, 7:302, 317:408),]

# cleaning cols by replacing non numeric data points
ctr <- ctr %>% 
  mutate(tstb2223 = ifelse(ctr$tstb2223 == "[z]", 0, tstb2223)) %>% 
  mutate(tstb2324 = ifelse(ctr$tstb2324 == "[z]", 0, tstb2324)) %>% 
  mutate(avebandd_expp2223 = ifelse(ctr$avebandd_expp2223 == "[z]", 0, avebandd_expp2223)) %>% 
  mutate(avebandd_expp2324 = ifelse(ctr$avebandd_expp2324 == "[z]", 0, avebandd_expp2324)) %>% 
  mutate(avebandd_expp2223 = ifelse(ctr$avebandd_expp2223 == "[x]", 0, avebandd_expp2223))

ctr <- ctr %>% 
  mutate(tstb2223 = as.numeric(tstb2223)) %>% 
  mutate(tstb2324 = as.numeric(tstb2324)) %>% 
  mutate(avebandd_expp2223 = as.numeric(avebandd_expp2223)) %>%
  mutate(avebandd_expp2324 = as.numeric(avebandd_expp2324))
#write.csv(ctr, "D:\\Users\\emily.keenan\\Documents\\GitHub\\council-tax\\bandd_ctr.csv")




