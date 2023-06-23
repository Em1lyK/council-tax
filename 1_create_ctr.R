#emily keenan
#11/05/2023
#building 2023-24 ctr for 
#this is me making changes

# IMPORTING LIBRARIES #

library(tidyverse)
library("readODS")
library(janitor)
library(plyr)
library(readxl)

# reading in relevant data sets
setwd("D:\\Users\\emily.keenan\\OneDrive - MHCLG\\Documents\\GitHub\\council-tax")

restructure_2324 <- function(df, summarise_numerics = TRUE) {

  # Ref data required by function. Should prob lump this outside {}

  terminated_orgs <-
  read_excel("input/restructuring 2013 to 2023.xlsx",
  sheet = "ONS code lookup 2013 to 2023") %>% 
  clean_names() %>%
  select(status, contains("ons_code_")) %>%
  filter(status == "terminated")

  geogs_2023 <- terminated_orgs %>%
  pivot_longer(-c(ons_code_2023, status), values_to = "old_geog") %>%
  select(-name, -status) %>%
  unique() # Gardening 

  # The function will output the rebased and collapsed 2324 LAs

  if (summarise_numerics == TRUE) {
  output <- df %>%
  left_join(geogs_2023, by = c("ons_code" = "old_geog")) %>%
  mutate(ons_code = ifelse(!is.na(ons_code_2023), ons_code_2023, ons_code)) %>%
  select(-ons_code_2023) %>%
  group_by(ons_code) %>%
  dplyr::summarise(across(where(is.numeric), sum)) %>%
  ungroup()  
  } else { # If you do not want to summarise across numerics (e.g. not wanting to add up new vars)
  output <<- df %>%
  left_join(geogs_2023, by = c("ons_code" = "old_geog")) %>%
  mutate(ons_code = ifelse(!is.na(ons_code_2023), ons_code_2023, ons_code)) %>%
  select(-ons_code_2023) %>%
  ungroup() 
  }

  return(output)
}


# specific sheets: billing and precepting ctr data
cts_loc <- "input/CTR_Table_9_2023-24 (10).ods"
#location of ctb file
ctb_input <- c('input\\Council_Taxbase_local_authority_level_data_2022 (4).ods')

ctr_billing_raw <-read_ods(cts_loc, sheet = "Data_Billing", skip = 4)

ctr_precepting_raw <- read_ods(cts_loc, sheet = "Data_Precepting", skip = 5)


##Isolate relevant rows 
#tidy column names 

ctr_billing <- ctr_billing_raw %>% 
  clean_names()

ctr_precepting <- ctr_precepting_raw %>% 
  clean_names()

#billing authorities: rename authority, tstb and band d columns
#used column numbers because the names are horrifying

ctr_billing <- ctr_billing %>%
  dplyr::rename(ecode = e_code,
         onscode = ons_code,
         tstb_2223 = x7_council_tax_base_for_council_tax_setting_purposes_previous_year, 
         tstb_2324 = x7_council_tax_base_for_council_tax_setting_purposes_current_year,
         avebandd_expp2223 = x9_average_band_d_2_adult_equivalent_council_tax_including_adult_social_care_precept_and_excluding_local_precepts_previous_year, 
         avebandd_expp2324 = x9_average_band_d_2_adult_equivalent_council_tax_including_adult_social_care_precept_and_excluding_local_precepts_current_year
  )


#isolate relevant columns

ctr_billing <- ctr_billing %>% 
  select(ecode:class, tstb_2223:tstb_2324, avebandd_expp2223:avebandd_expp2324)



#precepting authorities: rename authority, tstb and band d columns

ctr_precepting <- ctr_precepting %>% 
  dplyr::rename(ecode = e_code,
         onscode = ons_code,
         tstb_2223 = x2_council_tax_base_for_the_major_precepting_authoritys_area_for_precept_purposes_after_council_tax_reduction_scheme_to_1_decimal_place_previous_year, 
         tstb_2324 = x2_council_tax_base_for_the_major_precepting_authoritys_area_for_precept_purposes_after_council_tax_reduction_scheme_to_1_decimal_place_current_year,
         avebandd_expp2223 = x3_average_band_d_2_adult_equivalent_council_tax_of_major_precepting_authority_line_1_line_2_previous_year, 
         avebandd_expp2324 = x3_average_band_d_2_adult_equivalent_council_tax_of_major_precepting_authority_line_1_line_2_current_year
  )

#isolate relevant columns

ctr_precepting <- ctr_precepting %>% 
  select(ecode:class, tstb_2223:tstb_2324, avebandd_expp2223:avebandd_expp2324)

##combing the billing and precepting data 
ctr <- rbind(ctr_billing, ctr_precepting)

ctr <- ctr[c(1:6, 303:316, 7:302, 317:408),]

# cleaning cols by replacing non numeric data points
ctr <- ctr %>% 
  mutate(tstb_2223 = ifelse(ctr$tstb_2223 == "[z]", 0, tstb_2223)) %>% 
  mutate(tstb_2324 = ifelse(ctr$tstb_2324 == "[z]", 0, tstb_2324)) %>% 
  mutate(avebandd_expp2223 = ifelse(ctr$avebandd_expp2223 == "[z]", 0, avebandd_expp2223)) %>% 
  mutate(avebandd_expp2324 = ifelse(ctr$avebandd_expp2324 == "[z]", 0, avebandd_expp2324)) %>% 
  mutate(avebandd_expp2223 = ifelse(ctr$avebandd_expp2223 == "[x]", 0, avebandd_expp2223))

ctr <- ctr %>% 
  mutate(tstb_2223 = as.numeric(tstb_2223)) %>% 
  mutate(tstb_2324 = as.numeric(tstb_2324)) %>% 
  mutate(avebandd_expp2223 = as.numeric(avebandd_expp2223)) %>%
  mutate(avebandd_expp2324 = as.numeric(avebandd_expp2324))
write.csv(ctr, "D:\\Users\\emily.keenan\\OneDrive - MHCLG\\Documents\\GitHub\\council-tax\\output\\bandd_ctr.csv")

################################################################################
######################read in ctb######################
#################################################################################
ctb_raw <- read_ods(ctb_input, sheet = "Council_Taxbase_Data", range = "A6:N316")

ctb_val <- ctb_raw %>%
  clean_names()%>%
  select(!notes)

ctb_val <- ctb_val |>
    select(!contains('band'))

ctb_val_2324 <- restructure_2324(ctb_val)
ctb_val_2324 <- ctb_val_2324 |>
    dplyr::rename(onscode = ons_code)

ctb_val_2324 <- left_join(ctb_val_2324, select(ctr, ecode:class), by = 'onscode')
ctb_val_2324 <- ctb_val_2324 |>
  relocate(ecode:class)
#view(ctr)
#view(ctb_val_2324)

write.csv(ctb_val_2324, "D:\\Users\\emily.keenan\\OneDrive - MHCLG\\Documents\\GitHub\\council-tax\\output\\ctb_val.csv")



