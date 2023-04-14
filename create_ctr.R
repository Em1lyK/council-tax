#emily keenan
#14/04/2023
#building 2023-24 ctr for referendum principles model

library(tidyverse)

library("readODS")

cts_loc <- "Q:\\ADD Directorate\\Local Policy Analysis\\LGF\\Council Tax\\Referendum principles\\2023-24 Referendum Principles Tool\\CTR_Table_9_2023-24.ods"



## read in billing and precepting ctr data
ctr_billing_raw <-read_ods(cts_loc, sheet = "Data_Billing", skip = 4)

ctr_precepting_raw <- read_ods(cts_loc, sheet = "Data_Precepting", skip = 5)

##Isolate relevant rows 


#billing authorities: rename authority, tstb and band d columns
#used column numbers because the names are horrifying

ctr_billing <- ctr_billing_raw %>%
  rename(ecode = 'E-code',
         onscode = 'ONS Code',
         authority = Authority,
         region = Region,
         class = Class,
         tstb2223 = 23, 
         tstb2324 = 24,
         avebandd_expp2223 = 27, 
         avebandd_expp2324 = 28
  )

#isolate relevant columns

ctr_billing <- ctr_billing %>% 
  select(ecode:class, tstb2223:tstb2324, avebandd_expp2223:avebandd_expp2324)



#precepting authorities: rename authority, tstb and band d columns

ctr_precepting <- ctr_precepting_raw %>% 
  rename(ecode = 'E-code',
         onscode = 'ONS Code',
         authority = Authority,
         region = Region,
         class = Class, 
         tstb2223 = 11, 
         tstb2324 = 12,
         avebandd_expp2223 = 13, 
         avebandd_expp2324 = 14
  )

#isolate relevant columns

ctr_precepting <- ctr_precepting %>% 
  select(ecode:class, tstb2223:tstb2324, avebandd_expp2223:avebandd_expp2324)

##combing the billing and precepting data 
ctr <- rbind(ctr_billing, ctr_precepting)

ctr <- ctr[c(1:6, 303:316, 7:302, 317:408),]

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





