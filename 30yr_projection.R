#forecast band d assuming 3% increase in each year and 1% tstb increase
#27/04/23

#### define variables ####

#band d and tstb percentage increase and forecast length
levels_growth <- 0.03
tstb_growth <- 0.01

forecast_len <- 30

#create multiplication coefficients of ct for the next x years
power <- (1:forecast_len)
increase <- as.matrix((1+levels_growth)^power)

#empty data frame
d <- data.frame(matrix(ncol = 29, nrow = 408))



#### function to multiply a column by a list of numbers and output many columns ####
multiply_repeat <- function(b, c, d) {
  i <- 1 
  while (i < 30) {
    d[,i] <<- data.frame(b*c[[i]])
    i = i+1
    if (i== 30){
      break
      }
  }
}



####initial data ####

current_bandd <- ctr %>% 
  select(ecode:class, avebandd_expp2324)

#### forecast band d ####

#call forecasting function 
multiply_repeat(current_bandd$avebandd_expp2324, increase, forecast_bandd)


###tidy up output data frame

forecast_bandd <- d

#rename columns
forecast_bandd <- forecast_bandd %>% 
  rename_with(~ gsub("X", "year_", .x, fixed = TRUE))

#add la names/references
forecast_bandd <- cbind(forecast_bandd, current_bandd %>% 
                          select(ecode:region))
forecast_bandd <- forecast_bandd %>% 
  relocate(ecode:region)


#################################################################
#### QA ####
#################################################################


d <-d %>% 
  mutate(check = X29/current_bandd$avebandd_expp2324 == 1.03^29) %>% 
  mutate(num_check = X29/current_bandd$avebandd_expp2324 - 1.03^29) %>% 
  add_column(current_bandd$authority)


####################################################################
#### 30 year taxbase projection ####
####################################################################

#### define forecast parameters ####
tstb_growth <- 0.01


#vector to calculate tstb every year for the length of the forecast 
tstb_power <- (1:tstb_len)
tstb_incr <- as.matrix((1+tstb_growth)^tstb_power)


multiply_repeat(ctr$tstb2324, tstb_incr, forecast_tsbt)


###tidy up output data frame

forecast_tstb <- d

#rename columns
forecast_tstb <- forecast_tstb %>% 
  rename_with(~ gsub("X", "year_", .x, fixed = TRUE))

#add la names/references
forecast_tstb <- cbind(forecast_tstb, current_bandd %>% 
                          select(ecode:region))
forecast_tstb <- forecast_tstb %>% 
  relocate(ecode:region)


#############################################################
####QA tstb projection ####
#######################################################

d <-d %>% 
  mutate(check = X29/ctr$tstb2324 == 1.01^29) %>% 
  mutate(num_check = X29/ctr$tstb2324 - 1.01^29) %>% 
  add_column(current_bandd$authority)



###############################################
####calculate   ctr in each forecast year ####
############################################

forecast_ctr <- forecast_bandd[c(5:33)]* forecast_tstb[c(5:33)]

#add la names/references
forecast_ctr <- cbind(forecast_ctr, current_bandd %>% 
                         select(ecode:region))
forecast_ctr <- forecast_ctr %>% 
  relocate(ecode:region)

#### Multiplication QA ####
forecast_ctr$year_1 == forecast_tstb$year_1*forecast_bandd$year_1
(ctr[1,7]*1.01^29)*(ctr[1,9]*1.03^29)== forecast_ctr[1, 33]


tb_forecast <- ctr |>
  mutate(year_growth = tstb2324/ tstb2223-1)
view(tb_forecast)       

###########################################################
####### calculate historic percetnage increase ############
###########################################################

#merge all the historic data frames together
ctr_historic <- ctr |>
  select(ecode:class, tstb_2324) |>
  inner_join(ctr_1516, by = "ecode")|>
  inner_join(ctr_1617, by = "ecode") |>
  inner_join(ctr_1718, by = "ecode") |>
  inner_join(ctr_1819, by = "ecode") |>
  inner_join(ctr_1920, by = "ecode") |>
  inner_join(ctr_2021, by = "ecode") |>
  inner_join(ctr_2122, by = "ecode") |>
  inner_join(ctr_2223, by = "ecode") |>
  relocate(tstb_2324, .after = last_col())

#tidy up the datat frame
ctr_historic <- ctr_historic |>
  select(ecode:class.x, contains("tstb")) |>
  filter(!class.x %in% c("PCC", "MF", "CFA", "CA", "SC", "GLA", "[z]")) |>
  filter(!region %in% c("Eng", "[z]"))

ctr_historic <- ctr_historic |>
  mutate_at(c("tstb_1516", "tstb_1617", "tstb_1718", "tstb_1819", "tstb_1920", "tstb_2021", "tstb_2122", "tstb_2223"), as.numeric)

str(ctr_historic)

#group by region and sum up
region_historic <- ctr_historic |>
  group_by(region) |>
  summarise(across(contains("tstb"), ~ sum(.x, na.rm=TRUE)))

#create a tstb total datat frame
total_tstb <- numcolwise(sum)(region_historic)

# pivot longer so I cal graph it
total_tstb <- pivot_longer(total_tstb, (c("tstb_1516", "tstb_1617", "tstb_1718", "tstb_1819", "tstb_1920", "tstb_2021", "tstb_2122", "tstb_2223", "tstb_2324")))

#trying to rename columns but currently UNSUCCESSFUL ###########################
total_tstb <- total_tstb |>
  rename(TSTB = value, Year = name)

#plot of total tstb but only using consistent local authorities  
ggplot(total_tstb, aes(x=name , y=value)) + geom_point(size = 5, shape =15) +
  theme(axis.text=element_text(size=17), axis.title=element_text(size=17,face="bold"))

view(total_tstb)
view(ctr_2223)
view(region_historic)
view(ctr_1516

#write_csv(ctr_historic, "output\\ctr_historic.csv")
#write_csv(region_historic, "output\\reg_his_tstb.csv")

view(ctr_historic)
view(ctr)
