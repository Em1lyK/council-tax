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



#############################################################
####QA tstb projection ####
#######################################################

d <-d %>% 
  mutate(check = X29/ctr$tstb2324 == 1.01^29) %>% 
  mutate(num_check = X29/ctr$tstb2324 - 1.01^29) %>% 
  add_column(current_bandd$authority)



       