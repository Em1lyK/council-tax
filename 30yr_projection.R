#forecast band d assuming 3% increase in each year

#### define variables ####

#band d percentage increase and forecast length
growth_rate <- 0.03
forecast_len <- 30

#create multiplication coefficients of ct for the next x years
power <- (1:forecast_len)
increase <- as.matrix((1+growth_rate)^power)

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

#################################################################
#### QA ####
#################################################################


d <-d %>% 
  mutate(check = X29/current_bandd$avebandd_expp2324 == 1.03^29) %>% 
  mutate(num_check = X29/current_bandd$avebandd_expp2324 - 1.03^29) %>% 
  add_column(current_bandd$authority)


       