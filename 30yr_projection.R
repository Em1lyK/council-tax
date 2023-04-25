#forecast band d assuming 3% increase in each year

#define variables
#band d percentage increase
growth_rate <- 0.03
d <- data.frame(matrix(ncol = 29, nrow = 408))

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



####inital data ####

current_bandd <- ctr %>% 
  select(ecode:class, avebandd_expp2324)

#forecast band d

multiply_repeat(current_bandd$avebandd_expp2324, increase, forecast_bandd)



test <- as.data.frame(d$X29/current_bandd$avebandd_expp2324 == 1.03^29)

d <-d %>% 
  mutate(check = X29/current_bandd$avebandd_expp2324 == 1.03^29) %>% 
  mutate(num_check = X29/current_bandd$avebandd_expp2324 - 1.03^29) %>% 
  add_column(current_bandd$authority)

print(current_bandd[62,3])
print(current_bandd[63,3])
print(if())

test_df <- data.frame(matrix(ncol = 29, nrow = 408, 1.03^29))
test_df = 1.03^29

check <- test_df- 


forecast_bandd <- current_bandd %>%
  mutate(yr2425 = avebandd_expp2324*(1+percent_inc)) 

multiply_repeat(current_bandd, current_bandd$avebandd_expp2324, increase, forecast_bandd)

%>% 
  mutate(yr2526 = yr2425*(1+percent_inc))
d <- data.frame()
multiply_repeat(current_bandd$avebandd_expp2324, increase, forecast_bandd)

power <- (1:30)
increase <- as.matrix(1.03^power)
bandd2324<- 


d[,1] <- data.frame(current_bandd$avebandd_expp2324*increase[[1]])
d[,2] <- data.frame(current_bandd$avebandd_expp2324*increase[[2]])
d[,3] <- data.frame(current_bandd$avebandd_expp2324*increase[[3]])

       