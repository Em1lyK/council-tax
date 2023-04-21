#forecast band d assuming 3% increase in each year

#define variables
#band d percentage increase
growth_rate <- 0.03

multiply_repeat <- function(b, c, d) {
  i <- 1 
  while (i < 30) {
    d[,i] <<- data.frame(b*c[[i]])
    i+1
    if (i== 30){
      break
      }
  }
}



forecast_bandd <- current_bandd %>% mutate(with_purr = accumulate(avebandd_expp2324, ~ .x * (1 + growth_rate)))


multiply_repeat <- function(b, c, d) {
i <- 1
while (i<30){
  d <<- mutate(with_purr_i = accumulate(b, ~ .x*(1+c)))
  i+1
  if (i == 30){
    break
  }
}
}

d <- data.frame(matrix(ncol = 40, nrow = 408))

current_bandd <- ctr %>% 
  select(ecode:class, avebandd_expp2324)

#forecast band d

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

       