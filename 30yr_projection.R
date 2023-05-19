#forecast band d assuming 3% increase in each year and 1% tstb increase
#27/04/23

#graphing package and stats package (zoo)
install.packages('forecast', dependencies = TRUE)
install.packages("zoo")

library(zoo)
library(ggplot2) 
library(forecast) 
theme_set(theme_classic())

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
e <- data.frame(matrix(ncol = 9, nrow = 9))



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


#### function to calculate tstb percentage increases 
precentage_inc <- function(a) {
  i <- 3 
  j <- 1
  while (i < 11) {
    e[,j] <<- data.frame((a[,i]- a[,i-1])/a[,i-1])
    i = i+1
    j = j+1
    if (i== 11){
      break
      }
  }
  return(e)
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
  dplyr::summarise(dplyr::across(dplyr::contains("tstb"), ~ sum(.x, na.rm=TRUE)))

#create a tstb total datat frame
total_tstb <- numcolwise(sum)(region_historic)

# pivot longer so I cal graph it
total_tstb <- pivot_longer(total_tstb, (c("tstb_1516", "tstb_1617", "tstb_1718", "tstb_1819", "tstb_1920", "tstb_2021", "tstb_2122", "tstb_2223", "tstb_2324")))

#trying to rename columns but currently UNSUCCESSFUL ###########################
total_tstb <- total_tstb |>
  dplyr::rename(TSTB = value, Year = name)

#plot of total tstb but only using consistent local authorities  
ggplot(total_tstb, aes(x=Year , y=TSTB)) + geom_point(size = 5, shape =15) +
  theme(axis.text=element_text(size=17), axis.title=element_text(size=17,face="bold"))

region_historic_long <- pivot_longer(region_historic, !region)

tstb_region_plot <- ggplot(region_historic_long, aes(name, value, group = region, color = region)) + geom_line(size=1.2)
tstb_region_plot

#call function to calculate the percentage increase in tstb by region
precentage_inc(region_historic)
regtstb_inc <- e

regtstb_inc <- regtstb_inc |>
  select(!X9)
  
regtstb_inc <- cbind(regtstb_inc, region_historic$region)

regtstb_inc <- regtstb_inc |>
  dplyr::rename(region ="region_historic$region") |>
  relocate(region)

#check
(region_historic[,10]- region_historic[,9])/region_historic[,9] == e[,8]

view(region_historic)
view(regtstb_inc)

regtstb_inc_long <- pivot_longer(regtstb_inc, !region) 

view(regtstb_inc_long)

e_tstb_his <- regtstb_inc_long |>
  filter(region == "E")
em_tstb_his <- regtstb_inc_long |>
  filter(region == "EM")
l_tstb_his <- regtstb_inc_long |>
  filter(region == "L")
en_tstb_his <- regtstb_inc_long |>
  filter(region == "EN")
nw_tstb_his <- regtstb_inc_long |>
  filter(region == "NW")
se_tstb_his <- regtstb_inc_long |>
  filter(region == "SE")
sw_tstb_his <- regtstb_inc_long |>
  filter(region == "SW")
wm_tstb_his <- regtstb_inc_long |>
  filter(region == "WM")
yh_tstb_his <- regtstb_inc_long |>
  filter(region == "YH")


e_tstb_mave <- zoo::rollmean(e_tstb_his$value, 8)
em_tstb_mave <- zoo::rollmean(e_tstb_his$value, 8)
l_tstb_mave <- zoo::rollmean(e_tstb_his$value, 8)
en_tstb_mave <- zoo::rollmean(e_tstb_his$value, 8)
nw_tstb_mave <- zoo::rollmean(e_tstb_his$value, 8)
se_tstb_mave <- zoo::rollmean(e_tstb_his$value, 8)
sw_tstb_mave <- zoo::rollmean(e_tstb_his$value, 8)
wm_tstb_mave <- zoo::rollmean(e_tstb_his$value, 8)
yh_tstb_mave <- zoo::rollmean(e_tstb_his$value, 8)

moving_average <- function(x) {
  i <- 1
  while(i<30){
    f[,i] <<- data.frame()
  }

}

#### function to calculate tstb percentage increases 
precentage_inc <- function(a) {
  i <- 3 
  j <- 1
  while (i < 11) {
    e[,j] <<- data.frame((a[,i]- a[,i-1])/a[,i-1])
    i = i+1
    j = j+1
    if (i== 11){
      break
      }
  }
  return(e)
}



view(moving_ave)
view(region_historic_long)
view(e_tstb_his)


region_historic <- ts(region_historic, 2015, 2023, 1)
is.ts(region_historic)
str(region_historic)


view(total_tstb)
view(ctr_2223)
view(region_historic)
view(ctr_1516

#write_csv(ctr_historic, "output\\ctr_historic.csv")
#write_csv(region_historic, "output\\reg_his_tstb.csv")

view(ctr_historic)
view(ctr)



set.seed(1)

x.Date <- as.Date(paste(2004, rep(1:4, 4:1), sample(1:28, 10), sep = "-"))
view(x.Date)
x <- zoo(rnorm(12), x.Date)
view(x)
## rolling operations for univariate series
rollmean(x, 3)
