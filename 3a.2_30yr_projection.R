#forecast band d assuming 3% increase in each year and 1% tstb increase
#02/06/23

#graphing package and stats package (zoo)
install.packages('forecast', dependencies = TRUE)
install.packages("zoo")

library(tidyverse)
library("readODS")
library(janitor)
library(plyr)
library(zoo)
library(ggplot2) 
library(forecast) 
theme_set(theme_classic())


#### set working directory ####
#setwd("D:\\Users\\emily.keenan\\OneDrive - MHCLG\\Documents\\GitHub\\council-tax")


#### define variables ####

#band d and tstb percentage increase and forecast length
levels_growth <- 0.03
tstb_growth <- 0.01

forecast_len <- 30

#create multiplication coefficients of ct for the next x years
power <- (1:forecast_len)
increase <- as.matrix((1+levels_growth)^power)

#read in ctr from github 
ctr_loc_2324 <-  'https://raw.githubusercontent.com/Em1lyK/council-tax/main/output/areabandd_ctr.csv' 
ctr <- read_csv(url(ctr_loc_2324))
historic_ctr_loc <- 'https://raw.githubusercontent.com/Em1lyK/council-tax/main/output/ctr_historic.csv'
ctr_historic <- read_csv(url(historic_ctr_loc))

#empty data frame
d <- data.frame(matrix(ncol = 29, nrow = 302))
e <- data.frame(matrix(ncol = 9, nrow = 9))
f <- data.frame(matrix(ncol = 29, nrow = 302))
g <- data.frame(matrix(ncol = 29, nrow = 9))
output <- data.frame(matrix(ncol = 29, nrow = 302))

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


#### function to calculate the tstb growth for 30 yrs in each region ### 
reg_inc_repeat <- function(g, inital_inc, inc_df) {
  i <- 1
  while(i < 30) {
    g[,i+1] <<- data.frame(inital_inc ^ i) 
    i = i+1
   if (i== 30){
      break
      }
  }
  return(assign(paste0(inc_df), g, envir = parent.frame()))
}

#### function to multiply a column by a list of numbers and output many columns ####
tstb_multiply_repeat <- function(starting_val, increase_df, output) {
  i <- 1 
  while (i < 30) {
    output[,i] <<- data.frame(starting_val*increase_df[[i]])
    i = i+1
    if (i== 30){
      break
      }
  }
}

####initial data ####
current_bandd <- ctr %>% 
  select(ecode:class, areabandd_2324)

#### forecast band d ####

#call forecasting function 
multiply_repeat(current_bandd$areabandd_2324, increase, forecast_bandd)


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


###########################################################
####### calculate historic percetnage increase ############
###########################################################

#tidy up the datat frame
ctr_historic <- ctr_historic |>
  select(ecode:class.x, contains("tstb")) |>
  filter(!class.x %in% c("PCC", "MF", "CFA", "CA", "SC", "GLA", "[z]")) |>
  filter(!region %in% c("Eng", "[z]"))

ctr_historic <- ctr_historic |>
  mutate_at(c("tstb_1516", "tstb_1617", "tstb_1718", "tstb_1819", "tstb_1920", "tstb_2021", "tstb_2122", "tstb_2223"), 
            as.numeric) 

view(ctr_historic)
str(ctr_historic)
#group by region and sum up
region_historic <- ctr_historic |>
  group_by(region) |>
  dplyr::summarise(dplyr::across(dplyr::contains("tstb"), ~ sum(.x, na.rm=TRUE))) |>
  ungroup()

# region_historic <- region_historic |>
#   pivot_longer(!region) |>
#   ungroup()
# view(region_historic)

#create a tstb total datat frame
total_tstb <- numcolwise(sum)(region_historic)

# pivot longer so I cal graph it
total_tstb <- pivot_longer(total_tstb, (c("tstb_1516", "tstb_1617", "tstb_1718", "tstb_1819", "tstb_1920", "tstb_2021", "tstb_2122", "tstb_2223", "tstb_2324")))

#rename columns 
total_tstb <- total_tstb |>
  dplyr::rename(TSTB = value, Year = name)

#plot of total tstb but only using consistent local authorities  
ggplot(total_tstb, aes(x=Year , y=TSTB)) + geom_point(size = 5, shape =15) +
  theme(axis.text=element_text(size=17), axis.title=element_text(size=17,face="bold"))

region_historic_long <- pivot_longer(region_historic, !region)

tstb_region_plot <- ggplot(region_historic_long, aes(name, value, group = region, color = region)) + geom_line(size=1.2)
tstb_region_plot

####################################################################################################################
################call function to calculate the percentage increase in tstb by region###############################
####################################################################################################################

precentage_inc(region_historic)
regtstb_inc <- e                                                    #data frame frame of percentage increases in each year and region

regtstb_inc <- regtstb_inc |>                                       #remove na column
  select(!X9)                                 
regtstb_inc <- cbind(regtstb_inc, region_historic$region)           #add region column

regtstb_inc <- regtstb_inc |>                                       #move region coumn and rename
  dplyr::rename(region ="region_historic$region") |>
  relocate(region)
regtstb_inc_long <- pivot_longer(regtstb_inc, !region)              #restructure data frame 

mean_tstb_inc <- regtstb_inc_long |>                                #calculate the mean tstb increase 
  group_by(region) |>
  dplyr::summarise(mean = mean(value))

################################ trial different averages ################################################

### calculate mean without Covid year (20/21 to 21/22) ###
mean_noc <- regtstb_inc_long |>
  filter(!name == 'X6')                                             #remove rows with name == X6 (tstb change 20/21 to 21/22)

mean_noc <- mean_noc |>
    group_by(region) |>
    dplyr::summarise(mean_noc = mean(value))                          #calculate mean without Covid year

### calculate the median of tstb growth ###
median_tstb_inc <- regtstb_inc_long |>
  group_by(region) |>
  dplyr::summarise(median = median(value))

### calculate mean without Covid year (20/21 to 21/22) ###
median_noc <- regtstb_inc_long |>
  filter(!name == 'X6')                                             #remove rows with name == X6 (tstb change 20/21 to 21/22)

median_noc <- median_noc |>
    group_by(region) |>
    dplyr::summarise(median_noc = median(value))                          #calculate mean without Covid year


### combine the columns ###
ave_tstb_inc <- left_join(mean_tstb_inc,mean_noc, by = 'region')
ave_tstb_inc <- left_join(ave_tstb_inc, median_tstb_inc, by = 'region')
ave_tstb_inc <- left_join(ave_tstb_inc, median_noc, by = 'region')

######################################################################################################################
#### 30 year taxbase projection with regional discrimination ####
####################################################################################################################

#### define forecast parameters ####
median_tstb_inc$median <- median_tstb_inc$median + 1                #add one to the growth 
g <- cbind(g, median_tstb_inc$region)                               #add region colum  to empty df
g <- g |>                                                           #tidy up region column
  dplyr::rename(region = 'median_tstb_inc$region') |>
  relocate(region)
view(east_forecast)
reg_inc_repeat(g, median_tstb_inc$median, 'forecast_tstbinc')      #call function to calculate the taxbase growth for the next 30yr in reference to yr 1

### isolate the forecats for each of the regions and then pivot longer to be able to use the other function 
east_forecast <- forecast_tstbinc |>
  filter(region == 'E') |>
  pivot_longer(!region)
em_forecast <- forecast_tstbinc |>
  filter(region == 'EM') |>
  pivot_longer(!region)
l_forecast <- forecast_tstbinc |>
  filter(region == 'L') |>
  pivot_longer(!region)
ne_forecast <- forecast_tstbinc |>
  filter(region == 'NE') |>
  pivot_longer(!region)
nw_forecast <- forecast_tstbinc |>
  filter(region == 'NW') |>
  pivot_longer(!region)
se_forecast <- forecast_tstbinc |>
  filter(region == 'SE') |>
  pivot_longer(!region)
sw_forecast <- forecast_tstbinc |>
  filter(region == 'SW') |>
  pivot_longer(!region)
wm_forecast <- forecast_tstbinc |>
  filter(region == 'WM') |>
  pivot_longer(!region)
yh_forecast <- forecast_tstbinc |>
  filter(region == 'YH') |>
  pivot_longer(!region)

#### Call the function to apply the forecasted increase to each of the regions, store the data frame and join the region column back on 
tstb_multiply_repeat(ctr$tstb_2324, east_forecast$value, forecast_e_tstb)
forecast_e_tstb <- output
forecast_e_tstb <- cbind(forecast_e_tstb, ctr$region, ctr$ecode)
tstb_multiply_repeat(ctr$tstb_2324, em_forecast$value, forecast_em_tstb)
forecast_em_tstb <- output
forecast_em_tstb <- cbind(forecast_em_tstb, ctr$region, ctr$ecode)
tstb_multiply_repeat(ctr$tstb_2324, l_forecast$value, forecast_l_tstb)
forecast_l_tstb <- output
forecast_l_tstb <- cbind(forecast_l_tstb, ctr$region, ctr$ecode)
tstb_multiply_repeat(ctr$tstb_2324, ne_forecast$value, forecast_ne_tstb)
forecast_ne_tstb <- output
forecast_ne_tstb <- cbind(forecast_ne_tstb, ctr$region, ctr$ecode)
tstb_multiply_repeat(ctr$tstb_2324, nw_forecast$value, forecast_nw_tstb)
forecast_nw_tstb <- output
forecast_nw_tstb <- cbind(forecast_nw_tstb, ctr$region, ctr$ecode)
tstb_multiply_repeat(ctr$tstb_2324, se_forecast$value, forecast_se_tstb)
forecast_se_tstb <- output
forecast_se_tstb <- cbind(forecast_se_tstb, ctr$region, ctr$ecode)
tstb_multiply_repeat(ctr$tstb_2324, sw_forecast$value, forecast_sw_tstb)
forecast_sw_tstb <- output
forecast_sw_tstb <- cbind(forecast_sw_tstb, ctr$region, ctr$ecode)
tstb_multiply_repeat(ctr$tstb_2324, wm_forecast$value, forecast_wm_tstb)
forecast_wm_tstb <- output
forecast_wm_tstb <- cbind(forecast_wm_tstb, ctr$region, ctr$ecode)
tstb_multiply_repeat(ctr$tstb_2324, yh_forecast$value, forecast_yh_tstb)
forecast_yh_tstb <- output
forecast_yh_tstb <- cbind(forecast_yh_tstb, ctr$region, ctr$ecode)

#filter out the correct region from each of the forecasted region data frames 
forecast_e_tstb <- forecast_e_tstb |>
   dplyr::rename(region = 'ctr$region') |>
   dplyr::rename(ecode = 'ctr$ecode') |>
  dplyr::filter(region == 'E')
forecast_em_tstb <- forecast_em_tstb |>
   dplyr::rename(region = 'ctr$region') |>
   dplyr::rename(ecode = 'ctr$ecode') |>
   dplyr::filter(region == 'EM')
forecast_l_tstb <- forecast_l_tstb |>
  dplyr::rename(region = 'ctr$region') |>
  dplyr::rename(ecode = 'ctr$ecode') |>
  dplyr::filter(region == 'L')
forecast_ne_tstb <- forecast_ne_tstb |> 
  dplyr::rename(region = 'ctr$region') |>
  dplyr::rename(ecode = 'ctr$ecode') |>
  dplyr::filter(region == 'NE')
forecast_nw_tstb <- forecast_nw_tstb |>
  dplyr::rename(region = 'ctr$region') |>
  dplyr::rename(ecode = 'ctr$ecode') |>
  dplyr::filter(region == 'NW')
forecast_se_tstb <- forecast_se_tstb |>
  dplyr::rename(region = 'ctr$region') |>
  dplyr::rename(ecode = 'ctr$ecode') |>
  dplyr::filter(region == 'SE')
forecast_sw_tstb <- forecast_sw_tstb |>
  dplyr::rename(region = 'ctr$region') |>
  dplyr::rename(ecode = 'ctr$ecode') |>
  dplyr::filter(region == 'SW')
forecast_wm_tstb <- forecast_wm_tstb |>
  dplyr::rename(region = 'ctr$region') |>
  dplyr::rename(ecode = 'ctr$ecode') |>
  dplyr::filter(region == 'WM')
forecast_yh_tstb <- forecast_yh_tstb |>
  dplyr::rename(region = 'ctr$region') |>
  dplyr::rename(ecode = 'ctr$ecode') |>
  dplyr::filter(region == 'YH')

#bind the forecasted tstb back together 
tstb_forecast_reg <- rbind(forecast_e_tstb, forecast_em_tstb, forecast_l_tstb, forecast_ne_tstb, 
                            forecast_nw_tstb, forecast_se_tstb, forecast_sw_tstb, forecast_wm_tstb, forecast_yh_tstb)

#Select the LA identifying columns from the ctr  
name_code <- ctr |>
  select(ecode:authority)
tstb_forecast_reg <- left_join(tstb_forecast_reg, name_code, by = 'ecode')                    #join the identifying column back into the forecasted data frame
tstb_forecast_reg <- tstb_forecast_reg |>                                                         #reorganise the identifying columns 
  relocate(region:authority)

#rename columns
tstb_forecast_reg <- tstb_forecast_reg %>% 
  rename_with(~ gsub("X", "year_", .x, fixed = TRUE))

####match the band d forecast data frame to the tstb forecast data ###
#select identifying authority columns 
ordered_bandd_forecast <- tstb_forecast_reg |>
  select(region:authority)
#order the band d forecast in the same order as the tstb forecast 
ordered_bandd_forecast <- left_join(ordered_bandd_forecast, forecast_bandd, by = c('authority', 'region', 'ecode', 'onscode'))

ctr_forecast <- select(tstb_forecast_reg, contains('year')) * select(ordered_bandd_forecast, contains('year'))                #multiply the tstb forecast by the band d  forecast
ctr_forecast <- cbind(ctr_forecast, select(tstb_forecast_reg, region:authority))                                             #add the la id columns back in 
ctr_forecast <- ctr_forecast |>                                                                                               #reorganise the id columns
  relocate(region:authority)

birmingham <- ctr_forecast |>
  dplyr::filter(onscode == 'E08000025') |>
  pivot_longer(!region:authority) |>
  select(name:value)

birmingham <- cbind(birmingham, c(1:29))
birmingham <- birmingham |>
  dplyr::rename(year = 'c(1:29)')

plot_birmingham <- ggplot(birmingham, aes(x = year, y = value, group = 1)) + geom_line()

write_csv(birmingham, 'output\\birmingham.csv')




