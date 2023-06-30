#09/06/23
#Emily Keenan 
#code to calculate the ctr forecast per dwelling 
library(plotly)

#### order the ctr forecast in the order of the dwellings forecast ####
ordered_ctr_forecast <- val_forecast_reg |>
    select(ecode)                                                                           #select the ecode column the valuation forecast data frame 

ordered_ctr_forecast <- left_join(ordered_ctr_forecast, ctr_forecast, by = 'ecode')         #left join the ctr forecast to the ecode column which is in the order of the valuation forecast 
                                                                                            #this give you the ctr forecast in the same order of the valuation forecast

#### ctr per valuation list dwelling ####
ctr_per_dwel <- select(ordered_ctr_forecast, contains('year'))/ select(val_forecast_reg, contains('year')) #divide the ctr forecast by the valuation list forecast
ctr_per_dwel <- cbind(ctb_val |>
                        select(ecode:class)|>
                        na.omit(), ctr_per_dwel)                                                            #add the la details back on to the datat frame

### QA check ###
year1_check <- select(ordered_ctr_forecast, year_1)/select(val_forecast_reg, year_1)                        #double check the above lines does what I think it does
year1_check == select(ctr_per_dwel, year_1)                                                                 #compare results


#### isolate year 1 and year 30 ####
view(ctr_per_dwel)
input_ct_dew <- ctr_per_dwel %>% ungroup() %>% mutate(across(contains("year"), ~. - year_1)  )
# alternative to above
input_ct_dew <- ctr_per_dwel %>% ungroup()
input_ct_dew_long <-  pivot_longer(input_ct_dew, contains("year"))  %>% mutate(name = as.numeric(str_replace(name, "year_", "")))
view(rel_m8_innit)
view(rel_m8_long)

yr_30_plot <- ggplot(input_ct_dew_long, aes(name, value, group = ecode, color = class)) + geom_line(size=1.2) 
ggsave(yr_30_plot, 'output\\avearea_30yr_plot.png')


nottingham <- val_forecast_reg |>
  dplyr::filter(onscode == 'E06000018') |>
  pivot_longer(!region:authority) |>
  select(name:value)

nottingham <- cbind(nottingham, c(1:29))
nottingham <- nottingham |>
  dplyr::rename(year = 'c(1:29)')

nottingham_plot <- ggplot(nottingham, aes(x = year, y = value, group = 1)) + geom_line()




west_devon <- ctr_forecast |>
  dplyr::filter(onscode == 'E07000047') |>
  pivot_longer(!region:authority) |>
  select(name:value)

west_devon <- cbind(west_devon, c(1:29))
west_devon <- west_devon |>
  dplyr::rename(year = 'c(1:29)')

plot_west_devon <- ggplot(west_devon, aes(x = year, y = value, group = 1)) + geom_line()



str(ctr_perdwel_long)

view(ctr_perdwel_long)
plot %>%
  ggplot( aes(x=date, y=ctr_per_dwell, group=region)) +
    geom_line() +
    geom_point() +
    ggtitle("insert titleeeeee") 





#################################################################################################
############### PREVIOUS TESTS#######################
ctr_perdwel_long <- pivot_longer(ctr_per_dwel, !ecode:class)
ctr_forecast_long <- pivot_longer(ctr_forecast, !region:authority)
val_forecast_long <- pivot_longer (val_forecast_reg, !region:authority)
tstb_forecast_long <- pivot_longer(tstb_forecast_reg, !region:authority)
bandd_forecast_long <- pivot_longer(ordered_bandd_forecast, !region:authority)

ctr_dwel_plot <- ggplot(ctr_perdwel_long, aes(name, value, group = ecode, color = region)) + geom_line(size=1.2)
ctr_fore_plot <- ggplot(ctr_forecast_long, aes(name, value, group = ecode, color = region)) + geom_line(size=1.2)
val_forecast_plot <- ggplot(val_forecast_long, aes(name, value, group = ecode, color = region)) + geom_line(size=1.2)
tstb_forecast_plot <- ggplot(tstb_forecast_long, aes(name, value, group = ecode, color = region)) + geom_line(size=1.2)
bandd_forecast_plot <- ggplot(bandd_forecast_long, aes(name, value, group = ecode, color = region)) + geom_line(size=1.2)