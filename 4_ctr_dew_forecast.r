#09/06/23
#Emily Keenan 
#code to calculate the ctr forecast per dwelling 

#### order the ctr forecast in the order of the dwellings forecast ####
ordered_ctr_forecast <- val_forecast_reg |>
    select(ecode)                                                                           #select the ecode column the valuation forecast data frame 

ordered_ctr_forecast <- left_join(ordered_ctr_forecast, ctr_forecast, by = 'ecode')         #left join the ctr forecast to the ecode column which is in the order of the valuation forecast 
                                                                                            #this give you the ctr forecast in the same order of the valuation forecast

#### ctr per valuation list dwelling ####
ctr_per_dwel <- select(ordered_ctr_forecast, contains('year'))/ select(val_forecast_reg, contains('year'))

view(ordered_ctr_forecast)
view(val_forecast_reg)
view(ctr_per_dwel)
view(ctr_forecast)
count(ctr_forecast$year_1 = 0)


### QA check ###
year1_check <- select(ordered_ctr_forecast, year_1)/select(val_forecast_reg, year_1)
year1_check == select(ctr_per_dwel, year_1)

year1_check <- cbind(year1_check, select(ordered_ctr_forecast, authority))
invalid_check <- year1_check |>
    filter(authority = 'Invalid Number')
view(year1_check)

str(ordered_bandd_forecast, contains('year'))
str(val_forecast_reg, contains('year'))
a <-select(val_forecast_reg, contains('year'))
str(ordered_bandd_forecast)


rouge_rows <- ctb_val |>
    filter(authority == 'NA')
view(rouge_rows)


rouge_rows <- ctb_val |>
    filter(ecode == 'E0931')
view(rouge_rows)
