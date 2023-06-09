#09/06/23
#Emily Keenan 
#code to calculate the ctr forecast per dwelling 

#### order the ctr forecast in the order of the dwellings forecast ####
ordered_ctr_forecast <- val_forecast_reg |>
    select(ecode)

ordered_ctr_forecast <- left_join(ordered_ctr_forecast, ctr_forecast, by = 'ecode')

#### ctr per valuation list dwelling ####
ctr_per_dwel <- select(ordered_bandd_forecast, contains('year'))/ select(val_forecast_reg, contains('year'))
str(ordered_bandd_forecast, contains('year'))
str(val_forecast_reg, contains('year'))
a <-select(val_forecast_reg, contains('year'))
