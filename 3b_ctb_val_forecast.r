#02/06/23
#Emily Keenan
#read in the ctb and forecast 

#location of ctb file
ctb_loc <- 'https://raw.githubusercontent.com/Em1lyK/council-tax/main/output/ctb_val.csv'
ctb_val <- readr::read_csv(url(ctb_loc))

#define empty data frame
output <- data.frame(matrix(ncol = 29, nrow = 297))


### unify column names 
ctb_val <- ctb_val |>
  select(-...1)

####################################################################
#### 30 year taxbase projection with regional discrimination ####
####################################################################


###START AGAIN HERE###
#### Call the function to apply the forecasted increase to each of the regions, store the data frame and join the region column back on 
tstb_multiply_repeat(ctb_val$total, east_forecast$value, dwel_forecast_e)
forecast_e_val <- output
forecast_e_val <- cbind(forecast_e_val, ctb_val$region, ctb_val$ecode)
tstb_multiply_repeat(ctb_val$total, em_forecast$value, dwel_forecast_em)
forecast_em_val <- output
forecast_em_val <- cbind(forecast_em_val, ctb_val$region, ctb_val$ecode)
tstb_multiply_repeat(ctb_val$total, l_forecast$value, dwel_forecast_l)
forecast_l_val <- output
forecast_l_val <- cbind(forecast_l_val, ctb_val$region, ctb_val$ecode)
tstb_multiply_repeat(ctb_val$total, ne_forecast$value, dwel_forecast_ne)
forecast_ne_val <- output
forecast_ne_val <- cbind(forecast_ne_val, ctb_val$region, ctb_val$ecode)
tstb_multiply_repeat(ctb_val$total, nw_forecast$value, dwel_forecast_nw)
forecast_nw_val <- output
forecast_nw_val <- cbind(forecast_nw_val, ctb_val$region, ctb_val$ecode)
tstb_multiply_repeat(ctb_val$total, se_forecast$value, dwel_forecast_se)
forecast_se_val <- output
forecast_se_val <- cbind(forecast_se_val, ctb_val$region, ctb_val$ecode)
tstb_multiply_repeat(ctb_val$total, sw_forecast$value, dwel_forecast_sw)
forecast_sw_val <- output
forecast_sw_val <- cbind(forecast_sw_val, ctb_val$region, ctb_val$ecode)
tstb_multiply_repeat(ctb_val$total, wm_forecast$value, dwel_forecast_wm)
forecast_wm_val <- output
forecast_wm_val <- cbind(forecast_wm_val, ctb_val$region, ctb_val$ecode)
tstb_multiply_repeat(ctb_val$total, yh_forecast$value, dwel_forecast_yh)
forecast_yh_val <- output
forecast_yh_val <- cbind(forecast_yh_val, ctb_val$region, ctb_val$ecode)

#filter out the correct region from each of the forecasted region data frames 
forecast_e_val <- forecast_e_val |>
   dplyr::rename(region = 'ctb_val$region') |>
   dplyr::rename(ecode = 'ctb_val$ecode') |>
  dplyr::filter(region == 'E')
forecast_em_val <- forecast_em_val |>
   dplyr::rename(region = 'ctb_val$region') |>
   dplyr::rename(ecode = 'ctb_val$ecode') |>
   dplyr::filter(region == 'EM')
forecast_l_val <- forecast_l_val |>
  dplyr::rename(region = 'ctb_val$region') |>
  dplyr::rename(ecode = 'ctb_val$ecode') |>
  dplyr::filter(region == 'L')
forecast_ne_val <- forecast_ne_val |> 
  dplyr::rename(region = 'ctb_val$region') |>
  dplyr::rename(ecode = 'ctb_val$ecode') |>
  dplyr::filter(region == 'NE')
forecast_nw_val <- forecast_nw_val |>
  dplyr::rename(region = 'ctb_val$region') |>
  dplyr::rename(ecode = 'ctb_val$ecode') |>
  dplyr::filter(region == 'NW')
forecast_se_val <- forecast_se_val |>
  dplyr::rename(region = 'ctb_val$region') |>
  dplyr::rename(ecode = 'ctb_val$ecode') |>
  dplyr::filter(region == 'SE')
forecast_sw_val <- forecast_sw_val |>
  dplyr::rename(region = 'ctb_val$region') |>
  dplyr::rename(ecode = 'ctb_val$ecode') |>
  dplyr::filter(region == 'SW')
forecast_wm_val <- forecast_wm_val |>
  dplyr::rename(region = 'ctb_val$region') |>
  dplyr::rename(ecode = 'ctb_val$ecode') |>
  dplyr::filter(region == 'WM')
forecast_yh_val <- forecast_yh_val |>
  dplyr::rename(region = 'ctb_val$region') |>
  dplyr::rename(ecode = 'ctb_val$ecode') |>
  dplyr::filter(region == 'YH')

#bind the forecasted valuation hh back together 
val_forecast_reg <- rbind(forecast_e_val, forecast_em_val, forecast_l_val, forecast_ne_val, 
                            forecast_nw_val, forecast_se_val, forecast_sw_val, forecast_wm_val, forecast_yh_val)
                            
#Select the LA identifying columns from the ctr  
name_code <- ctr |>
  select(ecode:authority)
val_forecast_reg <- left_join(val_forecast_reg, name_code, by = 'ecode')                    #join the identifying column back into the forecasted data frame
val_forecast_reg <- val_forecast_reg |>                                                         #reorganise the identifying columns 
  relocate(region:authority)

#rename columns
val_forecast_reg <- val_forecast_reg %>% 
  rename_with(~ gsub("X", "year_", .x, fixed = TRUE))

