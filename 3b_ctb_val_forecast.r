#02/06/23
#Emily Keenan
#read in the ctb and forecast 

#location of ctb file
ctb_input <- c('input\\Council_Taxbase_local_authority_level_data_2022 (4).ods')
#read in ctb
ctb_raw <- read_ods(ctb_input, sheet = "Council_Taxbase_Data", range = "A6:N316")

ctb_val <- ctb_raw %>%
  clean_names()%>%
  select(!notes)

ctb_val <- ctb_val |>
    select(!contains('band'))

view(ctb_val)

####################################################################
#### 30 year taxbase projection with regional discrimination ####
####################################################################

#### define forecast parameters ####
median_tstb_inc$median <- median_tstb_inc$median + 1                #add one to the growth 
g <- cbind(g, median_tstb_inc$region)                               #add region colum  to empty df
g <- g |>                                                           #tidy up region column
  dplyr::rename(region = 'median_tstb_inc$region') |>
  relocate(region)
view(east_forecast)
reg_inc_repeat(g, median_tstb_inc$median, 'forecast_tstbinc')      #call function to calculate the taxbase growth for the next 30yr in reference to yr 1



###START AGAIN HERE###
#### Call the function to apply the forecasted increase to each of the regions, store the data frame and join the region column back on 
tstb_multiply_repeat(ctb_val$total, east_forecast$value, dwel_forecast_e)
dwel_forecast_e <- d
forecast_e_tstb <- cbind(forecast_e_tstb, ctr$region)
tstb_multiply_repeat(ctr$tstb_2324, em_forecast$value, dwel_forecast_em)
forecast_em_tstb <- d
forecast_em_tstb <- cbind(forecast_em_tstb, ctr$region)
tstb_multiply_repeat(ctr$tstb_2324, l_forecast$value, forecast_l_tstb)
forecast_l_tstb <- d
forecast_l_tstb <- cbind(forecast_l_tstb, ctr$region)
tstb_multiply_repeat(ctr$tstb_2324, ne_forecast$value, forecast_ne_tstb)
forecast_ne_tstb <- d
forecast_ne_tstb <- cbind(forecast_ne_tstb, ctr$region)
tstb_multiply_repeat(ctr$tstb_2324, nw_forecast$value, forecast_nw_tstb)
forecast_nw_tstb <- d
forecast_nw_tstb <- cbind(forecast_nw_tstb, ctr$region)
tstb_multiply_repeat(ctr$tstb_2324, se_forecast$value, forecast_se_tstb)
forecast_se_tstb <- d
forecast_se_tstb <- cbind(forecast_se_tstb, ctr$region)
tstb_multiply_repeat(ctr$tstb_2324, sw_forecast$value, forecast_sw_tstb)
forecast_sw_tstb <- d
forecast_sw_tstb <- cbind(forecast_sw_tstb, ctr$region)
tstb_multiply_repeat(ctr$tstb_2324, wm_forecast$value, forecast_wm_tstb)
forecast_wm_tstb <- d
forecast_wm_tstb <- cbind(forecast_wm_tstb, ctr$region)
tstb_multiply_repeat(ctr$tstb_2324, yh_forecast$value, forecast_yh_tstb)
forecast_yh_tstb <- d
forecast_yh_tstb <- cbind(forecast_yh_tstb, ctr$region)

#filter out the correct region from each of the forecasted region data frames 
forecast_e_tstb <- forecast_e_tstb |>
   dplyr::rename(region = 'ctr$region') |>
  dplyr::filter(region == 'E')
forecast_em_tstb <- forecast_em_tstb |>
   dplyr::rename(region = 'ctr$region') |>
   dplyr::filter(region == 'EM')
forecast_l_tstb <- forecast_l_tstb |>
  dplyr::rename(region = 'ctr$region') |>
  dplyr::filter(region == 'L')
forecast_ne_tstb <- forecast_ne_tstb |>
  dplyr::rename(region = 'ctr$region') |>
  dplyr::filter(region == 'NE')
forecast_nw_tstb <- forecast_nw_tstb |>
  dplyr::rename(region = 'ctr$region') |>
  dplyr::filter(region == 'NW')
forecast_se_tstb <- forecast_se_tstb |>
  dplyr::rename(region = 'ctr$region') |>
  dplyr::filter(region == 'SE')
forecast_sw_tstb <- forecast_sw_tstb |>
  dplyr::rename(region = 'ctr$region') |>
  dplyr::filter(region == 'SW')
forecast_wm_tstb <- forecast_wm_tstb |>
  dplyr::rename(region = 'ctr$region') |>
  dplyr::filter(region == 'WM')
forecast_yh_tstb <- forecast_yh_tstb |>
  dplyr::rename(region = 'ctr$region') |>
  dplyr::filter(region == 'YH')

#bind the forecasted tstb back together 
tstb_forecast_reg <- rbind(forecast_e_tstb, forecast_em_tstb, forecast_l_tstb, forecast_ne_tstb, 
                            forecast_nw_tstb, forecast_se_tstb, forecast_sw_tstb, forecast_wm_tstb, forecast_yh_tstb)
#rename the authority column
tstb_forecast_reg <- tstb_forecast_reg |>
  dplyr::rename(authority = 'current_bandd$authority')
#Select the LA identifying columns from the ctr  
name_code <- ctr |>
  select(ecode:authority)
tstb_forecast_reg <- left_join(tstb_forecast_reg, name_code, by = 'authority')                    #join the identifying column back into the forecasted data frame
tstb_forecast_reg <- tstb_forecast_reg |>                                                         #reorganise the identifying columns 
  relocate(authority:onscode)

#rename columns
tstb_forecast_reg <- tstb_forecast_reg %>% 
  rename_with(~ gsub("X", "year_", .x, fixed = TRUE)) |>
  dplyr::select(-check, -num_check)