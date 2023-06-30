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

###############################################################################################################################################################
######################## QA check ########################
###############################################################################################################################################################
year1_check <- select(ordered_ctr_forecast, year_1)/select(val_forecast_reg, year_1)                        #double check the above lines does what I think it does
year1_check == select(ctr_per_dwel, year_1)                                                                 #compare results

#alternative to above
input_ct_dew <- ctr_per_dwel %>% ungroup()                                                                  #ungroup 
input_ct_dew_long <-  pivot_longer(input_ct_dew, contains("year"))  %>% mutate(name = as.numeric(str_replace(name, "year_", "")))     #remove year from all the column names

yr_30_plot <- ggplot(input_ct_dew_long, aes(name, value, group = ecode, color = class)) + geom_line(size=1.2)                         #plot the ctr per dwelling of all local authority
ggsave(yr_30_plot, 'output\\avearea_30yr_plot.png')                                                                                   #save plot 

#######################################################################################################################################
######################### analyse the input data ######################################################
#######################################################################################################################################

analyse_30yr <- input_ct_dew |>
  arrange(year_29)                                                                                          #arrange the data frame by local authorities ctr per dwelling 

#### average of top and bottom ####
matrix_30yr <- as.matrix(select(analyse_30yr, contains('year_')))                                           #format ctr per dwellings in order of LAs with the most to least 

tail_30yr <- apply(matrix_30yr[1:(nrow(matrix_30yr)/2),], 2, median)                                        #calculat the median of the smallest ctr per dwelling
head_30yr <- apply(matrix_30yr[(nrow(matrix_30yr)/2):(nrow(matrix_30yr)),], 2, median)                      #calculate the median of the largest ctr per dwelling
yr30_diff <- as.data.frame(head_30yr - tail_30yr)                                                           #calculate the difference between the largest and smallest median 
future_years <- seq(from = 2024, length.out = 29)                                                           #vector of the years we have forecasted for 
yr30_diff <- cbind(yr30_diff, future_years)                                                                 #add the vector of forecast years to the median difference 
yr30_diff <- yr30_diff |>
  rename(diff = 'head_30yr - tail_30yr', year = 'future_years')                                             #update the column names

diff_yr30_plot <- yr30_diff %>%                                                                             #input the difference in average for each year
        ggplot(aes(x = year , y = diff)) +
        geom_bar(stat = "identity", fill = "#d62d60")  +                                                    #specify a bar chart and colour
        #scale_y_continuous(limits = c(0,)) +     
        theme_minimal() +                    
        ylab("Average gap bewteen authorities with the larger CTR per dwelling and\
        authorities with the smaller CTR per dwelling (£)") + 
        xlab("Year")                                                                                        #add axis lables
    diff_yr30_plot <- diff_yr30_plot + theme(text = element_text(size = 20))                                #increase text size 
   
    ggplot2::ggsave("output\\ave_ctrdwe_diff.png", diff_yr30_plot)                                          #save the diff plot 
