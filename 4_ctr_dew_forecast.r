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

#######################################################################################################################################
######################### analyse the input data ######################################################
#######################################################################################################################################

analyse_30yr <- input_ct_dew |>
  arrange(year_29)

#### average of top and bottom ####
median(select(analyse_30yr, contains('year_')))
median(as.matrix(select(analyse_30yr, year_1)))
median(select())
str(select(analyse_30yr, year_1))
as.numeric(select(analyse_30yr, year_1))
as.matrix(select(analyse_30yr, contains('year_')))
median(as.matrix(select(analyse_30yr, contains('year_'))))
matrix_30yr <- as.matrix(select(analyse_30yr, contains('year_')))
median(matrix_30yr)
view(matrix_30yr)


tail_30yr <- apply(matrix_30yr[1:(nrow(matrix_30yr)/2),], 2, median)
head_30yr <- apply(matrix_30yr[(nrow(matrix_30yr)/2):(nrow(matrix_30yr)),], 2, median)
yr30_diff <- as.data.frame(head_30yr - tail_30yr)
future_years <- seq(from = 2024, length.out = 29)
yr30_diff <- cbind(yr30_diff, future_years)
yr30_diff <- yr30_diff |>
  rename(diff = 'head_30yr - tail_30yr', year = 'future_years')


diff_yr30_plot <- yr30_diff %>% 
        ggplot(aes(x = year , y = diff)) +              #input data fro hh percentage and income band
        geom_bar(stat = "identity", fill = "#d62d60")  +                                        #specify a bar chart
        #scale_y_continuous(limits = c(0,)) +     
        theme_minimal() +                    
        ylab("Average gap bewteen authorities with the larger CTR per dwelling and\
        authorities with the smaller CTR per dwelling (£)") + 
        xlab("Year")                                           #add axis lables
    diff_yr30_plot <- diff_yr30_plot + theme(text = element_text(size = 20))                  #increase text size 
   
    ggplot2::ggsave(paste0(file_loc), a)
    ggplot2::ggsave(paste0(file_loc_x), a)

yr30_diff[,1]
view(analyse_30yr)
apply(matrix_30yr[1:nrow(matrix_30yr)/2,], 2, median)
apply(matrix_30yr[nrow(matrix_30yr)/2:nrow(matrix_30yr),], 2, median)

yr30_diff <- cbind(yr30_diff, ctr_perdwel_long$name)

view(ctr_per_dwel)

ctr_perdwel_long <- pivot_longer(ctr_per_dwel[1,], !ecode:class)


view(yr30_diff)
view(tail_30yr)
view(head_30yr)
view(matrix_30yr[1:(nrow(matrix_30yr)/2),])
view(matrix_30yr[1:148,])
matrix_30yr[1:,]

view(ctr_perdwel_long)
