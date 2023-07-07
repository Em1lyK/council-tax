#plot the average difference between the historic ct per dwelling of local authortiies 
#Emily Keenan
#30/06/23

library(readxl)
ctr_2223_expp <- ctr_2223_expp |>
    mutate(area_bandd_2223 = as.numeric(area_bandd_2223)) |>
    mutate(tstb_2223 = as.numeric(tstb_2223))

#function to clean ctrs and select area band d and tstb coloumns (20/21 to 22/23)
clean_ctr <- function(output, raw_input, yr, area_col, tstb_col) {
    a <- raw_input |>
    clean_names()                                                                       #clean names from the excel sheet

    a <- a |>
    dplyr::rename(  !! paste0("area_bandd_", yr) := paste0(area_col),                   #rename the area band d column 
                    !! paste0("tstb_", yr) := paste0(tstb_col)) |>                      #rename the tstb column
    select(ecode:class, paste0("area_bandd_", yr), paste0("tstb_", yr), ons_code) |>    #select the LA identification columns, area band d, and tstb columns
    relocate(ons_code) |>                                                               #relocate ons code column 
    filter(ecode != "NA")                                                               #remove rows with no ecode


    return(assign(output, a, envir = parent.frame()))                                   #defined data frame in the global environment 

}

#function to clean ctrs and select area band d and tstb columns (16/17 to 20/21)
clean_ctr_2 <- function(output, raw_input, yr, area_col, tstb_col) {    
    a <- raw_input |>
    clean_names()

     a <- a |>
    dplyr::rename(  !! paste0("area_bandd_", yr) := paste0(area_col), 
                    !! paste0("tstb_", yr) := paste0(tstb_col)) |>
    select(ecode:class, paste0("area_bandd_", yr), paste0("tstb_", yr)) |>
    filter(ecode != "NA")
    return(assign(output, a, envir = parent.frame()))
    
}

#location of historic ctr data 
ctr_2223_raw_loc <- paste0("https://raw.githubusercontent.com/Em1lyK/council-tax/main/output/ctr_2223_raw.csv")
ctr_2122_raw_loc <- paste0("https://raw.githubusercontent.com/Em1lyK/council-tax/main/output/ctr_2122_raw.csv")
ctr_2021_raw_loc <- paste0("https://raw.githubusercontent.com/Em1lyK/council-tax/main/output/ctr_2021_raw.csv")
ctr_1920_raw_loc <- paste0("https://raw.githubusercontent.com/Em1lyK/council-tax/main/output/ctr_1920_raw.csv")
ctr_1819_raw_loc <- paste0("https://raw.githubusercontent.com/Em1lyK/council-tax/main/output/ctr_1819_raw.csv")
ctr_1718_raw_loc <- paste0("https://raw.githubusercontent.com/Em1lyK/council-tax/main/output/ctr_1718_raw.csv")
ctr_1617_raw_loc <- paste0("https://raw.githubusercontent.com/Em1lyK/council-tax/main/output/ctr_1617_raw.csv")
ctr_1516_raw_loc <- paste0("https://raw.githubusercontent.com/Em1lyK/council-tax/main/output/ctr_1516_raw.csv")

#read in the historic ctr data 
ctr_2223_raw <- read_csv(url(ctr_2223_raw_loc), col_names = FALSE)
ctr_2122_raw <- read_csv(url(ctr_2122_raw_loc), col_names = FALSE)
ctr_2021_raw <- read_csv(url(ctr_2021_raw_loc), col_names = FALSE)
ctr_1920_raw <- read_csv(url(ctr_1920_raw_loc), col_names = FALSE)
ctr_1819_raw <- read_csv(url(ctr_1819_raw_loc), col_names = FALSE)
ctr_1718_raw <- read_csv(url(ctr_1718_raw_loc), col_names = FALSE)
ctr_1617_raw <- read_csv(url(ctr_1617_raw_loc), col_names = FALSE)
ctr_1516_raw <- read_csv(url(ctr_1516_raw_loc), col_names = FALSE)

ctr_2223_raw[1, 29] <- 'NULL'                       #remove problematic cell contents 
ctr_2122_raw[1, 29] <- 'NULL'                       #remove problematisc cell contents 

#define the column names using row one because read_csv was being a problem 
ctr_2223_raw <- ctr_2223_raw |>
    row_to_names(row_number = 1)
ctr_2122_raw <- ctr_2122_raw |>
    row_to_names(row_number = 1)
ctr_2021_raw <- ctr_2021_raw |>
    row_to_names(row_number = 1)
ctr_1920_raw <- ctr_1920_raw |>
    row_to_names(row_number = 1)
ctr_1819_raw <- ctr_1819_raw |>
    row_to_names(row_number = 1)
ctr_1718_raw <- ctr_1718_raw |>
    row_to_names(row_number = 1)
ctr_1617_raw <- ctr_1617_raw |>
    row_to_names(row_number = 1)
ctr_1516_raw <- ctr_1516_raw |>
    row_to_names(row_number=1)

#clean the data and selet the area band d column and the tstb
clean_ctr('ctr_2223_expp', ctr_2223_raw, '2223', 'x17_average_band_d_2_adult_equivalent_council_tax_for_area_of_the_billing_authority_including_both_local_and_major_precepts_lines_8_16_a_16_b_16_c_16_d', 
          'x22')
clean_ctr('ctr_2122_expp', ctr_2122_raw, '2122', 'x17_average_band_d_2_adult_equivalent_council_tax_for_area_of_the_billing_authority_including_both_local_and_major_precepts_lines_8_16_a_16_b_16_c_16_d',
          'x22')
clean_ctr('ctr_2021_expp', ctr_2021_raw, '2021', 'x17_average_band_d_2_adult_equivalent_council_tax_for_area_of_the_billing_authority_including_both_local_and_major_precepts_lines_8_16_a_16_b_16_c_16_d',
          'x20')

clean_ctr_2('ctr_1920_expp', ctr_1920_raw, '1920', 'x17_average_band_d_2_adult_equivalent_council_tax_for_area_of_the_billing_authority_including_both_local_and_major_precepts_lines_8_16_a_16_b_16_c_16_d',
            'x20')
clean_ctr_2('ctr_1819_expp', ctr_1819_raw, '1819', 'x15_average_band_d_2_adult_equivalent_council_tax_for_area_of_the_billing_authority_including_both_local_and_major_precepts_lines_8_14_a_14_b_14_c_14_d',
             'x20')
clean_ctr_2('ctr_1718_expp', ctr_1718_raw, '1718', 'x15_average_band_d_2_adult_equivalent_council_tax_for_area_of_the_billing_authority_including_both_local_and_major_precepts_lines_8_14_a_14_b_14_c_14_d',
             'x20')
clean_ctr_2('ctr_1617_expp', ctr_1617_raw, '1617', 'x15_average_band_d_2_adult_equivalent_council_tax_for_area_of_the_billing_authority_including_both_local_and_major_precepts_lines_10_14_a_14_b_14_c', 
             'x20')
clean_ctr_2('ctr_1516_expp', ctr_1516_raw, '1516', 'x15_average_band_d_2_adult_equivalent_council_tax_for_area_of_the_billing_authority_including_both_local_and_major_precepts_lines_10_14_a_14_b_14_c',
             'x22' )

###########################################################################
#### multiply the tstb by the area band d to calculate and area ctr ######
###########################################################################
ctr_2223_expp <- ctr_2223_expp |>
    mutate(ctr_223 = "area_bandd_2223"*"tstb_2223")
ctr_2223_expp <- ctr_2223_expp |>
    mutate(area_bandd_2223 = as.numeric(area_bandd_2223)) |>
    mutate(tstb_2223 = as.numeric(tstb_2223))
ctr_2122_expp <- ctr_2122_expp |>
    mutate(area_bandd_2122 = as.numeric(area_bandd_2122)) |

numeric_multiply(ctr_2223_expp, ctr_2223_expp$area_bandd_2223, ctr_2223_expp$tstb_2223, '2223', area_bandd_2223, tstb_2223, 'ctr_test')
view(ctr_test)

numeric(ctr_2223_expp, ctr_2223_expp$area_bandd_2223, ctr_2223_expp$tstb_2223, '2223', 'ctr_2223_expp')
multiply(ctr_2223_expp, ctr_test$area_bandd_2223, ctr_test$tstb_2223, '2223', 'ctr_2223_expp')
numeric(ctr_2122_expp, ctr_2122_expp$area_bandd_2122, ctr_2122_expp$tstb_2122, '2122', 'ctr_2122_expp')
multiply(ctr_2122_expp, ctr_test$area_bandd_2122, ctr_test$tstb_2122, '2122', 'ctr_2122_expp')
numeric(ctr_2021_expp, ctr_2021_expp$area_bandd_2021, ctr_2021_expp$tstb_2021, '2021', 'ctr_2021_expp')
multiply(ctr_2021_expp, ctr_test$area_bandd_2021, ctr_test$tstb_2021, '2021', 'ctr_2021_expp')
numeric(ctr_1920_expp, ctr_1920_expp$area_bandd_1920, ctr_1920_expp$tstb_1920, '1920', 'ctr_1920_expp')
multiply(ctr_1920_expp, ctr_test$area_bandd_1920, ctr_test$tstb_1920, '1920', 'ctr_1920_expp')
numeric(ctr_1819_expp, ctr_1819_expp$area_bandd_1819, ctr_1819_expp$tstb_1819, '1819', 'ctr_1819_expp')
multiply(ctr_1819_expp, ctr_test$area_bandd_1819, ctr_test$tstb_1819, '1819', 'ctr_1819_expp')
numeric(ctr_1718_expp, ctr_1718_expp$area_bandd_1718, ctr_1718_expp$tstb_1718, '1718', 'ctr_1718_expp')
multiply(ctr_1718_expp, ctr_test$area_bandd_1718, ctr_test$tstb_1718, '1718', 'ctr_1718_expp')
numeric(ctr_1617_expp, ctr_1617_expp$area_bandd_1617, ctr_1617_expp$tstb_1617, '1617', 'ctr_1617_expp')
multiply(ctr_1617_expp, ctr_test$area_bandd_1617, ctr_test$tstb_1617, '1617', 'ctr_1617_expp')
numeric(ctr_1516_expp, ctr_1516_expp$area_bandd_1516, ctr_1516_expp$tstb_1516, '1516', 'ctr_1516_expp')
multiply(ctr_1516_expp, ctr_test$area_bandd_1516, ctr_test$tstb_1516, '1516', 'ctr_1516_expp')




rm(ctr_test)
#function to force tstb and andd area to be a number
numeric <- function(input, area_bandd, tstb, yr, output) {
    b <- input |>
        mutate(!! paste0("area_bandd_", yr) := as.numeric(area_bandd)) |>
        mutate(!! paste0("tstb_", yr) := as.numeric(tstb)) 

     return(assign(output, b, envir = parent.frame()))  
}

#multiple area_band d by tstb to get ctr
multiply <- function(input, area_bandd, tstb, yr, output) {
    b <- input |>
        mutate( !! paste0("ctr_", yr) := area_bandd *  tstb)
     return(assign(output, b, envir = parent.frame())) 
}

#next need to reading in number of dwellings on the valuation list 



view(ctr_areabandd_2223)
ctr_areabandd_2122$x17_average_band_d_2_adult_equivalent_council_tax_for_area_of_the_billing_authority_including_both_local_and_major_precepts_lines_8_16_a_16_b_16_c_16_d
ctr_areabandd_2122 <- ctr_2122_raw|>
    clean_names()


ctr_areabandd_2223$x17_average_band_d_2_adult_equivalent_council_tax_for_area_of_the_billing_authority_including_both_local_and_major_precepts_lines_8_16_a_16_b_16_c_16_d
ctr_areabandd_2223 <- ctr_2223_raw |>
    clean_names()

ctr_areabandd_2021 <- ctr_2021_raw |>
    clean_names()
ctr_areabandd_2021$x17_average_band_d_2_adult_equivalent_council_tax_for_area_of_the_billing_authority_including_both_local_and_major_precepts_lines_8_16_a_16_b_16_c_16_d

#function to select area band D (21/22 to 22/23)
ctr_area <- function(output, raw_input, area_cy) {
    a <- raw_input |>
    clean_names()

    a <- a |>
    dplyr::rename(!! paste0("area_", area_cy) := x17_average_band_d_2_adult_equivalent_council_tax_for_area_of_the_billing_authority_including_both_local_and_major_precepts_lines_8_16_a_16_b_16_c_16_d) |>
    select(ecode:class, paste0("area_", area_cy), ons_code) |>
    relocate(ons_code) |>
    filter(ecode != "NA")
    return(assign(output, a, envir = parent.frame()))
}

ctr_area('ctr_area_2223', ctr_2223_raw, '2223')
