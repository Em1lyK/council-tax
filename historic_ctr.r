#script to read in historic ctr files
#11/05/2023


setwd("D:\\Users\\emily.keenan\\OneDrive - MHCLG\\Documents\\GitHub\\council-tax")

ctr_2223_loc <- paste0("input\\ctr_2223.xlsx")
ctr_2122_loc <- paste0("input\\ctr_2122.xlsx")

ctr_2223_raw <- readxl::read_excel(ctr_2223_loc, sheet = "Data", skip = 3)
ctr_2122_raw <- readxl::read_excel(ctr_2122_loc, sheet = "Data", skip = 3)

clean_ctr <- function(output, raw_input, tstb_pv, tstb_cy) {
    output <- raw_input |>
    clean_names()

    output <<- output |>
    rename(tstb_pv =  x7_council_tax_base_for_council_tax_setting_purposes, tstb_cy = x22)|>
    select(ecode:class, tstb_pv:tstb_cy, ons_code) |>
    relocate(ons_code) |>
    filter(ecode != "NA")


}

ctr_2223 <- data.frame()
clean_ctr(ctr_2223, ctr_2223_raw, tstb_2122, tstb_2223)

ctr_2223 <- ctr_2223_raw |>
 clean_names() 
 
 ctr_2223 <- ctr_2223|>
    rename(tstb_2122 = x7_council_tax_base_for_council_tax_setting_purposes, tstb_2223 = x22) |>
    select(ecode:class, tstb_2122:tstb_2223, ons_code)|>
    relocate(ons_code) |>
    filter(ecode != "NA")

ctr_2122 <- ctr_2122_raw |>
 clean_names() 
 
 ctr_2122 <- ctr_2122|>
    rename(tstb_2021 = x7_council_tax_base_for_council_tax_setting_purposes, tstb_2122 = x22) |>
    select(ecode:class, tstb_2021:tstb_2122, ons_code)|>
    relocate(ons_code) |>
    filter(ecode != "NA")





view(ctr_2223)
