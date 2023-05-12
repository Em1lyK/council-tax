#script to read in historic ctr files
#11/05/2023

#set working dir
setwd("D:\\Users\\emily.keenan\\OneDrive - MHCLG\\Documents\\GitHub\\council-tax")

#location of historic ctrs
ctr_2223_loc <- paste0("input\\ctr_2223.xlsx")
ctr_2122_loc <- paste0("input\\ctr_2122.xlsx")
ctr_2021_loc <- paste0("input\\ctr_2021.xlsx")
ctr_1920_loc <- paste0("input\\ctr_1920.xlsx")
ctr_1819_loc <- paste0("input\\ctr_1819.xlsx")
ctr_1718_loc <- paste0("input\\ctr_1718.xlsx")
ctr_1617_loc <- paste0("input\\ctr_1617.xlsx")
ctr_1516_loc <- paste0("input\\ctr_1516.xlsx")


#read in historic ctr excel sheets 
ctr_2223_raw <- readxl::read_excel(ctr_2223_loc, sheet = "Data", skip = 3)
ctr_2122_raw <- readxl::read_excel(ctr_2122_loc, sheet = "Data", skip = 3)
ctr_2021_raw <- readxl::read_excel(ctr_2021_loc, sheet = "Data", skip = 3)
ctr_1920_raw <- readxl::read_excel(ctr_1920_loc, sheet = "Data", skip = 3)
ctr_1819_raw <- readxl::read_excel(ctr_1819_loc, sheet = "Data", skip = 3)
ctr_1718_raw <- readxl::read_excel(ctr_1718_loc, sheet = "Data", skip = 3)
ctr_1617_raw <- readxl::read_excel(ctr_1617_loc, sheet = "Data", skip = 3)
ctr_1516_raw <- readxl::read_excel(ctr_1516_loc, sheet = "Data", skip = 3)

#function to clean ctrs and select relevent columns (21/22 to 22/23)
clean_ctr <- function(output, raw_input, tstb_pv, tstb_cy) {
    output <- raw_input |>
    clean_names()

    output <<- output |>
    rename(tstb_pv =  x7_council_tax_base_for_council_tax_setting_purposes, tstb_cy = x22)|>
    select(ecode:class, tstb_pv:tstb_cy, ons_code) |>
    relocate(ons_code) |>
    filter(ecode != "NA")
    return(output)

}

#function to clean ctrs and select relevent columns (16/17 to 20/21)
clean_ctr_2 <- function(output, raw_input, tstb_pv, tstb_cy) {
    output <- raw_input |>
    clean_names()

    output <<- output |>
    rename(tstb_pv =  x7_council_tax_base_for_council_tax_setting_purposes, tstb_cy = x20)|>
    select(ecode:class, tstb_pv:tstb_cy) |>
    filter(ecode != "NA")
    return(output)

}

#function to clean ctrs and select relevent columns (15/16)
clean_ctr_3 <- function(output, raw_input, tstb_pv, tstb_cy) {
    output <- raw_input |>
    clean_names()

    output <<- output |>
    rename(tstb_pv =  x8_council_tax_base_for_council_tax_setting_purposes_line_4_x_line_6_line_7, tstb_cy = x22)|>
    select(ecode:class, tstb_pv:tstb_cy) |>
    filter(ecode != "NA")
    return(output)

}

#call function 
clean_ctr(ctr_2223, ctr_2223_raw, tstb_2122, tstb_2223)
ctr_2223 <- output 

clean_ctr(ctr_2122, ctr_2122_raw, tstb_2021, tstb_2122)
ctr_2122 <- output

#call second function due to change in the number of columns (no ONS codes)
clean_ctr_2(ctr_2021, ctr_2021_raw, tstb_1920, tstb_2021)
ctr_2021 <- output

clean_ctr_2(ctr_1920, ctr_1920_raw, tstb_1819, tstb_1920)
ctr_1920 <- output

clean_ctr_2(ctr_1819, ctr_1819_raw, tstb_1718, tstb_1819)
ctr_1819 <- output

clean_ctr_2(ctr_1718, ctr_1718_raw, tstb_1617, tstb_1718)
ctr_1718 <- output

clean_ctr_2(ctr_1617, ctr_1617_raw, tstb_1516, tstb_1617)
ctr_1617 <- output

#call third function due to changes in column names and number of columns
clean_ctr_3(ctr_1516, ctr_1516_raw, tstb_1415, tstb_1516)
ctr_1516 <- output
