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
clean_ctr <- function(output, raw_input, tstb_cy) {
    a <- raw_input |>
    clean_names()

    a <- a |>
    dplyr::rename(!! paste0("tstb_", tstb_cy) := x22) |>
    select(ecode:class, paste0("tstb_", tstb_cy), ons_code) |>
    relocate(ons_code) |>
    filter(ecode != "NA")
    return(assign(output, a, envir = parent.frame()))

}

#function to clean ctrs and select relevent columns (16/17 to 20/21)
clean_ctr_2 <- function(output, raw_input, tstb_cy) {    
    a <- raw_input |>
    clean_names()

    a <- a |>
    dplyr::rename( !! paste0("tstb_", tstb_cy) := x20) |>
    select(ecode:class, paste0("tstb_", tstb_cy)) |>
    filter(ecode != "NA")
    return(assign(output, a, envir = parent.frame()))
    
}

#function to clean ctrs and select relevent columns (15/16)
clean_ctr_3 <- function(output, raw_input, tstb_cy) {
    a <- raw_input |>
        clean_names()

    a <- a |>
    dplyr::rename(!! paste0("tstb_", tstb_cy) := x22) |>
    select(ecode:class, paste0("tstb_", tstb_cy)) |>
    filter(ecode != "NA")
    return(assign(output, a, envir = parent.frame()))
}



#call function 
clean_ctr("ctr_2223", ctr_2223_raw, "2223")

clean_ctr("ctr_2122", ctr_2122_raw, "2122")

#call second function due to change in the number of columns (no ONS codes)
clean_ctr_2("ctr_2021", ctr_2021_raw, "2021")

clean_ctr_2("ctr_1920", ctr_1920_raw, "1920")

clean_ctr_2("ctr_1819", ctr_1819_raw, "1819")

clean_ctr_2("ctr_1718", ctr_1718_raw, "1718")

clean_ctr_2("ctr_1617", ctr_1617_raw, "1617")

#call third function due to changes in column names and number of columns
clean_ctr_3("ctr_1516", ctr_1516_raw, "1516")

#merge all the historic data frames together
ctr_historic <- ctr |>
  select(ecode:class, tstb_2324) |>
  inner_join(ctr_1516, by = "ecode")|>
  inner_join(ctr_1617, by = "ecode") |>
  inner_join(ctr_1718, by = "ecode") |>
  inner_join(ctr_1819, by = "ecode") |>
  inner_join(ctr_1920, by = "ecode") |>
  inner_join(ctr_2021, by = "ecode") |>
  inner_join(ctr_2122, by = "ecode") |>
  inner_join(ctr_2223, by = "ecode") |>
  relocate(tstb_2324, .after = last_col())

  
#tidy up the datat frame
ctr_historic <- ctr_historic |>
  select(ecode:class.x, contains("tstb")) |>
  filter(!class.x %in% c("PCC", "MF", "CFA", "CA", "SC", "GLA", "[z]")) |>
  filter(!region %in% c("Eng", "[z]"))

ctr_historic <- ctr_historic |>
  mutate_at(c("tstb_1516", "tstb_1617", "tstb_1718", "tstb_1819", "tstb_1920", "tstb_2021", "tstb_2122", "tstb_2223"), 
            as.numeric) 
write.csv(ctr_historic, "D:\\Users\\emily.keenan\\OneDrive - MHCLG\\Documents\\GitHub\\council-tax\\output\\ctr_historic.csv")



view(ctr_2223)
view(ctr_2122)
view (ctr_2021)
view(ctr_1920)
view(ctr_1819)
view(ctr_1718)
view(ctr_1617)
view(ctr_1516)

