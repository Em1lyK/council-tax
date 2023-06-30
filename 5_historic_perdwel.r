#plot the average difference between the historic ct per dwelling of local authortiies 
#Emily Keenan
#30/06/23

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
