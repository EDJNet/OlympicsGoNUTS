invisible(source(here::here("functions", "get_nuts_geo_2021.R")))
invisible(source(here::here("functions", "get_nuts_geo_2024.R")))

nuts_countries_v <- unique(nuts_countries_2021_v, nuts_countries_2024_v)

countries_from_2021_v  <- c(nuts_countries_2021_v[!(nuts_countries_2021_v %in% nuts_countries_2024_v)],
                            "PT", 
                            "NL")

countries_from_2024_v <- c(nuts_countries_2024_v[!(nuts_countries_2024_v %in% countries_from_2021_v)])


nuts1_combo_sf <- dplyr::bind_rows(
  nuts1_2021_sf |> 
    dplyr::mutate(MOUNT_TYPE = as.character(MOUNT_TYPE), URBN_TYPE = as.character(URBN_TYPE), COAST_TYPE = as.character(COAST_TYPE)) |> 
    dplyr::filter(CNTR_CODE %in% countries_from_2021_v),
  nuts1_2024_sf |> 
    dplyr::select(-MOUNT_TYPE) |> 
    dplyr::filter(CNTR_CODE %in% countries_from_2024_v)
)

nuts2_combo_sf <- dplyr::bind_rows(
  nuts2_2021_sf |> 
    dplyr::mutate(MOUNT_TYPE = as.character(MOUNT_TYPE), URBN_TYPE = as.character(URBN_TYPE), COAST_TYPE = as.character(COAST_TYPE)) |> 
    dplyr::filter(CNTR_CODE %in% countries_from_2021_v),
  nuts2_2024_sf |> 
    dplyr::select(-MOUNT_TYPE) |> 
    dplyr::filter(CNTR_CODE %in% countries_from_2024_v)
)

nuts3_combo_sf <- dplyr::bind_rows(
  nuts3_2021_sf |> 
    dplyr::mutate(MOUNT_TYPE = as.character(MOUNT_TYPE), URBN_TYPE = as.character(URBN_TYPE), COAST_TYPE = as.character(COAST_TYPE)) |> 
    dplyr::filter(CNTR_CODE %in% countries_from_2021_v),
  nuts3_2024_sf |> 
    dplyr::select(-MOUNT_TYPE) |> 
    dplyr::filter(CNTR_CODE %in% countries_from_2024_v)
)