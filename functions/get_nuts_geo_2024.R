fs::dir_create(here::here("eurostat"))
nuts_zip_path <- here::here("eurostat", "ref-nuts-2024-20m.geojson.zip")

if (fs::file_exists(nuts_zip_path)==FALSE) {
  download.file(url = "https://gisco-services.ec.europa.eu/distribution/v2/nuts/download/ref-nuts-2024-20m.geojson.zip", destfile = nuts_zip_path, method = "libcurl")
  unzip(zipfile = nuts_zip_path, exdir = here::here("eurostat", "nuts_geojson"))
}

# nuts_df <- readr::read_csv(file = here::here("eurostat", "nuts_geojson", "NUTS_AT_2024.csv"), show_col_types = FALSE)

nuts1_2024_sf <- sf::st_read(dsn = here::here("eurostat", "nuts_geojson", "NUTS_RG_20M_2024_4326_LEVL_1.geojson"), quiet = TRUE)
nuts2_2024_sf <- sf::st_read(dsn = here::here("eurostat", "nuts_geojson", "NUTS_RG_20M_2024_4326_LEVL_2.geojson"), quiet = TRUE)
nuts3_2024_sf <- sf::st_read(dsn = here::here("eurostat", "nuts_geojson", "NUTS_RG_20M_2024_4326_LEVL_3.geojson"), quiet = TRUE)



population_by_nuts2_2024_file <- here::here("eurostat",
                                            "population_by_nuts2_2024.csv")

if (fs::file_exists(population_by_nuts2_2024_file)==FALSE) {
  eurostat::get_eurostat(id = "tgs00096", filters = list(age = "TOTAL",
                                                         sex = "T")) %>% 
    dplyr::filter(is.na(values)==FALSE) %>% 
    dplyr::arrange(geo, dplyr::desc(time)) %>% 
    
    dplyr::group_by(geo) %>% 
    dplyr::slice(1) %>% 
    dplyr::ungroup() %>% 
    tidyr::drop_na() %>% 
    dplyr::transmute(nuts2_id = geo, nuts2_population = values) %>% 
    dplyr::filter(nuts2_population!=0) %>% 
    readr::write_csv(population_by_nuts2_2024_file)
}

population_by_nuts2 <- readr::read_csv(file = population_by_nuts2_2024_file, show_col_types = FALSE)


population_by_nuts3_2024_file <- here::here("eurostat",
                                            "population_by_nuts3_2024.csv")

if (fs::file_exists(population_by_nuts3_2024_file)==FALSE) {
  eurostat::get_eurostat(id = "demo_r_pjanaggr3",
                         filters = list(age = "TOTAL",
                                        sex = "T")) %>% 
    dplyr::arrange(geo, dplyr::desc(time)) %>% 
    dplyr::filter(is.na(values)==FALSE) %>% 
    dplyr::group_by(geo) %>% 
    dplyr::slice(1) %>% 
    dplyr::ungroup() %>% 
    tidyr::drop_na() %>% 
    dplyr::transmute(nuts3_id = geo, nuts3_population = values) %>% 
    dplyr::filter(nuts3_population!=0) %>% 
    dplyr::filter(nchar(nuts3_id)==5) %>% 
    readr::write_csv(population_by_nuts3_2024_file)
}

population_by_nuts3 <- readr::read_csv(file = population_by_nuts3_2024_file, show_col_types = FALSE)


if (fs::file_exists(here::here("eurostat",
                               "gdp_by_nuts3_2024.csv"))==FALSE) {
  
  gdp_by_nuts_df <- eurostat::get_eurostat(id = "nama_10r_3gdp",
                                           filters = list(unit = "MIO_EUR"))  %>%
    dplyr::arrange(geo, dplyr::desc(time)) %>% 
    dplyr::filter(is.na(values)==FALSE) %>% 
    dplyr::group_by(geo) %>% 
    dplyr::slice(1) %>% 
    dplyr::ungroup() %>% 
    tidyr::drop_na() %>% 
    dplyr::filter(values !=0) 
  
  
  gdp_by_nuts_df %>% 
    dplyr::transmute(nuts2_id = geo, nuts2_gdp = values) %>% 
    dplyr::filter(nchar(nuts2_id)==4) %>% 
    readr::write_csv(file = here::here("eurostat",
                                       "gdp_by_nuts2_2024.csv"))
  
  gdp_by_nuts_df %>% 
    dplyr::transmute(nuts3_id = geo, nuts3_gdp = values) %>% 
    dplyr::filter(nchar(nuts3_id)==5) %>%
    readr::write_csv(file = here::here("eurostat",
                                       "gdp_by_nuts3_2024.csv"))
  
  
}

gdp_by_nuts2_2024_df <- readr::read_csv(here::here("eurostat",
                                                   "gdp_by_nuts2_2024.csv"),
                                        show_col_types = FALSE)

gdp_by_nuts3_2024_df <- readr::read_csv(here::here("eurostat",
                                                   "gdp_by_nuts3_2024.csv"),
                                        show_col_types = FALSE)


nuts_countries_2024_v <- nuts2_2024_sf |> 
  sf::st_drop_geometry() |> 
  dplyr::distinct(CNTR_CODE) |> 
  dplyr::pull(CNTR_CODE)