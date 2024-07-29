o24_join_nuts <- function(all_medalists_wd_df) {
  all_medalists_wd_df %>% 
    dplyr::left_join(
      all_medalists_wd_df %>% 
        dplyr::filter(is.na(lon)==FALSE) %>% 
        dplyr::distinct(medalist_wikidata_id, .keep_all = TRUE) %>% 
        sf::st_as_sf(coords = c("lon","lat"), crs = 4326) %>% 
        sf::st_join(nuts1_2024_sf,
                    join = sf::st_intersects) %>% 
        st_drop_geometry() %>% 
        dplyr::transmute(medalist_wikidata_id,
                         nuts1_id = NUTS_ID,
                         nuts1_name = NAME_LATN),
      by = "medalist_wikidata_id") %>% 
    dplyr::left_join(
      all_medalists_wd_df %>% 
        dplyr::filter(is.na(lon)==FALSE) %>% 
        dplyr::distinct(medalist_wikidata_id, .keep_all = TRUE) %>% 
        sf::st_as_sf(coords = c("lon","lat"), crs = 4326) %>% 
        sf::st_join(nuts2_2024_sf,
                    join = sf::st_intersects) %>% 
        st_drop_geometry() %>% 
        dplyr::transmute(medalist_wikidata_id,
                         nuts2_id = NUTS_ID,
                         nuts2_name = NAME_LATN),
      by = "medalist_wikidata_id") %>% 
    dplyr::left_join(
      all_medalists_wd_df %>% 
        dplyr::filter(is.na(lon)==FALSE) %>% 
        dplyr::distinct(medalist_wikidata_id, .keep_all = TRUE) %>% 
        sf::st_as_sf(coords = c("lon","lat"), crs = 4326) %>% 
        sf::st_join(nuts3_2024_sf,
                    join = sf::st_intersects) %>% 
        st_drop_geometry() %>% 
        dplyr::transmute(medalist_wikidata_id,
                         nuts3_id = NUTS_ID,
                         nuts3_name = NAME_LATN),
      by = "medalist_wikidata_id") 
  
}


o24_join_nuts_stats <- function(all_medalists_wd_nuts_df) {
  
  all_medalists_wd_nuts_pop_df <- all_medalists_wd_nuts_df %>% 
    dplyr::left_join(y = population_by_nuts2, by = "nuts2_id") %>% 
    dplyr::left_join(y = population_by_nuts3, by = "nuts3_id") %>% 
    dplyr::left_join(y = gdp_by_nuts2_2021_df, by = "nuts2_id") %>% 
    dplyr::left_join(y = gdp_by_nuts3_2021_df, by = "nuts3_id") %>% 
    dplyr::mutate(nuts0_id = stringr::str_extract(string = nuts2_id, pattern = "[A-Z]{2}")) %>% 
    dplyr::mutate(nuts0_name = dplyr::case_when(is.na(nuts0_id) ~ as.character(NA),
                                                nuts0_id == "EL" ~ "Greece", 
                                                nuts0_id == "UK" ~ "United Kingdom", 
                                                nuts0_id == "XK" ~ "Kosovo", 
                                                TRUE ~ countrycode::countrycode(sourcevar = nuts0_id,
                                                                                origin = "iso2c",
                                                                                destination = "country.name")))
  
  all_medalists_wd_nuts_pop_df
  
}
