o24_export_datasets <- function(all_medalists_wd_nuts_pop_df, olympics_year) {
  
  fs::dir_create(here::here(olympics_year))
  
  all_medalists_wd_nuts_pop_df %>% 
    readr::write_csv(file = here::here(olympics_year, stringr::str_c(olympics_year, "_medalists_all.csv")))
  
  all_medalists_wd_nuts_pop_df %>%
    dplyr::filter(is.na(nuts0_id)==FALSE) %>% 
    readr::write_csv(file = here::here(olympics_year, stringr::str_c(olympics_year, "_medalists_nuts_only.csv")))
  
  all_medalists_wd_nuts_pop_df %>%
    dplyr::filter(is.na(lat)==TRUE) %>% 
    dplyr::select(event_name, medal, medalist_name, medalist_wikidata_id, country_medal, delegation_name, sex_or_gender) %>% 
    readr::write_csv(file = here::here(olympics_year, stringr::str_c(olympics_year, "_medalists_missing_place_of_birth.csv")))
  
  
  all_medalists_wd_nuts_df |> 
    dplyr::filter(country_medal_NUTS_code %in% nuts_countries_v, 
                  is.na(lat)) |> 
    dplyr::select(event_name, medal, medalist_name, medalist_wikidata_id, country_medal, delegation_name, sex_or_gender) %>% 
    readr::write_csv(file = here::here(olympics_year, stringr::str_c(olympics_year, "_medalists_missing_place_of_birth_from_NUTS_countries.csv")))
  
  all_medalists_wd_nuts_pop_df %>% 
    dplyr::filter(is.na(nuts2_name)==FALSE) %>% 
    dplyr::group_by(nuts2_name) %>% 
    dplyr::count(name = "medals", sort = TRUE) %>% 
    dplyr::ungroup() %>% 
    dplyr::left_join(y = all_medalists_wd_nuts_pop_df %>% 
                       dplyr::distinct(nuts2_id, nuts2_name, nuts2_population, nuts0_name), by = "nuts2_name") %>% 
    dplyr::mutate(medals_per_million_residents = 1e06/(nuts2_population/medals)) %>% 
    dplyr::arrange(dplyr::desc(medals_per_million_residents)) %>% 
    dplyr::transmute(nuts2_id, 
                     nuts2_name,
                     country = nuts0_name,
                     medals, 
                     nuts2_population,
                     medals_per_million_residents) %>% 
    readr::write_csv(file = here::here(olympics_year, stringr::str_c(olympics_year, "_medals_per_million_residents_in_nuts2.csv")))
}
