update_cache <- FALSE

source(here::here("functions", "setup.R"))
source(here::here("functions", "get_list_of_lists.R"))

olympics_year <- list_of_lists_df |> 
  dplyr::arrange(dplyr::desc(year)) |> 
  dplyr::slice_head(n = 1) |> 
  dplyr::pull(year)

## Delete previous files ####

fs::dir_delete(olympics_year)
fs::file_delete(path = here::here("medalists_pages", stringr::str_c("medalists_", olympics_year, ".html")))
fs::file_delete(path = here::here("medalists_csv", stringr::str_c("medalists_", olympics_year, ".csv")))

## Render html page and generate datasets ####

fs::dir_create(here::here(olympics_year))

quarto::quarto_render(
  input = here::here("medalists_by_year.qmd"), 
  output_file = "temp.html",
  execute_params = list(year = olympics_year),
  execute_dir = here::here()
)

fs::file_move(path = here::here("temp.html"),
              new_path = here::here(olympics_year, "index.html"))

## Update Wikidata cache ####

if (update_cache) {
  
  library("here")
  source(here::here("functions", "setup.R"))
  source(here::here("functions", "extract_medals_from_wikipedia_page.R"))
  source(here::here("functions", "get_list_of_lists.R"))
  source(here::here("functions", "check_ranking_in_wikidata.R"))
  
  current_html <- o24_read_html(olympics_year = olympics_year,
                                list_of_lists_df = list_of_lists_df)
  
  
  all_medalists_df <- o24_get_all_medalists(current_html, olympics_year) |> 
    o24_drop_non_humans()
  
  mismatch_df <- get_mismatch_ranking_in_wikidata(all_medalists_df)
  
  if (nrow(mismatch_df)>0){
    print(mismatch_df)
    cli::cli_abort("Mismatch between data exported from Wikipedia and those recorded in Wikidata, check!")
  }
  
  tw_get(all_medalists_df[["medalist_wikidata_id"]], overwrite_cache = TRUE)
  
  # tw_get(all_medalists_df[["event_wikidata_id"]], overwrite_cache = TRUE)
  
  #tw_get(id = "Q122904975", overwrite_cache = TRUE)
  
  
  
  
  
  # tw_get_qualifiers(id = "Q4661886",
  #                   p = "P1344", overwrite_cache = TRUE) 
  
  ## Medalists with medal not recorded in Wikidata
  
  # all_medalists_df |> 
  #   dplyr::anti_join(check_df,
  #                    by = c("medalist_wikidata_id",
  #                           "event_wikidata_id")) 
  
}