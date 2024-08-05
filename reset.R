yearly_folders <- fs::dir_info() |> 
  dplyr::filter(stringr::str_starts(path, "[[:digit:]]{4}")) |> 
  dplyr::pull(path)

fs::dir_delete(yearly_folders)

#fs::dir_delete("medalists_pages")
fs::dir_delete("medalists_csv")

fs::file_delete(path = here::here("medalists_pages", "medalists_2024.html"))
fs::file_delete(path = here::here("medalists_csv", "medalists_2024.csv"))

library("here")
source(here::here("functions", "setup.R"))
source(here::here("functions", "extract_medals_from_wikipedia_page.R"))
source(here::here("functions", "get_list_of_lists.R"))


current_html <- o24_read_html(olympics_year = 2024,
                              list_of_lists_df = list_of_lists_df)


all_medalists_df <- o24_get_all_medalists(current_html, 2024) 

tw_get(all_medalists_df[["medalist_wikidata_id"]], overwrite_cache = TRUE)

tw_get(all_medalists_df[["event_wikidata_id"]], overwrite_cache = TRUE)
