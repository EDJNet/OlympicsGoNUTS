list_of_lists_df <- tw_get_wikipedia_category_members(url = "https://en.wikipedia.org/wiki/Category:Lists_of_Summer_Olympic_medalists_by_year") |> 
  dplyr::select(title_url, qid) |> 
  dplyr::mutate(year = stringr::str_extract(string = title_url, pattern = "[[:digit:]]{4}")) |> 
  dplyr::mutate(url = tw_get_wikipedia(qid)) |> 
  dplyr::relocate(year, title_url, qid)