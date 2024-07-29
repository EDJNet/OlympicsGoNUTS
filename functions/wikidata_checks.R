library("here")
source(here::here("functions", "setup.R"))
source(here::here("functions", "get_list_of_lists.R"))

olympics_year <- 2024

list_of_lists_df |> 
  dplyr::filter(year == olympics_year) |> 
  dplyr::pull(qid)

summer_olympics_qid <- "Q159821"

all_summer_olympics_df <- tw_get_property(id = "Q159821", p = "P527") |> 
  dplyr::transmute(olympics_qid = value) |> 
  dplyr::mutate(olympics_event_label = tw_get_label(olympics_qid)) |> 
  dplyr::mutate(year = as.numeric(stringr::str_extract(string = olympics_event_label, pattern = "[[:digit:]]{4}"))) |> 
  dplyr::filter(year <= lubridate::year(Sys.Date()))

all_olympics_events_df <- tw_get_property(id = all_summer_olympics_df[["olympics_qid"]], p = "P527") |> 
  dplyr::rename(olympics_qid = id) |> 
  dplyr::select(olympics_qid, value) |> 
  dplyr::left_join(y = all_summer_olympics_df |> 
                     dplyr::select(olympics_qid, year),
                   by = "olympics_qid") |> 
  dplyr::transmute(year, 
                   olympics_qid, 
                   olympics_label = tw_get_label(olympics_qid), 
                   olympics_event_qid = value, 
                   olympics_event_label = tw_get_label(olympics_event_qid)) |> 
  dplyr::mutate(sport_qid = tw_get_p1(olympics_event_qid, p = "P641")) |> 
  dplyr::mutate(sport_label = tw_get_label(id = sport_qid)) |> 
  dplyr::mutate(olympics_specific_event_qid = tw_get_p(id = olympics_event_qid, 
                                                       p = "P527")) |> 
  tidyr::unnest(olympics_specific_event_qid) |> 
  dplyr::mutate(olympics_specific_event_label = tw_get_label(olympics_specific_event_qid))



all_olympics_participants_df <- all_olympics_events_df[["olympics_event_qid"]] |> 
  tw_get_property(p = "P1344")



# tennis_df <- tw_query(query = list(
#   p = "P1344", q = "Q39080770"
# ))

tw_get_property(id = "Q109311615", p = "P1344", cache = FALSE)
