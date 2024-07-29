library("here")
source(here::here("functions", "setup.R"))

query <- list(
  c(p = "P31", q = "Q5"),
  c(p = "P1344", q = "Q25991447")
)

all_df <- tw_query(query)

qual_df <- tw_get_qualifiers(id = all_df[["id"]], p = "P1344")

qual_df |> 
  dplyr::filter(qualifier_id == "Q25991447", 
                qualifier_property == "P1352") |> 
  dplyr::mutate(qualifier_value = as.numeric(stringr::str_remove(qualifier_value, stringr::fixed("+")))) |> 
  dplyr::arrange(qualifier_value) |> 
  dplyr::select(id, qualifier_value) |> 
  dplyr::mutate(name = tw_get_label(id))
