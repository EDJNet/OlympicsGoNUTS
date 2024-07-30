get_mismatch_ranking_in_wikidata <- function(all_medalists_df) {
  ## Placement from Wikidata ####
  
  medalists_l <- all_medalists_df |> 
    dplyr::select(medalist_wikidata_id, medal, event_wikidata_id, delegation_wikidata_id) |> 
    purrr::transpose()
  
  #current_medalist <- medalists_l[[4]]
  
  check_df <- purrr::map(
    .x = medalists_l,
    .f = \(current_medalist) {
      current_event <- tw_get_qualifiers(id = current_medalist[["medalist_wikidata_id"]],
                                         p = "P1344") |> 
        dplyr::filter(qualifier_id == current_medalist[["event_wikidata_id"]])
      
      current_ranking <- current_event |> 
        dplyr::filter(qualifier_property == "P1352") |> 
        dplyr::transmute(ranking = as.integer(stringr::str_remove(qualifier_value, stringr::fixed("+")))) |> 
        dplyr::pull(ranking)
      
      if (length(current_ranking)>1) {
        ## keep only final
        final_set <- current_event |> 
          dplyr::filter(qualifier_property == "P2443"&qualifier_value=="Q1366722") |> 
          dplyr::pull(set)
        
        current_ranking <- current_event |> 
          dplyr::filter(set == final_set) |> 
          dplyr::filter(qualifier_property == "P1352") |> 
          dplyr::transmute(ranking = as.integer(stringr::str_remove(qualifier_value, stringr::fixed("+")))) |> 
          dplyr::pull(ranking)
      }
      
      if (length(current_ranking)==0) {
        return(NULL)
      } 
      
      tibble::tibble(medalist_wikidata_id = current_medalist[["medalist_wikidata_id"]], 
                     event_wikidata_id = current_medalist[["event_wikidata_id"]],
                     medal_from_wikipedia = current_medalist[["medal"]]) |> 
        dplyr::mutate(ranking_from_wikipedia = dplyr::case_when(medal_from_wikipedia == "gold" ~ 1L, 
                                                                medal_from_wikipedia == "silver" ~ 2L,
                                                                medal_from_wikipedia == "bronze" ~ 3L)) |> 
        dplyr::mutate(ranking_according_to_wikidata = current_ranking) |> 
        dplyr::mutate(check = ranking_according_to_wikidata == ranking_from_wikipedia)
    }) |> 
    purrr::list_rbind()
  
  mismatch_df <- check_df |> 
    dplyr::filter(!check)
}

#mismatch_df <- get_mismatch_ranking_in_wikidata(all_medalists_df)

fix_mismatch_ranking_in_wikidata <- function(all_medalists_df, mismatch_df) {
  
  mismatch_l <- purrr::transpose(mismatch_df)
  
  for (i in seq_along(mismatch_l)) {
    current_mismatch <- mismatch_l[[i]]
    
    correct_medal <- dplyr::case_when(current_mismatch[["ranking_according_to_wikidata"]] == 1L ~ "gold",
                                      current_mismatch[["ranking_according_to_wikidata"]] == 2L ~ "silver",
                                      current_mismatch[["ranking_according_to_wikidata"]] == 3L ~ "bronze"
    )
    all_medalists_df$medal[all_medalists_df$medalist_wikidata_id==current_mismatch$medalist_wikidata_id&all_medalists_df$event_wikidata_id==current_mismatch$event_wikidata_id] <- correct_medal
  } 
  
  all_medalists_df
}