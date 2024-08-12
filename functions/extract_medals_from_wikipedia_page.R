o24_read_html <- function(olympics_year, list_of_lists_df, update = FALSE) {
  
  current_url <- list_of_lists_df |> 
    dplyr::filter(year == olympics_year) |> 
    dplyr::pull(url)
  
  fs::dir_create(here::here("medalists_pages"))
  
  current_file <- here::here("medalists_pages", stringr::str_c("medalists_", olympics_year, ".html"))
  
  if (update==TRUE|fs::file_exists(current_file)==FALSE) {
    download.file(url = URLencode(current_url),
                  destfile = current_file)
  }

  rvest::read_html(current_file)
}


o24_get_tables <- function(html) {
  html |> 
    rvest::html_nodes(css = "table")
}


o24_get_all_medalists <- function(html, olympics_year) {
  
  fs::dir_create(here::here("medalists_csv"))
  
  current_file <- here::here("medalists_csv",
                             stringr::str_c("medalists_", olympics_year, ".csv"))
  
  if (fs::file_exists(current_file) == FALSE) {
    tables <- o24_get_tables(html)
    
    all_medalists_df <- purrr::map(
      .x = tables,
      .f = function(table) {
        o24_get_medals_from_table(table)
      }) |> 
      purrr::list_rbind() |> 
      dplyr::filter(stringr::str_detect(string = medalist_link,pattern = "#", negate = TRUE)) |> 
      distinct(event_link, medalist_link, .keep_all = TRUE) |> 
      dplyr::mutate(medalist_wikidata_id = tw_get_wikipedia_page_qid(medalist_link) |> 
                      dplyr::pull(qid),
                    event_wikidata_id = tw_get_wikipedia_page_qid(event_link)|>
                      dplyr::pull(qid),
                    delegation_wikidata_id = tw_get_wikipedia_page_qid(delegation_link) |>
                      dplyr::pull(qid)) %>% 
      dplyr::mutate(event_link = paste0("https://en.wikipedia.org", event_link),
                    medalist_link = paste0("https://en.wikipedia.org", medalist_link),
                    delegation_link = paste0("https://en.wikipedia.org", delegation_link))
    
    readr::write_csv(x = all_medalists_df, file = current_file)
  }
  
  readr::read_csv(current_file)
}

o24_get_medalists_details <- function(all_medalists_wd_df) {
  
  all_medalists_wd_df %>% 
    mutate(medalist_name = tw_get_label(medalist_wikidata_id),
           place_of_birth_wikidata_id = tw_get_property_same_length(id = medalist_wikidata_id,
                                                                    p = "P19",
                                                                    only_first = TRUE,
                                                                    preferred = TRUE), 
           date_of_birth = tw_get_property_same_length(id = medalist_wikidata_id,
                                                       p = "P569",
                                                       only_first = TRUE,
                                                       preferred = TRUE), 
           event_sport_wikidata_id = tw_get_property_same_length(id = event_wikidata_id,
                                                                 p = "P641",
                                                                 only_first = TRUE,
                                                                 preferred = TRUE),
           event_part_of_wikidata_id = tw_get_property_same_length(id = event_wikidata_id,
                                                                   p = "P361",
                                                                   only_first = TRUE,
                                                                   preferred = TRUE),
           event_name = tw_get_label(event_wikidata_id),
           delegation_name = tw_get_label(delegation_wikidata_id), 
           sex_or_gender_wikidata_id = tw_get_property_same_length(id = medalist_wikidata_id,
                                                                   p = "P21",
                                                                   only_first = TRUE,
                                                                   preferred = TRUE)) %>% 
    
    
    mutate(country_medal_wikidata_id = tw_get_property_same_length(id = delegation_wikidata_id,
                                                                   p = "P17",
                                                                   only_first = TRUE, 
                                                                   preferred = TRUE),
           event_sport = tw_get_label(event_sport_wikidata_id),
           event_part_of = tw_get_label(event_part_of_wikidata_id),
           place_of_birth = tw_get_label(place_of_birth_wikidata_id),
           place_of_birth_located_in_wikidata_id = tw_get_property_same_length(id = place_of_birth_wikidata_id,
                                                                               p = "P131",
                                                                               only_first = TRUE,
                                                                               preferred = TRUE),
           place_of_birth_coordinates = tw_get_property_same_length(id = place_of_birth_wikidata_id,
                                                                    p = "P625",
                                                                    only_first = TRUE,
                                                                    preferred = TRUE),
           date_of_birth = stringr::str_extract(string = date_of_birth,
                                                pattern = "[[:digit:]]{4}-[[:digit:]]{2}-[[:digit:]]{2}") %>% 
             as.Date(),
           sex_or_gender = tw_get_label(sex_or_gender_wikidata_id)
    ) %>% 
    dplyr::mutate(event_part_of_sport_wikidata_id = tw_get_p1(event_part_of_wikidata_id, p = "P641")) |> 
    dplyr::mutate(event_part_of_sport = tw_get_label(event_part_of_sport_wikidata_id)) |> 
    dplyr::mutate(sport_wikidata_id = dplyr::if_else(is.na(event_sport_wikidata_id), 
                                                     event_part_of_sport_wikidata_id,
                                                     event_sport_wikidata_id
                                                     )) |> 
    dplyr::mutate(sport = tw_get_label(sport_wikidata_id)) |> 
    
    tidyr::separate(col = place_of_birth_coordinates,
                    into = c("lat", "lon"),
                    sep = ",",
                    remove = FALSE) %>% 
    mutate(country_medal = tw_get_label(country_medal_wikidata_id),
           place_of_birth_located_in = tw_get_label(place_of_birth_located_in_wikidata_id)) |> 
    dplyr::mutate(
      country_medal_code2 = tw_get_p1(id = country_medal_wikidata_id, p = "P297"),
      country_medal_code3 = tw_get_p1(id = country_medal_wikidata_id, p = "P298"),
      country_medal_ioc_country_code = tw_get_p1(id = country_medal_wikidata_id, p = "P984"),
      country_medal_NUTS_code = tw_get_p1(id = country_medal_wikidata_id, p = "P605")
    ) |> 
    dplyr::relocate(
      medalist_wikidata_id, 
      medalist_link,
      medalist_name,
      medal,
      delegation_wikidata_id,
      delegation_link,
      delegation_name,
      country_medal_wikidata_id,
      country_medal,
      country_medal_code2,
      country_medal_code3,
      country_medal_ioc_country_code,
      country_medal_NUTS_code,
      date_of_birth,
      place_of_birth_wikidata_id,
      place_of_birth,
      place_of_birth_located_in_wikidata_id,
      place_of_birth_located_in,
      place_of_birth_coordinates,
      lat,
      lon,
      sex_or_gender_wikidata_id,
      sex_or_gender,
      event_wikidata_id,
      event_link,
      event_name,
      event_part_of_wikidata_id,
      event_part_of,
      event_sport_wikidata_id,
      event_sport,
      event_part_of_sport_wikidata_id,
      event_part_of_sport,
      sport_wikidata_id,
      sport
    )
  
}

o24_drop_non_humans <- function(all_medalists_wd_df) {
  check_humans_df <- all_medalists_wd_df |> 
    dplyr::distinct(medalist_wikidata_id) |> 
    dplyr::mutate(medalist_instance_of = tw_get_p(id = medalist_wikidata_id,
                                                  p = "P31")) |> 
    tidyr::unnest(medalist_instance_of) |> 
    dplyr::group_by(medalist_wikidata_id) |> 
    dplyr::filter(!("Q5" %in% medalist_instance_of))
  
  all_medalists_wd_df |> 
    dplyr::anti_join(check_humans_df,
                     by = "medalist_wikidata_id")
}

o24_drop_human_events <- function(all_medalists_wd_df) {
  check_humans_df <- all_medalists_wd_df |> 
    dplyr::distinct(event_wikidata_id) |> 
    dplyr::mutate(event_instance_of = tw_get_p(id = event_wikidata_id,
                                                  p = "P31")) |> 
    tidyr::unnest(event_instance_of) |> 
    dplyr::group_by(event_instance_of) |> 
    dplyr::filter("Q5" %in% event_instance_of)
  
  all_medalists_wd_df |> 
    dplyr::anti_join(check_humans_df,
                     by = "event_wikidata_id")
}

### Extract from Wikipedia page ####


o24_get_medalist_from_cell <- function(row,
                                       td_number) {
  links <- row %>% 
    html_nodes("td") %>% 
    .[[td_number]] %>% 
    html_nodes("a") %>% 
    html_attr("href") 
  
  links <- links[!stringr::str_detect(links, "#cite")]
  links <- links[!stringr::str_detect(links, stringr::fixed("/wiki/List_of_"))]
  
  if (length(links)==0) {
    list(as.character(NA))
    #list(links)
  } else {
    links[stringr::str_detect(string = links,
                              pattern = "_at_the_[[:digit:]]{4}_Summer_Olympics",
                              negate = TRUE)] %>% 
      list()
  }
}

o24_get_country_from_cell <- function(row, td_number) {
  links <- row %>% 
    html_nodes("td") %>% 
    .[[td_number]] %>% 
    html_nodes("a") %>% 
    html_attr("href") 
  
  links <- links[!stringr::str_detect(links, "#cite")]
  
  if (length(links)==0) {
    as.character(NA)
  } else {
    links[stringr::str_detect(string = links,
                              pattern = "_at_the_[[:digit:]]{4}_Summer_Olympics",
                              negate = FALSE)] 
  }
}



o24_get_medals_from_table <- function(table) {
  
  table_colnames <- table %>%
    html_table() %>% 
    colnames()
  
  if (!(length(table_colnames)==4|length(table_colnames)==7|length(table_colnames)==6)) {
    return(NULL)
  }
  
  col4 <- table %>%
    html_table() %>% 
    colnames() %>% 
    .[4]
  
  if (col4 != "Bronze" & col4 != "Silver") {
    return(NULL)
  }
  
  
  current_table <- table %>% 
    html_nodes("tr") %>% 
    .[-1] #remove header
  
  purrr::map2_dfr(
    .x = current_table,
    .y = seq_along(current_table),
    .f = function(current_row, current_index) {
      # print(current_row %>%
      #         html_nodes("td") %>%
      #         .[[1]] %>%  html_text())
      
      col_number <- current_row %>% 
        html_nodes("td") %>% 
        length()
      
      if (col_number==3) {
        
        medal_row_df <- tibble::tibble(event_link = current_row %>% 
                                         html_nodes("th") %>% 
                                         .[[1]] %>% 
                                         html_nodes("a") %>% 
                                         html_attr("href"), 
                                       gold_medalist = current_row %>% 
                                         o24_get_medalist_from_cell(td_number = 1), 
                                       gold_country = current_row %>% 
                                         o24_get_country_from_cell(td_number = 1),
                                       silver_medalist = current_row %>% 
                                         o24_get_medalist_from_cell(td_number = 2), 
                                       silver_country = current_row %>% 
                                         o24_get_country_from_cell(td_number = 2),
                                       bronze_medalist = current_row %>% 
                                         o24_get_medalist_from_cell(td_number = 3), 
                                       bronze_country = current_row %>% 
                                         o24_get_country_from_cell(td_number = 3),
        ) 
        
      } else if (col_number==4) {
        
        medal_row_df <- tibble::tibble(event_link =  {
          links <- current_row %>% 
            html_nodes("td") %>% 
            .[[1]] %>% 
            html_nodes("a") %>% 
            html_attr("href")
          
          links[stringr::str_detect(links, "cite_note", negate = TRUE)]
        }, 
        gold_medalist = current_row %>% 
          o24_get_medalist_from_cell(td_number = 2), 
        gold_country = current_row %>% 
          o24_get_country_from_cell(td_number = 2),
        silver_medalist = current_row %>% 
          o24_get_medalist_from_cell(td_number = 3), 
        silver_country = current_row %>% 
          o24_get_country_from_cell(td_number = 3),
        bronze_medalist = current_row %>% 
          o24_get_medalist_from_cell(td_number = 4), 
        bronze_country = current_row %>% 
          o24_get_country_from_cell(td_number = 4),
        ) 
      } else if (col_number==7) {
        
        
        medal_row_df <- tibble::tibble(event_link = current_row %>% 
                                         html_nodes("td") %>% 
                                         .[[1]] %>% 
                                         html_nodes("a") %>% 
                                         html_attr("href"), 
                                       gold_medalist = current_row %>% 
                                         o24_get_medalist_from_cell(td_number = 2), 
                                       gold_country = current_row %>% 
                                         o24_get_country_from_cell(td_number = 2),
                                       silver_medalist = current_row %>% 
                                         o24_get_medalist_from_cell(td_number = 4), 
                                       silver_country = current_row %>% 
                                         o24_get_country_from_cell(td_number = 4),
                                       bronze_medalist = current_row %>% 
                                         o24_get_medalist_from_cell(td_number = 6), 
                                       bronze_country = current_row %>% 
                                         o24_get_country_from_cell(td_number = 6)
        ) 
      } else if (col_number==6) {
        
        
        medal_row_df <- tibble::tibble(event_link = current_row %>% 
                                         html_nodes("td") %>% 
                                         .[[1]] %>% 
                                         html_nodes("a") %>% 
                                         html_attr("href"), 
                                       gold_medalist = current_row %>% 
                                         o24_get_medalist_from_cell(td_number = 2), 
                                       gold_country = current_row %>% 
                                         o24_get_country_from_cell(td_number = 2),
                                       silver_medalist = current_row %>% 
                                         o24_get_medalist_from_cell(td_number = 4), 
                                       silver_country = current_row %>% 
                                         o24_get_country_from_cell(td_number = 4),
                                       bronze_medalist = current_row %>% 
                                         o24_get_medalist_from_cell(td_number = 5), 
                                       bronze_country = current_row %>% 
                                         o24_get_country_from_cell(td_number = 5)
        ) 
      } else if (col_number==1) {
        
        medal_row_df <- tibble::tibble(event_link = current_table[[current_index-1]] %>% 
                                         html_nodes("td") %>% 
                                         .[[1]] %>% 
                                         html_nodes("a") %>% 
                                         html_attr("href"), 
                                       gold_medalist = NA_character_, 
                                       gold_country = NA_character_,
                                       silver_medalist = NA_character_, 
                                       silver_country = NA_character_,
                                       bronze_medalist = current_row %>% 
                                         o24_get_medalist_from_cell(td_number = 1) |> 
                                         unlist(), 
                                       bronze_country = current_row %>% 
                                         o24_get_country_from_cell(td_number = 1),
        ) 
        
        
      } else {
        return(NULL)
      }
      
      
      
      dplyr::bind_rows(medal_row_df %>% 
                         dplyr::transmute(event_link,
                                          medal = "gold",
                                          medalist_link = gold_medalist,
                                          delegation_link = gold_country),
                       medal_row_df %>% 
                         dplyr::transmute(event_link,
                                          medal = "silver",
                                          medalist_link = silver_medalist,
                                          delegation_link = silver_country),
                       medal_row_df %>% 
                         dplyr::transmute(event_link,
                                          medal = "bronze",
                                          medalist_link = bronze_medalist,
                                          delegation_link = bronze_country) 
                       
      ) %>% 
        tidyr::unnest(cols = "medalist_link") %>% 
        dplyr::mutate(medalist_link = as.character(medalist_link)) %>% 
        dplyr::filter(stringr::str_detect(string = medalist_link,
                                          pattern = "#endnote",
                                          negate = TRUE))
      
    })
}
