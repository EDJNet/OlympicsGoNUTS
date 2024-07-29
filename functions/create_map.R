library("dplyr", warn.conflicts = FALSE)
library("leaflet")

o24_create_global_map <- function(all_medalists_wd_nuts_pop_df, olympics_year) {

  all_medalists_ll <- all_medalists_wd_nuts_pop_df  %>% 
    dplyr::filter(is.na(lon)==FALSE) |> 
    dplyr::select(lat, lon, medal, medalist_wikidata_id, medalist_name, medalist_link, event_part_of, delegation_name, place_of_birth, sport
                  #, img_filename
    ) %>% 
    dplyr::mutate(medal_icon_link = dplyr::case_when(medal == "gold" ~ "https://upload.wikimedia.org/wikipedia/commons/4/4f/Gold_medal_olympic.svg",
                                                     medal == "silver" ~ "https://upload.wikimedia.org/wikipedia/commons/6/67/Silver_medal_olympic.svg",
                                                     medal == "bronze" ~ "https://upload.wikimedia.org/wikipedia/commons/f/f9/Bronze_medal_olympic.svg")) %>% 
    dplyr::group_by(medalist_wikidata_id) %>% 
    dplyr::mutate(medal = stringr::str_c(medal,collapse = ", "), event_part_of = stringr::str_c(unique(event_part_of), collapse = ", ")) %>% 
    dplyr::mutate(popup_content = stringr::str_c(
      "<big><b><a href='", medalist_link, "' target='_blank'>", medalist_name, "</a></b><br />",
      "Born in ", place_of_birth,"<br />",
      "Won ", medal, " medal in ", sport, " for ", stringr::str_remove(string = delegation_name, pattern = " at the 2020 Summer Olympics"), "</big>"
      #,
      #    "<a href='https://en.wikipedia.org", medalist_link, "'>", "<img src='", "https://commons.wikimedia.org/w/index.php?title=Special:Redirect/file/", img_filename,  "&width=120'></a>"
      
    )) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(lat = as.numeric(lat), lon = as.numeric(lon)) |> 
    dplyr::group_by(lat, lon) %>% 
    dplyr::add_count(name = "n_born_same_place") %>% 
    dplyr::mutate(lat = dplyr::if_else(condition = n_born_same_place>1,
                                       true = jitter(x = lat, amount = 0.01),
                                       false = lat,
                                       missing = as.numeric(NA))) %>% 
    dplyr::mutate(lon = dplyr::if_else(condition = n_born_same_place>1,
                                       true = jitter(x = lon, amount = 0.01),
                                       false = lon,
                                       missing = as.numeric(NA)))
  
  # remove the following line if you're running it on your own:
  # <script defer data-domain=\"edjnet.github.io\" src=\"https://plausible.europeandatajournalism.eu/js/plausible.js\"></script>
  # the first part is necessary for responsive popups on mobile
  
  responsiveness_and_stats <- "\'<meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\"><script defer data-domain=\"edjnet.github.io\" src=\"https://plausible.europeandatajournalism.eu/js/plausible.js\"></script>\'"
  
  leaflet_medals <- all_medalists_ll %>% 
    leaflet() %>%
    leaflet::addTiles(urlTemplate = "https://{s}.basemaps.cartocdn.com/dark_all/{z}/{x}/{y}{r}.png") %>% 
    leaflet::addMarkers(
      lng = all_medalists_ll$lon,
      lat = all_medalists_ll$lat,
      popup = all_medalists_ll$popup_content,
      icon = ~ icons(
        iconUrl = medal_icon_link,
        iconWidth = 24, iconHeight = 24,
      )
    ) %>% 
    htmlwidgets::onRender(paste0("
    function(el, x) {
      $('head').append(",responsiveness_and_stats,");
    }"))
  
  
  
  htmlwidgets::saveWidget(leaflet_medals,
                          file = here::here(olympics_year, stringr::str_c("medalists_map.html")),
                          title = stringr::str_c("Medalists at the ", olympics_year, " Summer Olympics by place of birth"),
                          selfcontained = TRUE)
  
  fs::dir_delete(path = here::here(olympics_year, stringr::str_c("medalists_map_files")))
  
  leaflet_medals
}