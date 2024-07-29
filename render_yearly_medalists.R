source(here::here("functions", "setup.R"))
source(here::here("functions", "get_list_of_lists.R"))

modern_olympics_years_v <- list_of_lists_df |> 
  dplyr::arrange(dplyr::desc(year)) |> 
  dplyr::filter(year>1945) |> 
  dplyr::pull(year)

purrr::walk(
  .x = modern_olympics_years_v,
  .f = \(olympics_year) {
    
    fs::dir_create(here::here(olympics_year))
    
    quarto::quarto_render(
      input = here::here("medalists_by_year.qmd"), 
      output_file = "temp.html",
      execute_params = list(year = olympics_year),
      execute_dir = here::here()
    )
    
    fs::file_move(path = here::here("temp.html"),
                  new_path = here::here(olympics_year, "index.html"))
    
  }
)
