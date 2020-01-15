library(RSelenium)
library(tidyverse)

# load urls to scrape
seasons <- 'data/primera-b-metropolitana-urls.csv' %>%
  read_csv() %>%
  pull(url) %>%
  rev() %>%
  paste0('results/')

# Launch selenium driver
driver <- rsDriver(browser = c("chrome"))
remote_driver <- driver[["client"]]
remote_driver$open()

# functions to scrape scores and team info
get_details <- function(drv, .val) {
  e <- drv$findElements(using = 'class', value = .val)
  map_chr(e, ~ .x$getElementText() %>% unlist())
}

reshape_df <- function(game_time, home_team, away_team, scores) {
  tibble(game_time,
         home_team,
         scores,
         away_team) %>%
    mutate(scores = str_remove_all(scores, '[\n]')) %>%
    separate(scores,
             into = c('home_scr', 'away_scr'),
             sep = ' - ') %>%
    separate(game_time, into = c('date', 'time'), sep = ' ') %>%
    mutate(
      home_scr = as.integer(home_scr),
      away_scr = as.integer(away_scr),
      time = str_remove_all(time, '[:alpha:]')
    )
  
}

scrape_page <- function(url) {
  remote_driver$navigate(url)
  Sys.sleep(2)
  game_time <- get_details(remote_driver, 'event__time')
  home_team <-
    get_details(remote_driver, 'event__participant--home')
  away_team <-
    get_details(remote_driver, 'event__participant--away')
  scores <- get_details(remote_driver, 'event__scores')
  Sys.sleep(1)
  reshape_df(game_time, home_team, away_team, scores) %>%
    mutate(season = url)
}

# defend against 404 or missing data
scores <- map(seasons, possibly(~ scrape_page(.x), NULL))

scores %>%
  bind_rows() %>%
  write_csv('data/results.csv')
