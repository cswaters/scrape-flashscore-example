library(tidyverse)
library(RSelenium)

# Launch selenium driver
driver <- rsDriver(browser = c("chrome"))
remote_driver <- driver[["client"]]
remote_driver$open()

# functions to scrape scores and team info
get_details <- function(drv, .val) {
  e <- drv$findElements(using = 'class', value = .val)
  map_chr(e, ~ .x$getElementText() %>% unlist())
}

make_urls <- function(seas){
    paste0('https://www.flashscore.com/basketball/usa/wnba-',seas,'/results/')
  }

seasons <- make_urls(2015:2019)

scrape_page <- function(url) {
  remote_driver$navigate(url)
  Sys.sleep(1)
  game_date <- get_details(remote_driver, 'event__time')
  away_teams <- get_details(remote_driver, 'event__participant--away')
  home_teams <- get_details(remote_driver, 'event__participant--home')
  home_score <- get_details(remote_driver, 'event__score--home')
  away_score <- get_details(remote_driver, 'event__score--away')
  
  Sys.sleep(1)
  tibble(
    date = game_date,
    h = home_teams,
    v = away_teams,
    ptsh = home_score,
    ptsv = away_score
  ) %>% 
    mutate(url = url)
}

# defend against 404 or missing data
scores <- map(seasons, possibly(~ scrape_page(.x), NULL))

clean_scores <- scores %>% 
  bind_rows() %>% 
  separate(date, 
           into=c('game_date','game_time'), 
           sep=' ') %>% 
  extract(url, into = c('seas'), regex = '([:digit:]+)', remove = FALSE) %>% 
  mutate(game_date = str_replace_all(game_date, '[.]','-') %>% 
           paste0(seas) %>% lubridate::dmy(),
         ptsh = as.integer(ptsh),
         ptsv = as.integer(ptsv),
         seas = as.integer(seas),
         h = str_remove_all(h,' W'),
         v = str_remove_all(v,' W')
         )

clean_scores %>% 
  write_csv('data/wnba-game-scores-2015-2019.csv')
