library(tidyverse)
library(rvest)

results_archive <- 'https://www.flashscore.com/football/argentina/primera-b-metropolitana/archive/'
webpage <- read_html(results_archive)

paste0(
  'https://www.flashscore.com',
  webpage %>%
    html_nodes('a') %>%
    html_attr('href') %>%
    keep( ~ str_detect(., 'primera-b-metropolitana-'))
) %>%
  tibble(url = .) %>%
  write_csv('data/primera-b-metropolitana-urls.csv')




