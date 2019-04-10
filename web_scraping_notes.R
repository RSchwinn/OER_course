library(rvest)
library(tidyverse)

page <- read_html("http://www.imdb.com/chart/top")

titles <- page %>%
    html_nodes(".titleColumn a") %>%
    html_text()

years <- page %>%
    html_nodes(".secondaryInfo") %>%
    html_text() %>%
    str_replace("\\(", "") %>% # remove (
    str_replace("\\)", "") %>% # remove )
    as.numeric()

scores <- page %>%
    html_nodes("#main strong") %>%
    html_text() %>%
    as.numeric()

imdb_top_250 <- tibble(
    title = titles, 
    year = years, 
    score = scores
)
