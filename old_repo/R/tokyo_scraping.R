
library(tidyverse)
library(rvest)
library(janitor)


men_url <- "https://en.wikipedia.org/wiki/Sport_climbing_at_the_2020_Summer_Olympics_%E2%80%93_Men%27s"

mq <- men_url %>%
  read_html() %>%
  html_table() %>%
  .[[7]] %>%
  row_to_names(row_number = 1) %>%
  clean_names() %>% 
  rename(overall = rank, 
         speed = cp,
         bouldering = cp_2,
         lead = cp_3,
         total = total_11)
  

mf <- men_url %>%
  read_html() %>%
  html_table() %>%
  .[[8]] %>%
  row_to_names(row_number = 1) %>%
  clean_names() %>% 
  filter(climber != "Climber") %>% 
  mutate(rank = 1:8) %>% 
  rename(overall = rank, 
         speed = cp,
         bouldering = cp_2,
         lead = cp_3,
         total = total_16)

women_url <- "https://en.wikipedia.org/wiki/Sport_climbing_at_the_2020_Summer_Olympics_%E2%80%93_Women%27s_combined"

wq <- women_url %>% 
  read_html() %>% 
  html_table() %>% 
  .[[7]] %>% 
  row_to_names(row_number = 1) %>% 
  clean_names() %>% 
  rename(overall = rank, 
         speed = cp,
         bouldering = cp_2,
         lead = cp_3,
         total = total_10)

wf <- women_url %>% 
  read_html() %>% 
  html_table() %>% 
  .[[8]] %>% 
  row_to_names(row_number = 1) %>% 
  clean_names() %>% 
  filter(climber != "Climber") %>% 
  mutate(rank = 1:8) %>% 
  rename(overall = rank, 
         speed = cp,
         bouldering = cp_2,
         lead = cp_3)
