library(tidyverse)

women_qual <- tibble(
  speed = c(8, 17, 3, 14, 10, 4, 1, 2, 6, 13, 21,
            5, 12, 9, 7, 11, 15, 18, 19, 20, 16),
  bould = c(4, 3, 6, 1, 2, 9, 17, 12, 7, 5, 10, 14,
            13, 8, 11, 15, 18, 16, 20, 19, 21),
  lead = c(1, 2, 6, 9, 7, 4, 16, 14, 11, 8, 3,
           10, 5, 12, 15, 20, 13, 19, 17, 18, 21)
) %>% 
  mutate(total = speed * bould * lead,
         rank = row_number())


women_final <- tibble(
  speed = c(3, 6, 1, 4, 2, 5),
  bould = c(3, 1, 4, 5, 6, 2),
  lead = c(2, 3, 5, 1, 4, 6)
) %>% 
  mutate(total = speed * bould * lead,
         rank = row_number())  

men_qual <- tibble(
  speed = c(9, 3, 13, 15, 14, 19, 12, 1, 5, 6, 2, 10,
            11, 4, 18, 20, 8, 7, 16, 17, 21),
  bould = c(1, 7, 9, 2, 6, 5, 3, 20.5, 10, 13, 17, 12,
            8, 20.5, 11, 4, 16, 14, 18, 15, 19),
  lead = c(11, 5, 1, 4, 2, 3, 9, 19, 10, 7, 20, 8,
           12, 14, 6, 17, 15, 21, 13, 16, 18)
) %>% 
  mutate(total = speed * bould * lead,
         rank = row_number())

men_final <- tibble(
  speed = c(2, 6, 1, 4, 3, 5),
  bould = c(1, 3, 5, 2, 6, 4),
  lead = c(3, 1, 6, 4, 2, 5)
) %>% 
  mutate(total = speed * bould * lead,
         rank = row_number())