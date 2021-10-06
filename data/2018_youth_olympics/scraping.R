# Source:
# https://library.olympic.org/Default/digitalCollection/DigitalCollectionAttachmentDownloadHandler.ashx?parentDocumentId=177522&documentId=177531

library(tidyverse)
library(pdftools)
txt <- pdf_text("data/youth_olympics_2018/climbing_youth_olympics_2018.pdf")
# each index is a page

# women qualification
# page 15
women_qual <- txt[15] %>% 
  read_lines() %>% 
  tibble(info = .) %>% 
  mutate(info = str_squish(info),
         event = "wq") %>%
  filter(str_detect(info, "^[0-9]{1,2}\\s")) %>% 
  mutate(info = str_replace(info, " Zaneta", "-Zaneta"),
         info = str_replace(info, " Lara", "-Lara"),
         info = str_remove(info, " Q")) %>% 
  separate(info, into = c("rank", "bib", "last", "first", "from", "speed", "bould", "lead", "total"), sep = " ") %>% 
  mutate(across(c(rank, speed:total), as.numeric))

# write_csv(women_qual, "women_qual.csv")


# women final
# page 19
women_final <- txt[19] %>% 
  read_lines() %>% 
  tibble(info = .) %>% 
  mutate(info = str_squish(info),
         event = "wf") %>% 
  filter(str_detect(info, "^[1-6]{1}\\s")) %>% 
  separate(info, into = c("rank", "bib", "last", "first", "from", "speed", "bould", "lead", "total"), sep = " ") %>% 
  mutate(across(c(rank, speed:total), as.numeric))

# write_csv(women_final, "women_final.csv")


# men qualification
# page 25
men_qual <- txt[25] %>% 
  read_lines() %>% 
  tibble(info = .) %>% 
  mutate(info = str_squish(info),
         event = "mq") %>%
  filter(str_detect(info, "^[0-9]{1,2}\\s")) %>% 
  mutate(info = str_replace(info, "Chong Kiat ", "Chong-Kiat-"),
         info = str_replace(info, "Lukas Joris ", "Lukas-Joris-"),
         info = str_replace(info, "LINACISORO MOLINA Mikel ", "LINACISORO-MOLINA Mikel-"),
         info = str_replace(info, "David ", "David-"),
         info = str_remove(info, " Q")) %>% 
  separate(info, into = c("rank", "bib", "last", "first", "from", "speed", "bould", "lead", "total"), sep = " ") %>% 
  mutate(across(c(rank, speed:total), as.numeric))

# write_csv(men_qual, "men_qual.csv")


# men final
# page 29
men_final <- txt[29] %>% 
  read_lines() %>% 
  tibble(info = .) %>% 
  mutate(info = str_squish(info),
         event = "mf") %>% 
  filter(str_detect(info, "^[1-6]{1}\\s")) %>% 
  separate(info, into = c("rank", "bib", "last", "first", "from", "speed", "bould", "lead", "total"), sep = " ") %>% 
  mutate(across(c(rank, speed:total), as.numeric))

# write_csv(men_final, "men_final.csv")
