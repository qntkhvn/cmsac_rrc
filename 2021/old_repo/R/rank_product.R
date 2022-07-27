# Hannah Butler
# 07-10-2021
# Olympic Sport Climbing Scoring
##### Description #####
# Rank Product Test
# I recommend looking at the rmd file
##### Notes/Random thoughts ######
# generalizing the number of events
# does the probability of a tie change for the number of ranks?
##### Libraries #####
library(tidyverse)
##### Set Constants #####
m_qual_path <- "data/2018_youth_olympics/men_qual.csv"
m_fin_path <- "data/2018_youth_olympics/men_final.csv"
f_qual_path <- "data/2018_youth_olympics/women_qual.csv"
f_fin_path <- "data/2018_youth_olympics/women_final.csv"
##### Define Functions #####
geo_mean <- function(x, l = length(x)) {
  geomean <- prod(x)^(1/l)
  return(geomean)
}

assign_ranks <- function(c = 8) {
  # randomly assign ranks in 3 events & compute total
  # ranking in an event is considered to be independent from ranking in another event
  output <- data.frame(climber = sample(LETTERS, c, replace = FALSE)
                       , speed = sample(1:c, c, replace = FALSE)
                       , bould = sample(1:c, c, replace = FALSE)
                       , lead = sample(1:c, c, replace = FALSE)
                       ) %>%
    mutate(total = speed*bould*lead) %>%
    arrange(total)
  
  return(output)
}
##### load data #####
# add rank-product stat
m_qual_rp <- read_csv(m_qual_path)%>%
  mutate(rp = total^(1/3))
m_fin_rp <- read_csv(m_fin_path)%>%
  mutate(rp = total^(1/3))
f_qual_rp <- read_csv(f_qual_path)%>%
  mutate(rp = total^(1/3))
f_fin_rp <- read_csv(f_fin_path)%>%
  mutate(rp = total^(1/3))
##### Begin Code #####
n_perm <- 1000
# Rank Product for Finals (n = 6, k = 3)
# mens
fin_perms <- lapply(1:n_perm
                      , function(x) assign_ranks(6) %>% mutate(rp = total^(1/3))
                      )
rp_set_1 <- sapply(m_fin_perms, function(x) x$rp[1])
sum(m_fin$rp[1] >= rp_set_1)/1000 # mens finals first place p val = 0.568
sum(f_fin$rp[1] >= rp_set_1)/1000 # womens finals first place p val = 0.013
