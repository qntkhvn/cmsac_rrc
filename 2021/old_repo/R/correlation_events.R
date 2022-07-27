wq <- read_csv("data/youth_olympics_2018/women_qual.csv")
mq <- read_csv("data/youth_olympics_2018/men_qual.csv")

wf <- read_csv("data/youth_olympics_2018/women_final.csv")
mf <- read_csv("data/youth_olympics_2018/men_final.csv")

library(GGally)

wq %>% 
  select(rank, speed, bould, lead) %>%
  cor(method = "kendall")

wq %>% 
  select(speed, bould, lead) %>% 
  ggpairs(diag = "blank",
          upper = list(continuous = wrap("cor", method = "kendall")),
          axisLabels = "internal") +
  ggtitle("Kendall's rank correlations - Women's Qualification")

mq %>% 
  select(rank, speed, bould, lead) %>%
  cor(method = "kendall")

mq %>% 
  select(speed, bould, lead) %>% 
  ggpairs(diag = "blank",
          upper = list(continuous = wrap("cor", method = "kendall")),
          axisLabels = "internal") +
  ggtitle("Kendall's rank correlations - Women's Qualification")







