library(tidyverse)
library(broom)
theme_set(theme_light())

# Women's qual
wq <- read_csv("data/2020_olympics/wq.csv")
wq_cleaned <- wq %>%
  mutate(
    a = as.numeric(str_remove_all(a, "[A-z]")),
    b = as.numeric(str_remove_all(b, "[A-z]")),
    speed_best = ifelse(is.na(a), b,
                        ifelse(is.na(b), a,
                               ifelse(a < b, a, b))),
    bould_tops = as.numeric(str_sub(results, 1, 1)),
    lead_hr = ifelse(
      str_detect(hr, "\\+"),
      as.numeric(str_sub(hr, 1, nchar(hr) - 1)) + 0.5,
      as.numeric(hr)
    )
  ) %>%
  select(climber, speed_best, bould_tops, lead_hr) %>%
  column_to_rownames(var = "climber")

# Men's qual
mq <- read_csv("data/2020_olympics/mq.csv")
mq_cleaned <- mq %>%
  mutate(
    a = as.numeric(str_remove_all(a, "[A-z]")),
    b = as.numeric(str_remove_all(b, "[A-z]")),
    speed_best = ifelse(is.na(a), b,
                        ifelse(is.na(b), a,
                               ifelse(a < b, a, b))),
    bould_tops = as.numeric(str_sub(results, 1, 1)),
    lead_hr = ifelse(
      str_detect(hr, "\\+"),
      as.numeric(str_sub(hr, 1, nchar(hr) - 1)) + 0.5,
      as.numeric(hr)
    )
  ) %>%
  select(climber, speed_best, bould_tops, lead_hr) %>%
  column_to_rownames(var = "climber")

# SOME EDA

# pca fit
wq_pca <- wq_cleaned %>%
  prcomp(scale = TRUE)

# plot
library(ggfortify)
wq_pca %>% 
  autoplot(label = TRUE, 
           label.size = 2,
           loadings = TRUE,
           loadings.label = TRUE, 
           loadings.label.size = 3)


# percent variability
wq_pca %>% 
  tidy(matrix = "eigenvalues")

wq_pca %>% 
  tidy(matrix = "eigenvalues") %>% 
  ggplot(aes(PC, percent)) +
  geom_col()

# scores and loadings
wq_pca %>% 
  tidy(matrix = "scores")

wq_pca %>% 
  tidy(matrix = "loadings")

# how each variable contributes to each PC
wq_pca %>% 
  tidy(matrix = "loadings") %>% 
  ggplot(aes(value, column)) +
  facet_wrap(~ PC) +
  geom_col()

wq_pca %>% 
  tidy(matrix = "rotation")

# attach pca fit to original data
wq_pca %>% 
  augment(wq_cleaned)
