# Rank correlation analysis

# Tokyo 2020 women's qualification data
wq <- read_csv("https://raw.githubusercontent.com/qntkhvn/cmsac_rrc/main/data/2020_olympics/wq.csv")

# scatterplots of overall rank vs ranks of speed, bouldering, lead
wq %>% 
  select(overall, speed, bouldering, lead) %>% 
  rename_with(str_to_title) %>% 
  pivot_longer(!Overall, 
               names_to = "Discipline",
               values_to = "Rank") %>%
  mutate(Discipline = factor(Discipline, 
                             levels = c("Speed", "Bouldering", "Lead"))) %>% 
  ggplot(aes(Rank, Overall)) +
  geom_point() +
  geom_smooth(span = 1, color = "darkblue") +
  facet_wrap(~ Discipline) +
  theme(panel.grid.major.x = element_blank())

# kendall's tau matrix
wq %>% 
  select(overall, speed, bouldering, lead) %>% 
  cor(method = "kendall")

# testing for association
cor.test(wq$overall, wq$bouldering, method = "kendall")
cor.test(wq$overall, wq$lead, method = "kendall")
cor.test(wq$overall, wq$speed, method = "kendall")

# bootstrapped confidence intervals
# package from book Nonparametric Statistical Methods, 3rd Edition
# by Hollander, Wolfe, Chicken
library(NSM3) 

set.seed(21)
kendall.ci(wq$overall, wq$bouldering, bootstrap = TRUE, B = 1000)
kendall.ci(wq$overall, wq$lead, bootstrap = TRUE, B = 1000)
kendall.ci(wq$overall, wq$speed, bootstrap = TRUE, B = 1000)

# PCA

# some data prep
# mostly dealing with strings
wq_cleaned <- wq %>%
  mutate(
    a = as.numeric(str_remove_all(a, "[A-z]")),
    b = as.numeric(str_remove_all(b, "[A-z]")),
    speed_time = ifelse(is.na(a), b,
                        ifelse(is.na(b), a,
                               ifelse(a < b, a, b))),
    bould_tops = as.numeric(str_sub(results, 1, 1)),
    lead_holds = ifelse(
      str_detect(hr, "\\+"),
      as.numeric(str_sub(hr, 1, nchar(hr) - 1)) + 0.5,
      as.numeric(hr)),
    Qualified = ifelse(overall < 9, "Yes", "No"))

# PCA fit
wq_pca <- wq_cleaned %>%
  dplyr::select(climber, speed_time, bould_tops, lead_holds) %>%
  mutate(climber = str_remove(climber, ".*\\s")) %>% 
  column_to_rownames(var = "climber") %>% 
  prcomp(scale = TRUE)

# PCA biplot
library(ggfortify)
wq_pca %>% 
  autoplot(label = TRUE, 
           label.size = 2.5,
           label.repel = TRUE,
           loadings = TRUE,
           loadings.label = TRUE, 
           loadings.label.size = 3.5,
           loadings.label.repel = TRUE,
           data = wq_cleaned,
           colour = "Qualified") +
  scale_color_manual(values = c("maroon", "#b79906"))
