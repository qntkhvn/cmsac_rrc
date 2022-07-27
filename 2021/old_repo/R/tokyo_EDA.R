# load data from data folder first

mq %>% 
  select(climber,
         overall = rank, 
         speed = cp,
         bouldering = cp_2,
         lead = cp_3,
         total) %>% 
  select(climber, S = speed, B = bouldering, L = lead) %>% 
  pivot_longer(!climber, names_to = "event", values_to = "rank") %>% 
  mutate(climber = fct_rev(factor(climber, levels = mq$climber))) %>% 
  ggplot(aes(x = event, y = climber, fill = as.numeric(rank))) +
  geom_tile(color = "white", size = 0.1) +
  scale_fill_viridis(breaks = c(1, 5, 10, 15, 20), direction = -1) +
  coord_equal() +
  labs(fill = "Rank")



df <- mf %>% 
  select(climber,
         speed = cp,
         bould = cp_2,
         lead = cp_3) %>% 
  filter(climber != "Bassa Mawem")

rerank <- list()
for (i in 1:nrow(df)) {
  rerank[[i]] <- df[-i, ] %>%
    mutate(rank_drop = i)
}

rerank_df <- df %>%
  mutate(rank_drop = 0) %>%
  bind_rows(rerank) %>%
  mutate(speed = as.numeric(speed),
         bould = as.numeric(bould),
         lead = as.numeric(lead)) %>% 
  group_by(rank_drop) %>%
  mutate(
    speed = rank(speed),
    bould = rank(bould),
    lead = rank(lead),
    total = speed * bould * lead
  ) %>%
  arrange(total, .by_group = TRUE) %>%
  ungroup() %>%
  group_by(rank_drop, total) %>%
  mutate(
    speed_tb = ifelse(speed < lag(speed), 1, 0),
    bould_tb = ifelse(bould < lag(bould), 1, 0),
    lead_tb = ifelse(lead < lag(lead), 1, 0),
    tb = speed_tb + bould_tb + lead_tb,
    tb = ifelse(is.na(tb), 1, tb)
  ) %>%
  ungroup() %>%
  group_by(rank_drop) %>%
  arrange(total,-tb, .by_group = TRUE) %>%
  mutate(rank = row_number())




rerank_df %>% 
  mutate(climber = paste(str_sub(climber, 1, 1), word(climber, -1))) %>% 
  mutate(climber = fct_reorder(climber, -rank),
         rank = as.factor(rank)) %>% 
  #        rank_change = ifelse(
  #          rank_drop %in% c(0, 4) | 
  #            rank_drop == 1 & rank %in% 4:5 |
  #            rank_drop == 2 & rank %in% c(1, 5) |
  #            rank_drop %in% c(3, 5) & rank %in% c(1, 2, 5) |
  #            rank_drop == 6 & rank %in% 1:3, 
  #          "no", "yes")) %>% 
  ggplot(aes(x = climber, y = total)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = rank), hjust = -0.2, size = 3) +
  coord_flip() +
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf),
            data = ~ filter(., rank_drop %in% 1:6),
            color = "black", size = 1.5, fill = NA, inherit.aes = FALSE) +
  facet_wrap(~ rank_drop, nrow = 2) +
  expand_limits(y = 150, x = 0:7) +
  # scale_fill_manual(values = c("grey", "chocolate")) +
  # labs(x = "Men", y = "") +
  theme(axis.ticks = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_blank())





url <- "https://en.wikipedia.org/wiki/Sport_climbing_at_the_2020_Summer_Olympics_%E2%80%93_Women%27s_combined"

wf <- url %>% 
  read_html() %>% 
  html_table() %>% 
  .[[8]] %>% 
  row_to_names(row_number = 1) %>% 
  clean_names() %>% 
  filter(climber != "Climber") %>% 
  mutate(rank = 1:8)

df <- wf %>% 
  select(climber,
         speed = cp,
         bould = cp_2,
         lead = cp_3)







wf %>% 
  select(overall = rank,
         speed = cp,
         bould = cp_2,
         lead = cp_3) %>% 
  mutate(across(where(is.character), as.numeric)) %>% 
  ggpairs(diag = "blank",
          axisLabels = "none",
          upper = list(continuous = wrap("cor", method = "kendall", stars = FALSE))) +
  theme_light() + ggtitle("2020 Olympics - Women's Qualification")


rerank <- list()
for (i in 1:nrow(df)) {
  rerank[[i]] <- df[-i, ] %>%
    mutate(rank_drop = i)
}

rerank_df <- df %>%
  mutate(rank_drop = 0) %>%
  bind_rows(rerank) %>%
  mutate(speed = as.numeric(speed),
         bould = as.numeric(bould),
         lead = as.numeric(lead)) %>% 
  group_by(rank_drop) %>%
  mutate(
    speed = rank(speed),
    bould = rank(bould),
    lead = rank(lead),
    total = speed * bould * lead
  ) %>%
  arrange(total, .by_group = TRUE) %>%
  ungroup() %>%
  group_by(rank_drop, total) %>%
  mutate(
    speed_tb = ifelse(speed < lag(speed), 1, 0),
    bould_tb = ifelse(bould < lag(bould), 1, 0),
    lead_tb = ifelse(lead < lag(lead), 1, 0),
    tb = speed_tb + bould_tb + lead_tb,
    tb = ifelse(is.na(tb), 1, tb)
  ) %>%
  ungroup() %>%
  group_by(rank_drop) %>%
  arrange(total,-tb, .by_group = TRUE) %>%
  mutate(rank = row_number())


rerank_df %>% 
  mutate(climber = paste(str_sub(climber, 1, 1), word(climber, -1))) %>% 
  mutate(climber = fct_reorder(climber, -rank),
         rank = as.factor(rank)) %>% 
  #        rank_change = ifelse(
  #          rank_drop %in% c(0, 4) | 
  #            rank_drop == 1 & rank %in% 4:5 |
  #            rank_drop == 2 & rank %in% c(1, 5) |
  #            rank_drop %in% c(3, 5) & rank %in% c(1, 2, 5) |
  #            rank_drop == 6 & rank %in% 1:3, 
  #          "no", "yes")) %>% 
  ggplot(aes(x = climber, y = total)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = rank), hjust = -0.2, size = 3) +
  coord_flip() +
  # geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf),
  #           data = ~ filter(., rank_drop %in% c(1, 2, 3, 5, 6)), 
  #           color = "black", size = 1.5, fill = NA, inherit.aes = FALSE) +
  facet_wrap(~ rank_drop, nrow = 2) +
  expand_limits(y = 200, x = 0:7) +
  # scale_fill_manual(values = c("grey", "chocolate")) +
  # labs(x = "Men", y = "") +
  theme(axis.ticks = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_blank())
