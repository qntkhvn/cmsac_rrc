# Leave-one-climber-out Analysis

# 2018 Youth Olympics women's final data
wf <- read_csv("https://raw.githubusercontent.com/qntkhvn/cmsac_rrc/main/data/2018_youth_olympics/women_final.csv")

# function to drop and re-rank the climbers
drop_rerank <- function(df) {
  rerank <- list()
  
  # drop each rank
  for (i in 1:nrow(df)) {
    rerank[[i]] <- df[-i,] %>%
      mutate(rank_drop = i)
  }
  
  # new data with all cases of rank dropped
  rerank_df <- df %>%
    mutate(rank_drop = 0) %>%
    bind_rows(rerank) %>%
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
    
    # dealing with ties
    mutate(
      speed_tb = ifelse(speed < lag(speed), 1, 0),
      bould_tb = ifelse(bould < lag(bould), 1, 0),
      lead_tb = ifelse(lead < lag(lead), 1, 0),
      tb = speed_tb + bould_tb + lead_tb,
      tb = ifelse(is.na(tb), 1, tb)
    ) %>%
    ungroup() %>%
    group_by(rank_drop) %>%
    arrange(total, -tb, .by_group = TRUE) %>%
    mutate(rank = row_number(),
           last = str_to_title(last))
  
  return(rerank_df)
}


# plot all cases of modified rankings
drop_rerank(wf) %>%
  mutate(
    last = fct_reorder(last,-rank),
    rank = as.factor(rank),
    
    # indicator for climber with rank change, for viz (color filling) purpose
    rank_change = ifelse(
      rank_drop %in% c(0, 1, 4, 6) | rank_drop == 2 & rank %in% 1:3 |
        rank_drop == 3 & rank %in% c(1, 4, 5) | rank_drop == 5 & rank %in% c(1, 5),
      "no",
      "yes")) %>%
  ggplot(aes(x = last, y = total, fill = rank_change)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = rank),
            hjust = -0.2,
            size = 3,
            color = "black") +
  coord_flip() +
  
  # create black panel border for cases with change in rank orderings
  geom_rect(
    aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf),
    data = ~ filter(., rank_drop %in% c(2, 3, 5)),
    color = "black",
    size = 1.5,
    fill = NA,
    inherit.aes = FALSE
  ) +
  facet_wrap( ~ rank_drop, nrow = 2, ncol = 4) +
  expand_limits(y = 62, x = 0:7) +
  scale_fill_manual(values = c("grey", "red")) +
  labs(y = "Score", x = NULL) +
  theme(axis.ticks = element_blank(),
        panel.grid.minor = element_blank())
