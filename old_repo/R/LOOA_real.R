library(tidyverse)
theme_set(theme_light())

# Women final

women_final <- read_csv("data/youth_olympics_2018/women_final.csv")
rerank <- list()
for (i in 1:nrow(women_final)){
  rerank[[i]] <- women_final[-i,] %>%
    mutate(rank_drop = i)
}

rerank_df <- women_final %>% 
  mutate(rank_drop = 0) %>% 
  bind_rows(rerank) %>% 
  group_by(rank_drop) %>% 
  mutate(speed = rank(speed),
         bould = rank(bould),
         lead = rank(lead),
         total = speed * bould * lead) %>% 
  arrange(total, .by_group = TRUE) %>% 
  ungroup() %>% 
  group_by(rank_drop, total) %>% 
  mutate(speed_tb = ifelse(speed < lag(speed), 1, 0),
         bould_tb = ifelse(bould < lag(bould), 1, 0),
         lead_tb = ifelse(lead < lag(lead), 1, 0),
         tb = speed_tb + bould_tb + lead_tb,
         tb = ifelse(is.na(tb), 1, tb)) %>% 
  ungroup() %>% 
  group_by(rank_drop) %>% 
  arrange(total, -tb, .by_group = TRUE) %>% 
  mutate(rank = row_number())


rerank_df %>% 
  ggplot(aes(x = reorder(last, -rank), y = total)) +
  geom_col() +
  geom_text(aes(label = rank), hjust = -0.1, size = 3) +
  coord_flip() +
  facet_wrap(~ rank_drop) +
  labs(title = "Women's final rankings",
       subtitle = "0 is the original rankings, each facet number is the dropped rank",
       x = "")

# animation just for fun
library(gganimate)

a <- rerank_df %>% 
  ggplot() +
  geom_col(aes(x = rank, y = total, group = last), width = 0.5, fill = "salmon", show.legend = FALSE) +
  geom_text(aes(x = rank, y = 0, label = paste(last, " ", sep = " "), group = last), 
            hjust = 1, size = 3, vjust = 0.35) +
  geom_text(aes(x = rank, y = total, label = as.character(total)), 
            hjust = -0.1, size = 3) +
  scale_x_reverse() +
  coord_flip(clip = "off", expand = FALSE) +
  ylim(c(0, 65)) +
  theme(
    axis.ticks.y = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    plot.margin = margin(0.5, 0.5, 0.5, 3, "cm"),
    panel.spacing.x = unit(6, "lines"),
    panel.border = element_blank()
  ) +
  transition_states(rank_drop, transition_length = 4, state_length = 1, wrap = FALSE) +
  ggtitle("Remove rank {closest_state}")
  
animate(a, nframes = 100, fps = 20, height = 480, width = 600, res = 95)


library(cowplot)

a <- drop_rerank(wf) %>% 
  ggplot(aes(x = rank_drop, y = rank, color = last)) +
  geom_line(aes(color = last, alpha = 1), size = 2) +
  geom_point(aes(color = last, alpha = 1), size = 4) +
  # geom_text(aes(x = 7, label = last), hjust = -0.1, alpha = 0.7, size = 6) +
  # geom_text(aes(x = rank_drop, y = 6.5, label = as.character(rank_drop)),
  #           alpha = 0.5,  col = "gray", size = 10) +
  # geom_segment(aes(xend = max(rank_drop), yend = rank), linetype = 2, size = 0.7) +
  # geom_text(aes(x = 0, y = 0, label = paste("Leaving out climber with rank", floor(rank_drop))), hjust = 0, size = 5.5) +
  scale_y_reverse(breaks = 1:6) + 
  scale_x_continuous(breaks = 0:6) +
  theme_minimal_grid(font_size = 14, line_size = 0) +
  theme(
    legend.position = "none",
    # axis.title = element_blank(),
    # axis.text = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
  ) +
  transition_reveal(rank_drop) +
  enter_fade() +
  exit_fade() +
  labs(x = "Excluded Rank",
       y = "Final Rank",
       title = "Leave-one-climber-out Modified Rankings\n2018 Youth Olympics - Women's Event")


animate(a, res = 90, duration = 6, width = 550)
