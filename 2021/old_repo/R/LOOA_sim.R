# In what percentage of the (6!)^3 orderings would the winner change 
# if one of the 6 competitors was dropped and there were only 5 instead of 6?

library(tidyverse)
library(combinat)

nplay <- 5
perms <- crossing(e1 = permn(nplay), 
                  e2 = permn(nplay), 
                  e3 = permn(nplay))
# perms is a tibble of lists

# expand out
perms_expand <- perms %>% 
  mutate(perm = row_number()) %>% 
  unnest(cols = c(e1, e2, e3))

perms_expand <- perms_expand %>% 
  mutate(total = e1 * e2 * e3) %>% 
  group_by(perm) %>% 
  mutate(rank = rank(total)) %>%  # need to fix ties
  ungroup()

# get_perm <- function(x) {
#   unnest_perm <- perms[x, ] %>%
#     unnest(cols = c(e1, e2, e3)) %>% 
#     mutate(total = e1 * e2 * e3,
#            initial_rank = rank(total))
#   
#   unnest_perm %>%
#     mutate(drop = 0) %>%
#     bind_rows(unnest_perm %>% slice(-1) %>% mutate(drop = 1)) %>%
#     bind_rows(unnest_perm %>% slice(-2) %>% mutate(drop = 2)) %>%
#     bind_rows(unnest_perm %>% slice(-3) %>% mutate(drop = 3)) %>%
#     bind_rows(unnest_perm %>% slice(-4) %>% mutate(drop = 4)) %>%
#     bind_rows(unnest_perm %>% slice(-5) %>% mutate(drop = 5)) %>%
#     bind_rows(unnest_perm %>% slice(-6) %>% mutate(drop = 6)) %>%
#     mutate(perm = x)
# }
# 
# 
# perms_df <- tibble()
# for (row in 1:nrow(perms)) {
#   perms_df <- perms_df %>% 
#     bind_rows(get_perm(row))
# }
# 
# f <- perms_df %>% 
#   filter(drop != 0) %>% 
#   mutate(total = e1 * e2 * e3) %>% 
#   group_by(perm, drop) %>% 
#   mutate(new_rank = rank(total, ties.method = "random"),
#          initial_rank_ordered = rank(initial_rank))
#
#
# dealing with ties
#
# qual <- bind_rows(qual_sim) %>% 
#   mutate(total = e1 * e2 * e3) %>% 
#   group_by(sim) %>% 
#   mutate(rank = rank(total)) %>% 
#   ungroup()
# 
# fix_ties <- qual %>%
#   filter(!(rank %in% 1:nplay)) %>%
#   group_by(sim, rank) %>%
#   mutate(
#     tb1_prev = ifelse(e1 < lag(e1), 1, 0),
#     tb2_prev = ifelse(e2 < lag(e2), 1, 0),
#     tb3_prev = ifelse(e3 < lag(e3), 1, 0),
#     tb_prev = tb1_prev + tb2_prev + tb3_prev,
#     tb_prev = if_else(is.na(tb_prev), 0, tb_prev),
#     
#     tb1_next = ifelse(e1 < lead(e1), 1, 0),
#     tb2_next = ifelse(e2 < lead(e2), 1, 0),
#     tb3_next = ifelse(e3 < lead(e3), 1, 0),
#     tb_next = tb1_next + tb2_next + tb3_next,
#     tb_next = if_else(is.na(tb_next), 0, tb_next),
#     
#     tb = tb_prev + tb_next,
#     rank = ifelse(tb == 2, rank - 0.5, rank + 0.5)
#   ) %>% 
#   select(player:rank) %>% 
#   ungroup()
# 
# qual <- qual %>% 
#   filter(rank %in% 1:nplay) %>%
#   bind_rows(fix_ties) %>% 
#   arrange(sim, player)

