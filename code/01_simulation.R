# Simulation study

# Packages
library(tidyverse)
theme_set(theme_minimal())

# simulation, assuming uniform ranks
# this function takes in the number of simulations and players 
# and returns a simulated data frame with the following attributes: 
# player ID, the discipline ranks, overall score, final rank, sim number
climbing_sim <- function(nsim = 10000, nplay) {
  sims <- list()
  for (i in 1:nsim) {
    sims[[i]] <-
      bind_cols(
        player = 1:nplay,
        e1 = sample(1:nplay, replace = FALSE),
        e2 = sample(1:nplay, replace = FALSE),
        e3 = sample(1:nplay, replace = FALSE)
      ) %>%
      mutate(sim = i)
  }
  results <- bind_rows(sims) %>%
    mutate(score = e1 * e2 * e3) %>%
    group_by(sim) %>%
    mutate(rank = rank(score, ties.method = "random")) %>%
    ungroup()
  
  return(results)
}

# simulate qualification and final rounds
set.seed(1)
qual <- climbing_sim(nsim = 10000, nplay = 20)
final <- climbing_sim(nsim = 10000, nplay = 8)

# Question: Given that a climber wins any event, what's the probability...
# 1) of advancing to the final for a qualifier
# 2) of winning a medal for a finalist

# qualification distribution
qual_dist <- qual %>%
  filter(e1 == 1 | e2 == 1 | e3 == 1) %>%
  count(rank) %>%
  mutate(Probability = n / sum(n),
         Cumulative = cumsum(Probability)) %>%
  select(-n) %>%
  pivot_longer(!rank, names_to = "type") %>%
  mutate(round = "Qualification")

# final distribution
final_dist <- final %>%
  filter(e1 == 1 | e2 == 1 | e3 == 1) %>%
  count(rank) %>%
  mutate(Probability = n / sum(n),
         Cumulative = cumsum(Probability)) %>%
  select(-n) %>%
  pivot_longer(!rank, names_to = "type") %>%
  mutate(round = "Final")

# plot distributions, faceted by round
qual_dist %>%
  bind_rows(final_dist) %>%
  mutate(round = fct_relevel(round, "Qualification")) %>%
  ggplot(aes(rank, value, fill = type)) +
  geom_col(position = "dodge") +
  facet_wrap(~ round, scales = "free") +
  scale_x_reverse(breaks = 1:11) +
  coord_flip() +
  labs(x = "Rank",
       y = "Probability Density",
       fill = "Distribution") +
  scale_fill_manual(values = c("maroon", "midnightblue")) +
  theme(panel.grid.major.y = element_blank()) +
  theme(
    legend.position = "bottom",
    legend.margin = margin(-5),
    legend.key.size = unit(0.4, "cm")
  )


# Question: What's the expected score for each rank of both qualification and final

# get expected final scores
# also create color code for gold, silver, bronze
final_score <- final %>% 
  group_by(rank) %>% 
  summarize(avg_score = mean(score)) %>%
  mutate(round = "Final",
         color = ifelse(rank == 1, "gold",
                        ifelse(rank == 2, "#C0C0C0",
                               ifelse(rank == 3, "#A77044", "lightblue"))))

# get expected qualification scores (top 10)
# color code for finalists (top 8) and non-finalists
qual_score <- qual %>% 
  group_by(rank) %>% 
  summarize(avg_score = mean(score)) %>%
  filter(rank <= 10) %>% 
  mutate(round = "Qualification",
         color = ifelse(rank < 9, "maroon", "lightblue"))

# scoring visualization (faceted by round)
final_score %>% 
  bind_rows(qual_score) %>% 
  mutate(round = fct_relevel(round, "Qualification")) %>% 
  ggplot(aes(rank, avg_score, fill = color)) +
  geom_col() +
  geom_text(aes(label = ceiling(avg_score)), color = "black", size = 3, vjust = -0.3) +
  facet_wrap(~ round, scales = "free") +
  scale_x_continuous(breaks = 1:10) +
  scale_fill_identity() +
  labs(x = "Rank",
       y = "Average Score") +
  theme(panel.grid.major.x = element_blank())
