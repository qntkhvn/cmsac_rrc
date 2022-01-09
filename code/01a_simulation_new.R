# Simulation study
# 01/06/2022 - Hannah is adding in some comments so that she knows whats going on

# Packages
library(tidyverse)
theme_set(theme_minimal())
qual_dist_list <- final_dist_list <- list()

# HB - storing probabilities of speed specialists making finals/podium
speed_results_list_q <- speed_results_list_f <- BL_results_list_q <- BL_results_list_f <- list()

# simulation, assuming uniform ranks
# this function takes in the number of simulations and players 
# and returns a simulated data frame with the following attributes: 
# player ID, the discipline ranks, overall score, final rank, sim number
# Here we add in correlation between bouldering and lead. 
# We test 3 values 0.25, 0.5, and 0.75

climbing_sim_cor <- function(nsim = 10000, nplay, rho = 0) {
  sims <- list()
  #Number of points in the population 
  N <- 100000
  e1pop <- runif(N)
  e2pop <- runif(N)
  e3pop <- runif(N)
  
  #Pick some indexes
  ind <- sample(1:N, rho * N, replace = FALSE)
  e2pop[ind] <- sort(e2pop[ind])
  e3pop[ind] <- sort(e3pop[ind])
  
  for (i in 1:nsim) {
    
    #Sample nplay rows all together
    ind2 <- sample(1:N, nplay, replace = FALSE)
    
    sims[[i]] <-
      bind_cols(
        player = 1:nplay,
        e1 = rank(e1pop[ind2]),
        e2 = rank(e2pop[ind2]),
        e3 = rank(e3pop[ind2]), 
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
cor_vec <- seq(0, 1, length = 5)

for (rrr in 1:length(cor_vec)) {
  
  print(rrr)
  qual <- climbing_sim_cor(nsim = 10000, nplay = 20, rho = cor_vec[rrr])
  final <- climbing_sim_cor(nsim = 10000, nplay = 8, rho = cor_vec[rrr])
  
  #correlation on the x axis vs advancement probability?  
  #correlation on the x axis vs medal probability in the finals?    
  
  # HB - speed specialist results ------
  # probability that rank 1 in speed makes qualifiers
  speed_res_qual <- qual %>%
    group_by(e1) %>%
    summarize(finalist_prob = mean(rank <= 8)
              , rho = cor_vec[rrr]
              )
  
  # prob that rank 1 in bould/lead makes finals
  BL_res_qual <- qual %>%
    group_by(e2 == 1 | e3 == 1) %>%
    summarize(finalist_prob = mean(rank <= 8)
              , rho = cor_vec[rrr]
              )
  
  # probability that rank 1 in speed makes podium
  speed_res_fin <- final %>%
    group_by(e1) %>%
    summarize(podium_prob = mean(rank <= 3)
              , bronze_prob = mean(rank == 3)
              , silver_prob = mean(rank == 2)
              , gold_prob = mean(rank == 1)
              , rho = cor_vec[rrr]
              )
  
  # prob that rank 1 in bould/lead makes podium
  BL_res_fin <- final %>%
    group_by(BL = e2 == 1 | e3 == 1) %>%
    summarize(podium_prob = mean(rank <= 3)
              , bronze_prob = mean(rank == 3)
              , silver_prob = mean(rank == 2)
              , gold_prob = mean(rank == 1)
              , rho = cor_vec[rrr]
              )
  
  # store results in lists
  speed_results_list_q[[rrr]] <- speed_res_qual
  speed_results_list_f[[rrr]] <- speed_res_fin
  BL_results_list_q[[rrr]] <- BL_res_qual
  BL_results_list_f[[rrr]] <- BL_res_fin
  # -------------------------------------
  
  # qualification distribution
  qual_dist <- qual %>%
    filter(e1 == 1 | e2 == 1 | e3 == 1) %>%
    count(rank) %>%
    mutate(Probability = n / sum(n),
           Cumulative = cumsum(Probability)) %>%
    select(-n) %>%
    pivot_longer(!rank, names_to = "type") %>%
    mutate(round = "Qualification", rho = cor_vec[rrr])
  
  qual_dist_list[[rrr]] <- qual_dist
  
  # final distribution
  final_dist <- final %>%
    filter(e1 == 1 | e2 == 1 | e3 == 1) %>%
    count(rank) %>%
    mutate(Probability = n / sum(n),
           Cumulative = cumsum(Probability)) %>%
    select(-n) %>%
    pivot_longer(!rank, names_to = "type") %>%
    mutate(round = "Final", rho = cor_vec[rrr])
  
  
  final_dist_list[[rrr]] <- final_dist
}

# export sim results 
sim_corr_results <- bind_rows(qual_dist_list) %>% 
  bind_rows(final_dist_list)

# write_rds(sim_corr_results, "paper/sim_corr_results.rds")

# ggplot(aes(x = rho, y = value, color = factor(rank)), data = subset(final_dist_stack, type == "Probability")) + 
#   geom_point() +
#   geom_line()


bind_rows(final_dist_list) %>% 
  bind_rows(tibble(rank = 7, type = c("Probability", "Cumulative"), value = 0:1, round = "Final", rho = 1)) %>%
  filter(type == "Probability") %>% 
  ggplot(aes(x = rho, y = value, color = factor(rank))) +
  geom_point() +
  geom_line() +
  scale_y_continuous(breaks = seq(0, 0.5, 0.1)) +
  labs(color = "Rank",
       x = "Spearman Correlation",
       y = "Probability")


bind_rows(final_dist_list) %>% 
  count(rank)

# plot distributions, faceted by round
library(gganimate)
bind_rows(qual_dist_list) %>%
  bind_rows(final_dist_list) %>%
  mutate(round = fct_relevel(round, "Qualification")) %>%
  ggplot(aes(rank, value, fill = type)) +
  geom_col(position = "dodge") +
  facet_wrap(~ round, scales = "free") +
  scale_x_reverse(breaks = 1:12) +
  coord_flip() +
  labs(x = "Rank",
       y = "Probability Density",
       fill = "Distribution") +
  scale_fill_manual(values = c("maroon", "midnightblue")) +
  theme(panel.grid.major.y = element_blank()) +
  theme(legend.position = "bottom",
        legend.margin = margin(-5),
        legend.key.size = unit(0.4, "cm")) +
  transition_states(rho) +
  labs(title = "Spearman correlation between lead and bouldering: {closest_state}")
  

# Question: What's the expected score for each rank in both qualification and final?

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


### visualize speed results (probabilities of medalling in finals)
speed_final_results <- speed_results_list_f %>%
  do.call(rbind, .) %>%
  mutate(event = "speed") %>%
  filter(e1 == 1) %>%
  select(-e1)

BL_final_results <- BL_results_list_f %>%
  do.call(rbind, .) %>%
  mutate(event = "bould/lead") %>%
  filter(BL == TRUE) %>%
  select(-BL)

podium_probs <- rbind(speed_final_results, BL_final_results) %>%
  pivot_longer(podium_prob:gold_prob, names_to = "Medal") %>%
  mutate(Medal = ifelse(Medal == "gold_prob", "Gold", Medal)
         , Medal = ifelse(Medal == "silver_prob", "Silver", Medal)
         , Medal = ifelse(Medal == "bronze_prob", "Bronze", Medal)
         , Medal = ifelse(Medal == "podium_prob", "Any", Medal)
         )

podium_probs %>%
  filter(Medal %in% c("Gold", "Any")) %>%
  ggplot(aes(rho, value)) +
  geom_path(aes(color = Medal)) +
  geom_point(aes(color = Medal)) +
  facet_wrap(~event) +
  xlab("Spearman Correlation") +
  ylab("Medal Probability") +
  scale_color_manual(values = c("midnightblue", "darkgoldenrod1")) +
  ylim(c(0, 1))
