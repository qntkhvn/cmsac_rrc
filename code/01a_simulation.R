# Simulation study

# Packages
library(tidyverse)
theme_set(theme_minimal())
qual_dist_list <- final_dist_list <- list()

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
cor_vec <- seq(0,1,length = 5)

for (rrr in 1:length(cor_vec)){print(rrr)
qual <- climbing_sim_cor(nsim = 10000, nplay = 20, rho = cor_vec[rrr])
final <- climbing_sim_cor(nsim = 10000, nplay = 8, rho = cor_vec[rrr])

#correlation on the x axis vs advancement probability?  
#correlation on the x axis vs medal probability in the finals?    

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


final_dist_stack <- do.call(rbind,final_dist_list)
ggplot(aes(x = rho, y = value, colour = factor(rank)), data = subset(final_dist_stack,type == "Probability")) + geom_line()

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

