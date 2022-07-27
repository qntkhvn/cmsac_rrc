


# How likely is for a speed specialist to make it to finals and the podium

# if you win the first event, what's your prob of qualifying and winning the medal

library(tidyverse)
theme_set(theme_minimal())

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
qual <- final <- list()
for (c in 1:length(cor_vec)) {
  
  qual[[c]] <- climbing_sim_cor(nsim = 10000, nplay = 20, rho = cor_vec[c]) %>% 
    mutate(rho = cor_vec[c], round = "Qualification")
  
  final[[c]] <- climbing_sim_cor(nsim = 10000, nplay = 8, rho = cor_vec[c]) %>% 
    mutate(rho = cor_vec[c], round = "Final")
}  

sim_results <- bind_rows(qual) %>% 
  bind_rows(final)

# write_csv(sim_results, "paper/sim_results.csv")

sim_results %>% 
  group_by(rho, round) %>% 
  filter(e1 == 1) %>%
  count(rank) %>%
  mutate(Probability = n / sum(n),
         Cumulative = cumsum(Probability)) %>%
  ungroup() %>% 
  select(-n) %>% 
  pivot_longer(Probability:Cumulative, names_to = "prob") %>% 
  mutate(type = "Win Speed") %>% 
  bind_rows(
    sim_results %>% 
      group_by(rho, round) %>% 
      filter(e2 == 1 | e3 == 1) %>%
      count(rank) %>%
      mutate(Probability = n / sum(n),
             Cumulative = cumsum(Probability)) %>%
      ungroup() %>% 
      select(-n) %>% 
      pivot_longer(Probability:Cumulative, names_to = "prob") %>% 
      mutate(type = "Win Bouldering or Lead")
  ) %>% 
  filter(rank == 1 & prob == "Cumulative") %>% 
  write_rds("paper/sim_results_plot.rds")
