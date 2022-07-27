# Hannah Butler
# 07-09-2021
# Olympic Sport Climbing Scoring
#################### DESCRIPTION #########################
# estimate the probability of a medalist being displaced if another competitor drops out
# estimate the probability of a change in rankings if a competitor is removed
#################### NOTES ###############################
# write code to deal with ties 
# tiebreaker: determined by sum of medalist rankings (super rudimentary, update)
# in case of continued tie it doesn't matter, given the random nature of the ranks
# how often does someone who outperformed in 2 events have a lower rank?
#################### LIBRARIES ###########################
library(tidyverse)
library(ggplot2)
#################### SET CONSTANTS #######################
#
#################### VISUALIZE ###########################
# just look at this part
load("R/medal_displacement_probabilities.rdata")
load("R/top3_displace_probs.rdata")

# densities for probability of displacement, by medal & dropped rank
displacement_probs %>%
  ggplot() +
  geom_density(aes(x = displ_prob, fill =  medal), alpha = 0.3) +
  facet_wrap(~dropped_rank)

# densities for probability of displacement, by dropped rank
displace_probs %>%
  ggplot() +
  geom_density(aes(x = displ_prob, group = dropped_rank, fill = as.factor(dropped_rank)), alpha = 0.3)

#################### DEFINE FUNCTIONS ####################
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Assign Ranks 
assign_ranks <- function(c = 8) {
  # randomly assign ranks in 3 events & compute total
  # ranking in an event is considered to be independent from ranking in another event
  output <- data.frame(climber = sample(LETTERS, c, replace = FALSE)
                       , speed = sample(1:c, c, replace = FALSE)
                       , bould = sample(1:c, c, replace = FALSE)
                       , lead = sample(1:c, c, replace = FALSE)
                       ) %>%
    mutate(total = speed*bould*lead) %>%
    arrange(total) %>% # some questionable tie-breaking code
    mutate(m_rank_speed = ifelse(total %in% total[1:3] , rank(speed[1:3]), 0)
           , m_rank_bould = ifelse(total %in% total[1:3] , rank(bould[1:3]), 0)
           , m_rank_lead = ifelse(total %in% total[1:3] , rank(lead[1:3]), 0)
           , tiebreaker = m_rank_speed + m_rank_bould + m_rank_lead
           ) %>%
    arrange(total
            , tiebreaker
            )
  
  return(output)
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Remove a Climber from ranked data
drop_climber <- function(results, r = 4) {
  # drop a competitor and recompute rankings and totals
  # r = which rank competitor to drop
  ud_results <- results[-r, ] %>%
    mutate(speed = rank(speed)
           , bould = rank(bould)
           , lead = rank(lead)
           , total = speed*bould*lead
           ) %>%
    arrange(total)%>%
    mutate(m_rank_speed = ifelse(total %in% total[1:3], rank(speed[1:3]), 0)
           , m_rank_bould = ifelse(total %in% total[1:3], rank(bould[1:3]), 0)
           , m_rank_lead = ifelse(total %in% total[1:3], rank(lead[1:3]), 0)
           , tiebreaker = m_rank_speed + m_rank_bould + m_rank_lead
           ) %>%
    arrange(total
            , tiebreaker
            )
  return(ud_results)
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Simulate Ranks & Dropped Climber Reranks
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Reorder of all above competitors
displace_climber <- function(nsim = 1000, drop_rank = 4) {
  # simulate nsim rankings
  rank_perms <- lapply(1:nsim, function(x) assign_ranks())
  # drop ith competitor and recalculate ranks
  dropout_ranks <- lapply(rank_perms, function(x) drop_climber(x, r = drop_rank))
  # compare 1st - (r-1)th place & calculate % not matched
  pct_displ <- sapply(1:nsim, 
                      function(x) {
                        sum(rank_perms[[x]]$climber[1:(drop_rank - 1)] != dropout_ranks[[x]]$climber[1:(drop_rank - 1)]) > 0
                      }
                      ) %>%
    sum()/nsim
  
  return(pct_displ)
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Reorder of medalists
displace_medalist <- function(nsim = 1000, drop_rank = 4) {
  # simulate nsim rankings
  rank_perms <- lapply(1:nsim, function(x) assign_ranks())
  # drop ith competitor and recalculate ranks
  dropout_ranks <- lapply(rank_perms, function(x) drop_climber(x, r = drop_rank))
  # compare 1st-3rd place & calculate % not matched
  pct_displ <- sapply(1:nsim, 
                      function(x) {
                        sum(rank_perms[[x]]$climber[1:3] != dropout_ranks[[x]]$climber[1:3]) > 0
                      }
                      ) %>%
    sum()/nsim
  
  return(pct_displ)
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> CHANGE of finalists (permutations do not count)
displace_finalist <- function(nsim = 100, drop_rank = 9) {
  # simulate nsim rankings
  rank_perms <- lapply(1:nsim, function(x) assign_ranks(c = 20))
  # drop ith competitor and recalculate ranks
  dropout_ranks <- lapply(rank_perms, function(x) drop_climber(x, r = drop_rank))
  # compare 1st-3rd place & calculate % not matched
  pct_displ <- sapply(1:nsim, 
                      function(x) {
                        sum(rank_perms[[x]]$climber[1:8] %in% dropout_ranks[[x]]$climber[1:8]) < 8
                      }
  ) %>%
    sum()/nsim
  
  return(pct_displ)
}
#################### BEGIN CODE ####################
set.seed(80085)

pct_dispace_medalists <- lapply(4:8, function(x) replicate(100, displace_climber(drop_rank = x)))
names(pct_dispace_medalists) <- c("drop_r4", "drop_r5", "drop_r6", "drop_r7", "drop_r8")

pct_displace_finalists <- lapply(9:20, function(x) replicate(10000, displace_finalist(drop_rank = x)))
names(pct_displace_finalists) <- paste0("drop", 9:20)
save(pct_displace_finalists, file = "R/drop_qualifier_sims.rdata")
####################################
# format data
reorder_all_above <- lapply(4:8
                  , function(x) {
                    data.frame(dropped_rank = x, displ_prob = pct_displace_medalists[[x-3]])
                    }
                  ) %>%
  do.call(rbind, .)

#save(reorder_all_above, file = "R/reorder_all_above_probs.rdata")
#save(displace_probs, file = "R/top3_displace_probs.rdata")
#save(displacement_probs, file = "R/medal_displacement_probabilities.rdata")