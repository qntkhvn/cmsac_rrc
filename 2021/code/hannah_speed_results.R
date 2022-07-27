# Empirical Analysis of Speed Climber Specialist Disadvantage

# Note: For now, a speed specialist will just be the person who ranks #1 in the speed discipline. 
# This can and should be refined, but maybe not for this paper.

pull_vars <- function(dat) {
  dat_w_vars <- dat %>%
    select(rank = contains("rank")
           , speed = contains("speed")
           , bould = contains("bould")
           , lead = contains("lead")
           , total = contains("total")
           )
}

# Finals data:
finals_dat_list <- list()

finals_data <- c("data/2018_asiad/men_final.csv"
                      , "data/2018_asiad/women_final.csv"
                      , "data/2018_world/men_final.csv"
                      , "data/2018_world/women_final.csv"
                      , "data/2018_youth_olympics/men_final.csv"
                      , "data/2018_world/women_final.csv"
                      , "data/2018_youth_olympics/men_final.csv"
                      , "data/2018_youth_olympics/women_final.csv"
                      , "data/2019_world/men_final.csv"
                      , "data/2019_world/women_final.csv"
                      , "data/2020_africa/men_final.csv"
                      , "data/2020_africa/women_final.csv"
                      , "data/2020_europe/men_final.csv"
                      , "data/2020_europe/women_final.csv"
                      , "data/2020_oceania/men.csv"
                      , "data/2020_oceania/women.csv"
                      ) 

for (i in 1:length(finals_data)) {
  finals_dat_list[[i]] <- read_csv(finals_data[i]) %>%
    pull_vars() %>%
    mutate(group = ifelse(i%%2, "M", "F")
           , source = finals_data[i]
           )
  
  if (i > (length(finals_data) - 2) ) {
    finals_dat_list[[i]] <- read_csv(finals_data[i]) %>%
      select(starts_with("f_")) %>%
      pull_vars() %>%
      mutate(group = ifelse(i%%2, "M", "F")
             , source = finals_data[i]
      )
  }
}

finals_data_all <- do.call(rbind, finals_dat_list)
finals_data_complete <- finals_data_all[complete.cases(finals_data_all),]


speed_wins <- finals_data_complete %>%
  group_by(source, group, speed) %>%
  summarize(podium = mean(rank <= 3)
            , bronze = mean(rank == 3)
            , silver = mean(rank == 2)
            , gold = mean(rank == 1)
            ) %>%
  group_by(group, speed) %>%
  summarize(prob_podium = mean(podium)
            , prob_bronze = mean(bronze)
            , prob_silver = mean(silver)
            , prob_gold = mean(gold)
            )

BL_wins <- finals_data_complete %>%
  group_by(source, group, bould == 1 | lead == 1) %>%
  summarize(BL_win = bould == 1 | lead == 1
            , podium = mean(rank <= 3)
            , bronze = mean(rank == 3)
            , silver = mean(rank == 2)
            , gold = mean(rank == 1)
  ) %>%
  group_by(group, BL_win) %>%
  summarize(prob_podium = mean(podium)
            , prob_bronze = mean(bronze)
            , prob_silver = mean(silver)
            , prob_gold = mean(gold)
  )

bould_wins <- finals_data_complete %>%
  group_by(source, group, bould) %>%
  summarize(podium = mean(rank <= 3)
            , bronze = mean(rank == 3)
            , silver = mean(rank == 2)
            , gold = mean(rank == 1)
  ) %>%
  group_by(group, bould) %>%
  summarize(prob_podium = mean(podium)
            , prob_bronze = mean(bronze)
            , prob_silver = mean(silver)
            , prob_gold = mean(gold)
  )

lead_wins <- finals_data_complete %>%
  group_by(source, group, lead) %>%
  summarize(podium = mean(rank <= 3)
            , bronze = mean(rank == 3)
            , silver = mean(rank == 2)
            , gold = mean(rank == 1)
  ) %>%
  group_by(group, lead) %>%
  summarize(prob_podium = mean(podium)
            , prob_bronze = mean(bronze)
            , prob_silver = mean(silver)
            , prob_gold = mean(gold)
  )
