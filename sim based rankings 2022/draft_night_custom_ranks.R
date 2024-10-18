#setup --------
library(easypackages)
packages("coach")
options(tidyverse.quiet = TRUE)
options(nflreadr.verbose = FALSE)
# Helper Functions, Filepaths & Chart Themes -----
dropbox = ifelse(Sys.info()['sysname'] == "Windows", "C:/Users/matts/dropbox/", "~/Dropbox/")
projects_folder = paste0(dropbox, "Matt Savoca/Projects/")
draft_night = paste0(dropbox, "Matt Savoca/Projects/NFL 2022/draft_night/")
source(paste0(dropbox, "Matt Savoca/Projects/theme_FE.R"))
source(paste0(projects_folder, "NFL 2022/ff_projections_gp_adjustments.R"))
source(paste0(projects_folder, "NFL 2022/ff_projections_sims.R"))
source(paste0(draft_night, "fp_latest_ranks.R"))
source(paste0(draft_night, "4for4_latest_ranks.R"))
source(paste0(draft_night, "etr_latest_ranks.R"))
source(paste0(draft_night, "ffballers_latest_ranks.R"))
source(paste0(draft_night, "pff_latest_ranks.R"))
theme_set(theme_light())

tic()

# custom ranks -----
custom_ranks = ffballers_ranks %>%
  full_join(ranks_4for4) %>%
  full_join(etr_ranks) %>%
  full_join(pff_ranks) %>%
  select(
    player, pos, `4for4_pos_rank`, etr_pos_rank, pff_pos_rank, ffballers_pos_rank
  ) %>%
  pivot_longer(
    c(-player, -pos), values_to = "rank"
  ) %>%
  group_by(player, pos) %>%
  summarize(
    custom_ecr = mean(rank, na.rm = T), 
    custom_sd = sd(rank, na.rm = T), 
    .groups = "drop")


draft_night_ranks = 
  fp_ranks %>% as.data.frame() %>%
    left_join(
      custom_ranks %>% select(player, custom_ecr, custom_sd)
    ) %>%
  mutate(
    ecr = if_else(is.na(custom_ecr), ecr, custom_ecr),
    sd = if_else(is.na(custom_sd), sd, custom_sd),
  ) %>%
  select(-contains("custom")) %>%
  group_by(pos) %>%
  mutate(pos_adp = min_rank(ecr)) %>%
  ungroup()
  



# sims ------


adp_outcomes = ffs_adp_outcomes(
  scoring_history = ff_scoringhistory(conn = conn, season = 2013:2021)
)

adjusted_adp_outcomes = adjust_adp_outcomes(adp_outcomes = adp_outcomes)

fffl_adp_outcomes = adjusted_adp_outcomes %>% 
  select(pos, rank, prob_gp, mean_score, inj_adj_mean_score) %>% 
  distinct() %>% 
  #filter(pos != "QB" | rank < 24, pos != "TE" | rank < 16, pos != "WR" | rank <= 55, pos != "RB" | rank <= 60) %>%
  left_join(fffl_values %>% rename(pos = position, rank = pos_adp)) %>%
  mutate(
    adj_dpp = x_av/mean_score
  )



# multiseason functions -----

season_lineup_sim = function(test_values, fffl_values, n_lineups = 1, n_salary = 190, exposure = .5, player_locks = NULL, ...){
  message("creating df") 
  coach_values = test_values %>% 
    transmute(
      player_id = fantasypros_id,
      #player = paste0(pos, pos_adp),
      player,
      team, 
      position = pos,
      fpts_proj = projection,
      sd_proj = sd_projection,
      pos_adp = pos_adp %>% as.numeric()
    ) %>%
    mutate(
      fpts_proj = replace_na(fpts_proj, 0)
    )
  
  message("getting AVs")
  coach_proj = coach_values %>%
    left_join(
      fffl_values, by = c("position","pos_adp")
    ) %>%
    mutate(
        team = replace_na(team, "FA")
    ) %>%
    rowwise() %>%
    mutate(
      x_av = rnorm(1, x_av, 2),
      x_av = round_half_up(x_av) %>% as.integer(),
    ) %>%
    ungroup() %>%
    arrange(player_id) %>%
    mutate(
      row_id = row_number(),
      x_av = case_when(
        is.na(x_av) ~ as.integer(1),
        x_av < 1 ~ as.integer(1),
        T ~ x_av)
    ) %>%
    select(
      row_id,
      player_id,
      player,
      team,
      position,
      salary = x_av,
      fpts_proj
    ) %>%
    filter(
      position != "QB" | player %in% c("Patrick Mahomes II", "Lamar Jackson", "Justin Herbert", 
                                  "Josh Allen", "Kyler Murray", "Jalen Hurts", "Dak Prescott", 
                                  "Tom Brady", "Joe Burrow", "Aaron Rodgers", 
                                  "Matthew Stafford", "Trey Lance", "Derek Carr", 
                                  "Kirk Cousins", "Tua Tagovailoa", "Daniel Jones", 
                                  "Matt Ryan","Trevor Lawrence", "Justin Fields", 
                                  "Ryan Tannehill", "Jared Goff", "Carson Wentz")
    ) %>%
    filter(
      position != "TE" | player %in% c("Travis Kelce", "Mark Andrews", "Kyle Pitts", "Darren Waller", "George Kittle", 
      "Dalton Schultz", "T.J. Hockenson", "Dallas Goedert", "Dawson Knox", "Zach Ertz", "Cole Kmet", "Pat Freiermuth", "Albert Okwuegbunam", "Hunter Henry", "Noah Fant")
    ) %>%
    ungroup()
  

  
  message("creating model")
  fffl_model <- model_generic(coach_proj, 
                              #total_salary = rnorm(185, 5, n = 1), 
                              total_salary = n_salary,
                              roster_size = 10, 
                              max_from_team = 10, ...)
  
  #randomness <- function(x) rnorm(nrow(coach_values), x, coach_values$sd_proj)
  
  message("defining constraints")
  constraints <- list(
    "QB" = 1,
    "RB" = 2,
    "WR" = 2,
    "TE" = 1,
    "RB/WR/TE" = 3,
    "QB/RB/WR/TE" = 1
  )
  
  lock_value = coach_proj %>% 
    filter(player %in% c("Trey Lance")) %>% 
    pull(row_id) 
  
  lock_ct = lock_value %>%
    length()
  
  message(lock_ct)
  
  fffl_model <- add_generic_positions_constraint(fffl_model, coach_proj, constraints)
  
  message("creating lineup(s)")
  plan("multisession")
  model_results = optimize_generic(coach_proj, fffl_model, L = n_lineups, locks = lock_value, max_exposure = exposure, ...)
  
  message("summarizing results")
  results = data.table::rbindlist(model_results, idcol = "lineup") %>%
    mutate(lineups = max(lineup)) %>%
    group_by(player) %>%
    mutate(exposure = n()/max(lineups)) %>%
    group_by(lineup) %>%
    mutate(player_sal_rk  = rank(-salary, ties.method= "random"),
           lineup_score = sum(fpts_proj),
           top_2_spend = sum(salary[player_sal_rk <= 2]),
           top_2_score = sum(fpts_proj[player_sal_rk <= 2]),
           top_3_spend = sum(salary[player_sal_rk <= 3]),
           top_3_score = sum(fpts_proj[player_sal_rk <= 3]),
           mid_3_spend = sum(salary[player_sal_rk <= 6 & player_sal_rk >= 4]),
           mid_3_sscore = sum(fpts_proj[player_sal_rk <= 6 & player_sal_rk >= 4]),
           top_4_spend = sum(salary[player_sal_rk <= 4]),
           top_4_score = sum(fpts_proj[player_sal_rk <= 4]),
           top_5_spend = sum(salary[player_sal_rk <= 5]),
           top_5_score = sum(fpts_proj[player_sal_rk <= 5]),
           players = trimws(paste(unique(player),collapse = ", ")),
           start = trimws(paste(unique(player[player_sal_rk <= 2]),collapse = ", "))) %>%
    ungroup() %>%
    nest()
}
multiseason_lineup_sim = function(test_values, ...){
  suppressWarnings({
    season_values = 
      test_values %>%
      rename(season_data = data) %>%
      mutate(rosters = map(season_data, function(x){season_lineup_sim(x, fffl_values = fffl_values,...)})) %>%
      unnest(rosters) %>%
      unnest(data) %>%
      select(-lineups, -exposure) %>%
      group_by(player_id, player, team, position) %>%
      mutate(uses = n()) %>%
      ungroup() %>%
      arrange(-uses)
  })
  
  return(season_values)
}

multisim_summary = function(season_values){
  season_values %>%
    group_by(player_id, player, team, position, uses) %>%
    summarize_if(is.numeric, mean) %>%
    ungroup() %>%
    arrange(-uses)
}

# run the sims --------
N_LINEUPS = 1
N_SEASONS = 500


tic()
proj = ffs_generate_projections(adp_outcomes, draft_night_ranks, weeks = 1:14, n_seasons = N_SEASONS)




test_values = proj %>%
  mutate(bye = if_else(is.na(bye), 18, bye),
         flex = if_else(pos %in% c("RB", "WR", "TE"), T, F),
         projected_score = projection * gp_model *(week != bye)) %>% 
  left_join(draft_night_ranks %>% select(fantasypros_id, ecr, pos_adp)) %>% 
  group_by(fantasypros_id, player, pos, team, pos_adp, season) %>%
  summarize(
    projection = mean(projection, na.rm = T),
    sd_projection = sd(projected_score)
  ) %>%
  group_by(season) %>%
  nest() %>%
  ungroup()
toc()

tic()

multiseason_sim_raw = multiseason_lineup_sim(test_values[1:2,],  n_salary = 191, exposure = 1, n_lineups = N_LINEUPS)
#studs_sim_raw = suppressMessages(multiseason_lineup_sim(test_values,  n_salary = 195, n_lineups = N_LINEUPS))

multiseason_sim_raw  %>%
  group_by(season, lineup) %>%
  mutate(
    rbs =  trimws(paste(unique(player[position == "TE"]),collapse = ", ")),
    rb_sal = sum(salary[position == "RB"]),
    wr_sal = sum(salary[position == "WR"]),
    te_sal = sum(salary[position == "TE"]),
    qb_sal = sum(salary[position == "QB"]),
    rb_ct = n_distinct(player[position == "RB"]),
    wr_ct = n_distinct(player[position == "WR"]),
    te_ct = n_distinct(player[position == "TE"]),
    qb_ct = n_distinct(player[position == "QB"]),
    ) %>%
  ungroup() %>%
  group_by(rbs) %>%
  summarize(n = n()/10, salary = mean(salary[position == "TE"])) %>% arrange(-n)

multiseason_sim = multiseason_sim_raw %>%
  group_by(season, lineup) %>%
  mutate(
    rbs =  trimws(paste(unique(player[position == "RB"]),collapse = ", ")),
    rb_sal = sum(salary[position == "RB"]),
    wr_sal = sum(salary[position == "WR"]),
    te_sal = sum(salary[position == "TE"]),
    qb_sal = sum(salary[position == "QB"]),
    rb_ct = n_distinct(player[position == "RB"]),
    wr_ct = n_distinct(player[position == "WR"]),
    te_ct = n_distinct(player[position == "TE"]),
    qb_ct = n_distinct(player[position == "QB"]),
  ) %>%
  ungroup() %>%
  multisim_summary() %>% 
  mutate(exposure = uses/(N_LINEUPS*N_SEASONS)) %>% 
  select(player_id, player, team, position, exposure, everything())
plan("sequential")
toc()


# post-acution -----
rosters = ffs_rosters(conn)
constraints = ffs_starter_positions(conn)
league_info = ff_league(conn)

roster_scores <- ffs_score_rosters(
  projected_scores = proj,
  rosters = rosters
)



optimal_scores <- ffs_optimise_lineups(
  roster_scores = roster_scores,
  lineup_constraints = constraints,
  lineup_efficiency_mean = 0.775,
  lineup_efficiency_sd = 0.05,
  best_ball = FALSE, # or TRUE
  pos_filter = c("QB","RB","WR","TE","K")
)

schedules <- ffs_build_schedules(
  n_seasons = N_SEASONS,
  n_weeks = 17,
  seed = NULL,
  franchises = ffs_franchises(conn)
)

summary_week <- ffs_summarise_week(optimal_scores, schedules)
summary_season <- ffs_summarise_season(summary_week)
summary_simulation <- ffs_summarise_simulation(summary_season)


#scratchwork -----
testlock_sim_raw = multiseason_lineup_sim(test_values[1:3,],  n_salary = 181, n_lineups = 3, exposure = 1)
testlock_sim = testlock_sim_raw %>% multisim_summary() %>% mutate(exposure = uses/(N_LINEUPS*N_SEASONS)) %>% select(player_id, player, team, position, exposure, everything())
plan("sequential")
toc()



# single season sim -----


proj_metrics = proj %>%
  mutate(bye = if_else(is.na(bye), 18, bye),
         flex = if_else(pos %in% c("RB", "WR", "TE"), T, F),
         projected_score = projection * gp_model *(week != bye)) %>%
  generate_projection_metrics(flex_baseline_player = 132, skill_usable_rank = 36, qb_usable_rank = 24) %>%
  distinct() 

proj_values = proj_metrics %>%
  left_join(draft_night_ranks %>% select(fantasypros_id, ecr)) %>%
  generate_player_values(te_baseline_player = 18, qb_baseline_player = 24, flex_baseline_player = 132) %>%
  select(player, pos, team, usable_value, elite_value, everything()) %>%
  group_by(pos) %>%
  mutate(pos_adp = min_rank(ecr)) %>%
  ungroup()
  

coach_values = proj_values %>% 
  transmute(
    row_id = row_number(),
    player_id = fantasypros_id,
    #player = paste0(pos, pos_adp),
    player,
    team, 
    position = pos,
    fpts_proj = projection,
    sd_proj = sd_projection,
    pos_adp = pos_adp %>% as.numeric()
  ) %>%
  mutate(
    fpts_proj = replace_na(fpts_proj, 0)
  )

coach_proj = coach_values %>%
  left_join(
    fffl_values, by = c("position","pos_adp")
  ) %>%
  mutate(
    x_av = case_when(
      is.na(x_av) ~ 1,
      x_av < 1 ~ 1,
      T ~ x_av
    ) %>%
    team = replace_na(team, "FA")
  ) %>%
  rowwise() %>%
  mutate(
    x_av = rnorm(1, x_av, 2),
    x_av = round_half_up(x_av) %>% as.integer(),
     ) %>%
  ungroup() %>%
  select(
    row_id,
    player_id,
    player,
    team,
    position,
    salary = x_av,
    fpts_proj
  ) %>%
  ungroup()


fffl_model <- model_generic(coach_proj, 
                            #total_salary = rnorm(185, 5, n = 1), 
                            total_salary = 190, 
                            roster_size = 10, 
                            max_from_team = 10)

randomness <- function(x) rnorm(nrow(coach_values), x, coach_values$sd_proj)

constraints <- list(
  "QB" = 1,
  "RB" = 2,
  "WR" = 2,
  "TE" = 1,
  "RB/WR/TE" = 3,
  "QB/RB/WR/TE" = 1
)

fffl_model <- add_generic_positions_constraint(fffl_model, coach_proj, constraints)


plan("multisession")
model_results = optimize_generic(coach_proj, fffl_model, L = 20, max_exposure = .75)


results = data.table::rbindlist(model_results, idcol = "lineup") %>%
  mutate(lineups = max(lineup)) %>%
  group_by(player) %>%
  mutate(exposure = n()/max(lineups)) %>%
  group_by(lineup) %>%
  mutate(player_sal_rk  = rank(-salary, ties.method= "random"),
         lineup_score = sum(fpts_proj),
         top_2_spend = sum(salary[player_sal_rk <= 2]),
         top_2_score = sum(fpts_proj[player_sal_rk <= 2]),
         top_3_spend = sum(salary[player_sal_rk <= 3]),
         top_3_score = sum(fpts_proj[player_sal_rk <= 3]),
         mid_3_spend = sum(salary[player_sal_rk <= 6 & player_sal_rk >= 4]),
         mid_3_sscore = sum(fpts_proj[player_sal_rk <= 6 & player_sal_rk >= 4]),
         top_4_spend = sum(salary[player_sal_rk <= 4]),
         top_4_score = sum(fpts_proj[player_sal_rk <= 4]),
         top_5_spend = sum(salary[player_sal_rk <= 5]),
         top_5_score = sum(fpts_proj[player_sal_rk <= 5]),
         players = trimws(paste(unique(player),collapse = ", "))) %>%
  ungroup()

results %>%
  select(
    position,
    player_id,
    player,
    exposure
  ) %>%
  distinct() %>%
  arrange(-exposure) %>%
  mutate(player = reorder(player, exposure)) %>%
  ggplot()+
  aes(exposure, player)+
  geom_col()+
  facet_wrap(position ~ ., scales = "free_y")

plan("sequential")
toc()


