#intall packages -----
library(easypackages)

packages(
  "tidyverse",
  "ffsimulator",
  "data.table",
  "ffscrapr",
  "ggridges",
  "future",
  "usemodels",
  "tidymodels",
  "finetune",
  "tidytext",
  "doParallel",
  "tictoc",
  "plotly",
  "ggridges",
  "htmlwidgets",
  "janitor",
  "ggrepel",
  "coach",
  prompt = F
)

options(tidyverse.quiet = TRUE)
options(nflreadr.verbose = FALSE)
# Helper Functions, Filepaths & Chart Themes -----
dropbox = ifelse(Sys.info()['sysname'] == "Windows", "C:/Users/matts/dropbox/", "~/Dropbox/")
projects_folder = paste0(dropbox, "Matt Savoca/Projects/")
downloads_folder = paste0(dropbox, "Matt Savoca/Droploads/")
source(paste0(dropbox, "Matt Savoca/Projects/theme_FE.R"))
theme_set(theme_light())





# add projection module -----


source("ff_projections_name_switches.R")
source("ff_projections_gp_adjustments.R")
source("ff_projections_file_imports.R")
source("ff_projections_helper_functions.R")





# Custom Premium Ranks (Projectionator) -----

generate_custom_premium_ranks = function(ud_ranks, awesemo_ranks, etr_ranks,
                                         custom_rank_influence = 1,
                                         awesemo_qb_influence = .5,
                                         awesemo_rb_influence = .5,
                                         awesemo_wr_influence = .5,
                                         awesemo_te_influence = .5,
                                         etr_qb_influence = .5,
                                         etr_rb_influence = .5,
                                         etr_wr_influence = .5,
                                         etr_te_influence = .5, ...){
  custom_ranks = 
    ud_ranks %>%
    left_join(awesemo_ranks %>% select(ud_id, contains("awesemo"))) %>%
    left_join(etr_ranks %>% select(ud_id, contains("etr"))) %>%
    select(player, pos, ud_id, pos_adp, awesemo_rank, etr_rank) %>%
    mutate(
      pos_adp = as.numeric(pos_adp),
      awesemo_inf = case_when(
        pos == "QB" ~ awesemo_qb_influence,
        pos == "RB" ~ awesemo_rb_influence,
        pos == "WR" ~ awesemo_wr_influence,
        T ~ awesemo_te_influence
      ),
      etr_inf = case_when(
        pos == "QB" ~ etr_qb_influence,
        pos == "RB" ~ etr_rb_influence,
        pos == "WR" ~ etr_wr_influence,
        T ~ etr_te_influence
      )) %>%
    group_by(pos) %>%
    mutate(
      awesemo_max_pos_rank = 30 + max(awesemo_rank, na.rm = T) %>% as.numeric(),
      etr_max_pos_rank = 30 + max(etr_rank, na.rm = T) %>% as.numeric()
    ) %>%
    ungroup() %>%
    mutate(
      awesemo_rank = case_when(
        is.na(awesemo_rank) ~ awesemo_max_pos_rank, 
        T ~ as.numeric(awesemo_rank)),
      awesemo_rank = if_else(awesemo_rank == awesemo_max_pos_rank & pos_adp > awesemo_rank, pos_adp, awesemo_rank),
      etr_rank = case_when(
        is.na(etr_rank) ~ etr_max_pos_rank, 
        T ~ as.numeric(etr_rank)),
      etr_rank = if_else(etr_rank == etr_max_pos_rank & pos_adp > etr_rank, pos_adp, etr_rank),
    ) %>%
    rowwise() %>%
    mutate(
      custom_rank = ((awesemo_rank * awesemo_inf) + (etr_rank * etr_inf))*custom_rank_influence + pos_adp*(1-custom_rank_influence),
      custom_sd = sd(c(awesemo_rank, etr_rank), na.rm = T) %>% round(2)
    ) %>%
    ungroup() %>%
    select(-contains(c("max",
                       "awesemo",
                       "etr",
                       "pos_adp")
    ))
  return(custom_ranks)
}




# Custom FP Rankings -----

generate_custom_rankings = function(latest_rankings_raw, custom_ranks, custom_influence, ud_ranks, ...){
  latest_rankings_raw %>%
    group_by(pos) %>%
    mutate(rank = min_rank(ecr)) %>%
    ungroup() %>%
    mutate(player = fp_to_underdog_name_switch(player),
           pos = case_when(
             player == "Taysom Hill" ~ "TE",
             player == "Ty Montgomery" ~ "WR",
             player == "J.J. Arcega-Whiteside" ~ "TE",
             player == "Giovanni Ricci" ~ "RB",
             T ~ pos
           )) %>%
    full_join(custom_ranks) %>%
    mutate(
      ecr = case_when(
        is.na(custom_rank) ~ ecr, 
        is.na(ecr) ~ as.numeric(custom_rank),
        T ~ (1-custom_influence)*ecr+custom_influence*custom_rank), 
      sd = case_when(
        is.na(sd) ~ case_when(pos == "QB" ~ 3.0, pos == "TE" ~ 5, pos == "RB" ~ 12, T ~ 11),
        T ~ sd
      )) %>% 
    select(-ud_id) %>%
    mutate(player = awesemo_to_ff_nameadjust(player = player)) %>% 
    left_join(ud_ranks, by = c("player", "pos")) %>%
    mutate(
      fantasypros_id = if_else(
        is.na(fantasypros_id), ud_id, fantasypros_id
      ),
      scrape_date = lubridate::today()
    ) %>%
    filter(!player %in% c("John Lovett"))
}


# League Settings and Info ----
get_UD_league_settings = function(id = 802734610873188352, season = 2022, ...){
  UD_12TM = id
  conn =  sleeper_connect(season = 2022, league_id = UD_12TM)
  return(conn)
}


get_UD_league_info = function(best_ball = T, ...){
  league_info = ffscrapr::ff_league(conn) %>% mutate(best_ball = best_ball)
  return(league_info)
}

# ADP-Based Outcomes -----
# Custom ADP Outcomes
generate_custom_adp_outcomes = function(conn){
  message("generating scoring history....")
  
  options(dplyr.summarise.inform = FALSE)
  # Scoring History 
  scoring_history = ff_scoringhistory(conn)
  
  
  # Latest Rankings 
  
  ## FantasyPros Raw Rankings
  message("Done!")
  message("accessing latest rankings...")
  
  latest_rankings_raw = ffs_latest_rankings()
  
  # Historical ADP Outcomes 
  message("Done!")
  message("Accessing historical ADP outcomes...")
  
  adp_outcomes = ffs_adp_outcomes(
    scoring_history = scoring_history,
    gp_model = "simple" # or "none"
  )
  
  return(adp_outcomes)
  
}


adjust_adp_outcomes = function(adp_outcomes){
  adj_adp_outcomes = adp_outcomes %>%
    rowwise() %>%
    mutate(mean_score = week_outcomes %>% unlist() %>% mean(),
           inj_adj_mean_score = mean_score * prob_gp) %>%
    ungroup() %>%
    unnest(c(player_name, fantasypros_id)) %>%
    #unnest(week_outcomes) %>%
    #select(-fantasypros_id, -player_name) %>%
    distinct()
  
  return(adj_adp_outcomes)
  
}

# Custom Simulations ----

generate_custom_projections = function(adp_outcomes, latest_rankings, weeks = 1:17, n_seasons){
  ffsimulator::ffs_generate_projections(
    adp_outcomes, latest_rankings, weeks = weeks, n_seasons = n_seasons) %>% 
    as.data.frame() %>%
    mutate(bye = if_else(is.na(bye), 18, bye),
           flex = if_else(pos %in% c("RB", "WR", "TE"), T, F),
           projected_score = projection * gp_model *(week != bye))
}


# Simulation Metrics -----
generate_projection_metrics = function(proj, flex_baseline = 218,
                                       te_flexmax_pct = .7, te_usable_rank = 6,
                                       qb_baseline = 30, qb_flexmax_pct = .75, qb_usable_rank = 12,
                                       skill_posmax_pct = .9, skill_usable_rank = 24,
                                       te_baseline_player = 12,
                                       qb_baseline_player = 24,
                                       flex_baseline_player = 60){
  proj %>%
    group_by(season, week, pos) %>%
    mutate(weekly_pos_rank = min_rank(-projected_score),
           max_projected_score = max(projected_score)) %>%
    group_by(season, week, flex) %>% # Get Netrics to compare scores to FLEX baselines
    mutate(
      weekly_flex_rank = min_rank(-projected_score),
      flex_baseline = nth(projected_score, order_by = desc(projected_score), flex_baseline),
      qb_baseline = nth(projected_score, order_by = desc(projected_score), qb_baseline),
      max_flex_projection = max(projected_score),
    ) %>%
    ungroup() %>% 
    mutate(
      vbd_baseline = if_else(flex, flex_baseline, qb_baseline), # value above 0: score above replacement value
      weekly_value = projected_score - vbd_baseline, #surplus value above replacement
      baseline_week = weekly_value >= 0, # T/F was score above replacement?
      pos_optimal = projected_score == max_projected_score, #was player the optimal at their pos group
      pct_of_pos_max = projected_score/max_projected_score, #what pct of the positional max score did player produce
      pct_of_flex_max = projected_score/max_flex_projection, #what pct of the flex max score did player produce (if top score: 1)
      elite_week = if_else( #was the week an "Elite" Week (
        #A: Top Overall player at position and Player Produces 70% of max FLEX (or QB) score
        #B: Score 75% of the FLEX max (Excluding QB)
        #C: Top 2 At Position (Excluding TE)
        #D: Score 90% of Positional Max (Excluding TE))
        (weekly_pos_rank == 1 & pct_of_flex_max > te_flexmax_pct)|
          (pos != "QB" & pct_of_flex_max>= qb_flexmax_pct )|
          (pos != "TE" & (weekly_pos_rank <= 3 | pct_of_pos_max >= skill_posmax_pct)), 
        1, 0),
      usable_week = if_else( #was the week an "Usable" Week (
        #A: Top-12 QB
        #B: Top-6 TE
        #C: Top-24 WR or RB
        (pos %in% c("QB") & weekly_pos_rank <= qb_usable_rank) |
          (pos %in% c("TE") & weekly_pos_rank <= te_usable_rank) |
          (pos %in% c("WR","RB") & weekly_pos_rank <= skill_usable_rank), 1, 0)
    ) %>% 
    select(-flex_baseline, -qb_baseline) %>%
    group_by(player, fantasypros_id, flex, pos, team) %>%
    summarize(
      injury_prob = 1-mean(gp_model),
      x_games = 16 * (1-injury_prob),
      x_games=  x_games_adj(player, x_games),
      #x_games = x_games_adj(player, x_games),
      value = mean(weekly_value),
      lo_value = mean(weekly_value) - sd(weekly_value)/2,
      hi_value = mean(weekly_value) + sd(weekly_value)/2,
      lo_proj = mean(projected_score) - sd(projected_score)/2,
      projection = mean(projected_score),
      sd_projection = sd(projected_score),
      hi_proj = mean(projected_score) + sd(projected_score)/2,
      lo_elite = mean(elite_week) - sd(elite_week)/2,
      elite_rate = mean(elite_week, na.rm =T),
      hi_elite = mean(elite_week) + sd(elite_week)*2,
      lo_usable = mean(usable_week, na.rm =T)- sd(usable_week, na.rm =T)/2,
      usable_rate = mean(usable_week, na.rm =T),
      hi_usable = mean(usable_week, na.rm =T) + sd(usable_week, na.rm =T)/2,
      lo_baseline = mean(baseline_week, na.rm =T) - sd(baseline_week, na.rm =T)/2,
      baseline_rate = mean(baseline_week, na.rm =T),
      hi_baseline = mean(baseline_week, na.rm =T) + sd(baseline_week, na.rm =T)/2,
      lo_optimal = mean(pos_optimal)- sd(pos_optimal)/2,
      optimal_rate = mean(pos_optimal),
      hi_optimal = mean(pos_optimal) + sd(pos_optimal)/2,
      pct_of_pos_max = mean(pct_of_pos_max, na.rm = T),
      x_injured = round(injury_prob * x_games, 2),
      lo_x_elite = round(lo_elite* x_games,2),
      x_elite = round(elite_rate* x_games,2),
      hi_x_elite = round(hi_elite* x_games,2),
      lo_x_usable = round(lo_usable * x_games, 2),
      x_usable = round(usable_rate * x_games, 2),
      hi_x_usable = round(hi_usable * x_games, 2),
      lo_x_baseline = round(lo_baseline * x_games, 2),
      x_baseline = round(baseline_rate * x_games, 2),
      hi_x_baseline = round(hi_baseline * x_games, 2),
      wk15_proj = mean(projected_score[week == 15]),
      wk16_proj = mean(projected_score[week == 16]),
      wk17_proj = mean(projected_score[week == 17]),
      playoff_proj = mean(projected_score[week >= 15 & week <= 17]),
      fpts_10 = mean(projected_score >= 10) * x_games,
      fpts_20 = mean(projected_score >= 20) * x_games,
      fpts_25 = mean(projected_score >= 25) * x_games,
      fpts_30 = mean(projected_score >= 30) * x_games,
      fpts_40 = mean(projected_score >= 40) * x_games,
    ) %>%
    select(-x_games) %>%
    ungroup()
}


# underdog projection metrics ----
generate_seasonal_projection_metrics = function(proj, flex_baseline = 218,
                                       te_flexmax_pct = .7, te_usable_rank = 6,
                                       qb_baseline = 30, qb_flexmax_pct = .75, qb_usable_rank = 12,
                                       skill_posmax_pct = .9, skill_usable_rank = 24,
                                       te_baseline_player = 12,
                                       qb_baseline_player = 24,
                                       flex_baseline_player = 60,
                                       ud_ranks = ud_ranks_df){
  proj %>%
    group_by(season, week, pos) %>%
    mutate(weekly_pos_rank = min_rank(-projected_score),
           max_projected_score = max(projected_score)) %>%
    group_by(season, week, flex) %>% # Get Netrics to compare scores to FLEX baselines
    mutate(
      weekly_flex_rank = min_rank(-projected_score),
      flex_baseline = nth(projected_score, order_by = desc(projected_score), flex_baseline),
      qb_baseline = nth(projected_score, order_by = desc(projected_score), qb_baseline),
      max_flex_projection = max(projected_score),
    ) %>%
    ungroup() %>% 
    mutate(
      vbd_baseline = if_else(flex, flex_baseline, qb_baseline), # value above 0: score above replacement value
      weekly_value = projected_score - vbd_baseline, #surplus value above replacement
      baseline_week = weekly_value >= 0, # T/F was score above replacement?
      pos_optimal = projected_score == max_projected_score, #was player the optimal at their pos group
      pct_of_pos_max = projected_score/max_projected_score, #what pct of the positional max score did player produce
      pct_of_flex_max = projected_score/max_flex_projection, #what pct of the flex max score did player produce (if top score: 1)
      elite_week = if_else( #was the week an "Elite" Week (
        #A: Top Overall player at position and Player Produces 70% of max FLEX (or QB) score
        #B: Score 75% of the FLEX max (Excluding QB)
        #C: Top 2 At Position (Excluding TE)
        #D: Score 90% of Positional Max (Excluding TE))
        (weekly_pos_rank == 1 & pct_of_flex_max > te_flexmax_pct)|
          (pos != "QB" & pct_of_flex_max>= qb_flexmax_pct )|
          (pos != "TE" & (weekly_pos_rank <= 3 | pct_of_pos_max >= skill_posmax_pct)), 
        1, 0),
      usable_week = if_else( #was the week an "Usable" Week (
        #A: Top-12 QB
        #B: Top-6 TE
        #C: Top-24 WR or RB
        (pos %in% c("QB") & weekly_pos_rank <= qb_usable_rank) |
          (pos %in% c("TE") & weekly_pos_rank <= te_usable_rank) |
          (pos %in% c("WR","RB") & weekly_pos_rank <= skill_usable_rank), 1, 0)
    ) %>% 
    select(-flex_baseline, -qb_baseline) %>%
    group_by(player, fantasypros_id, flex, pos, team, season) %>%
    summarize(
      injury_prob = 1-mean(gp_model),
      x_games = 16 * (1-injury_prob),
      x_games=  x_games_adj(player, x_games),
      #x_games = x_games_adj(player, x_games),
      value = mean(weekly_value),
      lo_value = mean(weekly_value) - sd(weekly_value)/2,
      hi_value = mean(weekly_value) + sd(weekly_value)/2,
      lo_proj = mean(projected_score) - sd(projected_score)/2,
      projection = mean(projected_score),
      sd_projection = sd(projected_score),
      hi_proj = mean(projected_score) + sd(projected_score)/2,
      lo_elite = mean(elite_week) - sd(elite_week)/2,
      elite_rate = mean(elite_week, na.rm =T),
      hi_elite = mean(elite_week) + sd(elite_week)*2,
      lo_usable = mean(usable_week, na.rm =T)- sd(usable_week, na.rm =T)/2,
      usable_rate = mean(usable_week, na.rm =T),
      hi_usable = mean(usable_week, na.rm =T) + sd(usable_week, na.rm =T)/2,
      lo_baseline = mean(baseline_week, na.rm =T) - sd(baseline_week, na.rm =T)/2,
      baseline_rate = mean(baseline_week, na.rm =T),
      hi_baseline = mean(baseline_week, na.rm =T) + sd(baseline_week, na.rm =T)/2,
      lo_optimal = mean(pos_optimal)- sd(pos_optimal)/2,
      optimal_rate = mean(pos_optimal),
      hi_optimal = mean(pos_optimal) + sd(pos_optimal)/2,
      pct_of_pos_max = mean(pct_of_pos_max, na.rm = T),
      x_injured = round(injury_prob * x_games, 2),
      lo_x_elite = round(lo_elite* x_games,2),
      x_elite = round(elite_rate* x_games,2),
      hi_x_elite = round(hi_elite* x_games,2),
      lo_x_usable = round(lo_usable * x_games, 2),
      x_usable = round(usable_rate * x_games, 2),
      hi_x_usable = round(hi_usable * x_games, 2),
      lo_x_baseline = round(lo_baseline * x_games, 2),
      x_baseline = round(baseline_rate * x_games, 2),
      hi_x_baseline = round(hi_baseline * x_games, 2),
      wk15_proj = mean(projected_score[week == 15]),
      wk16_proj = mean(projected_score[week == 16]),
      wk17_proj = mean(projected_score[week == 17]),
      playoff_proj = mean(projected_score[week >= 15 & week <= 17]),
      fpts_10 = mean(projected_score >= 10) * x_games,
      fpts_20 = mean(projected_score >= 20) * x_games,
      fpts_25 = mean(projected_score >= 25) * x_games,
      fpts_30 = mean(projected_score >= 30) * x_games,
      fpts_40 = mean(projected_score >= 40) * x_games,
    ) %>%
    select(-x_games) %>%
    ungroup()
}

# Join Simulation Metrics With ADP -----

join_rankings_with_adp = function(proj_metrics, latest_rankings, adjusted_adp){
  proj_metrics %>%
                     left_join(
                       latest_rankings
                     ) %>%
                     left_join(
                       adjusted_adp %>% select(pos, rank, prank_x_score = inj_adj_mean_score)
                     ) %>%
                     distinct() %>%
                     arrange(-x_elite)
}


# Simulation-based Player Values ----

generate_player_values = function(proj_joined,
                                  te_baseline_player = 12,
                                  qb_baseline_player = 12,
                                  flex_baseline_player = 60,
                                  te_flexmax_pct = .7,
                                  posrank_cutoff = 5,
                                  team_bumps = c(), #c("LAC", "KC", "LV", "DEN", "BAL", "CIN", "CLE", "TB", "BUF"),
                                  player_bumps = c() #c("DeVonta Smith", "ELijah Moore","Amon-Ra St. Brown","Marquise Brown","Rashod Bateman")
){
  suppressMessages(
    proj_joined %>%
      mutate(vbd_baseline_rank = if_else(flex, flex_baseline_player, qb_baseline_player),
             x_usable = x_games_adj(player, x_usable),) %>%
      group_by(flex) %>%
      mutate(
        qb_vbd_baseline_usable = nth(x_usable, qb_baseline_player),
        qb_vbd_baseline_elite = nth(hi_x_elite, qb_baseline_player),
        flex_vbd_baseline_usable = nth(x_usable, flex_baseline_player),
        flex_vbd_baseline_elite = nth(hi_x_elite, flex_baseline_player)
      ) %>%
      ungroup() %>%
      group_by(pos) %>%
      mutate(
        te_vbd_baseline_usable = nth(x_usable, te_baseline_player),
        te_vbd_baseline_elite = nth(hi_x_elite, te_baseline_player)
      ) %>%
      ungroup() %>%
      mutate(
        vbd_baseline_usable = if_else(pos == "QB", qb_vbd_baseline_usable, flex_vbd_baseline_usable),
        vbd_baseline_elite = if_else(pos == "QB", qb_vbd_baseline_elite, flex_vbd_baseline_elite),
        te_extra_baseline_usable = if_else(pos == "TE", te_vbd_baseline_usable, 0),
        te_extra_baseline_elite = if_else(pos == "TE", te_vbd_baseline_elite, 0),
        te_usable_value = if_else(x_usable >= te_vbd_baseline_usable, ((1 -
                                                                          te_flexmax_pct) * (x_usable - te_vbd_baseline_usable)
        ), 0),
        te_elite_value = if_else(
          hi_x_elite >= te_vbd_baseline_elite,
          (1 - te_flexmax_pct) * (hi_x_elite - te_vbd_baseline_elite),
          0
        ),
        usable_value = if_else(
          pos != "TE",
          x_usable - vbd_baseline_usable,
          (x_usable - vbd_baseline_usable)  + te_usable_value
        ),
        elite_value = if_else(
          pos != "TE",
          hi_x_elite - vbd_baseline_elite,
          (hi_x_elite - vbd_baseline_elite) + te_elite_value
        ),
        usable_grp = round(usable_value)
      ) %>%
      group_by(flex, usable_grp) %>%
      mutate(group_elite_baseline = mean(hi_x_elite, na.rm = T)) %>%
      ungroup() %>%
      mutate(
        upside_value = hi_x_elite - group_elite_baseline,
        usable_value = case_when(
          ecr <= posrank_cutoff ~ usable_value,
          team %in% team_bumps| player %in% player_bumps ~ usable_value + .5, #ARBITRARY ADJUSTMENTS GASP
          T ~ usable_value
        )
      ) %>%
      arrange(-usable_value)
  )  
}



# Create a Set of Sim-Based Projections -----
generate_custom_sims = function(conn, adp_outcomes, latest_rankings, 
                                custom_ranks,
                                custom_influence = .3, 
                                weeks = 1:17, 
                                n_seasons = 1000, 
                                no_byes = F,
                                ud_ranks = ud_ranks_df,
                                ...
){
  message("generating scoring history....")
  
  options(dplyr.summarise.inform = FALSE)
  # Scoring History 
  scoring_history = ff_scoringhistory(conn)
  
  
  # Latest Rankings 
  
  ## FantasyPros Raw Rankings
  message("Done!")
  message("accessing latest rankings...")
  
  latest_rankings_raw = ffs_latest_rankings()
  
  # Historical ADP Outcomes 
  message("Done!")
  message("Accessing historical ADP outcomes...")
  
  adp_outcomes = ffs_adp_outcomes(
    scoring_history = scoring_history,
    gp_model = "simple" # or "none"
  )
  
  ud_ranks_ = ud_ranks
  
  latest_rankings = draft_night_ranks#generate_custom_rankings(latest_rankings_raw, custom_ranks, ud_ranks = ud_ranks_, custom_influence, ...)
  
  
  #Projections
  message("Done!")
  message(paste0("Generating ", length(weeks), " weeks of projections for ", n_seasons, " Season", if_else(n_seasons > 1, "s",""),"..."))
  
  proj = generate_custom_projections(adp_outcomes, latest_rankings, weeks = 1:17, n_seasons, ...)
  
  return(proj)
}


# Create a Set of Sim-Based Projections -----
generate_custom_sims = function(conn, adp_outcomes, latest_rankings, 
                                custom_ranks,
                                custom_influence = .3, 
                                weeks = 1:17, 
                                n_seasons = 1000, 
                                no_byes = F,
                                ud_ranks = ud_ranks_df,
                                ...
){
  message("generating scoring history....")
  
  options(dplyr.summarise.inform = FALSE)
  # Scoring History 
  scoring_history = ff_scoringhistory(conn)
  
  
  # Latest Rankings 
  
  ## FantasyPros Raw Rankings
  message("Done!")
  message("accessing latest rankings...")
  
  latest_rankings_raw = ffs_latest_rankings()
  
  # Historical ADP Outcomes 
  message("Done!")
  message("Accessing historical ADP outcomes...")
  
  adp_outcomes = ffs_adp_outcomes(
    scoring_history = scoring_history,
    gp_model = "simple" # or "none"
  )
  
  ud_ranks_ = ud_ranks
  
  latest_rankings = draft_night_ranks#generate_custom_rankings(latest_rankings_raw, custom_ranks, ud_ranks = ud_ranks_, custom_influence, ...)
  
  
  #Projections
  message("Done!")
  message(paste0("Generating ", length(weeks), " weeks of projections for ", n_seasons, " Season", if_else(n_seasons > 1, "s",""),"..."))
  
  proj = generate_custom_projections(adp_outcomes, latest_rankings, weeks = 1:17, n_seasons, ...)
  
  return(proj)
}


generate_seasonal_sims = function(conn, adp_outcomes, latest_rankings, 
                                weeks = 1:17, 
                                n_seasons = 1000, 
                                no_byes = F,
                                ...
){
  message("generating scoring history....")
  
  options(dplyr.summarise.inform = FALSE)
  # Scoring History 
  scoring_history = ff_scoringhistory(conn)
  
  
  # Latest Rankings 
  
  ## FantasyPros Raw Rankings
  message("Done!")
  message("accessing latest rankings...")
  
  latest_rankings_raw = ffs_latest_rankings()
  
  # Historical ADP Outcomes 
  message("Done!")
  message("Accessing historical ADP outcomes...")
  
  adp_outcomes = ffs_adp_outcomes(
    scoring_history = scoring_history,
    gp_model = "simple" # or "none"
  )
  
  latest_rankings = draft_night_ranks
  
  
  #Projections
  message("Done!")
  message(paste0("Generating ", length(weeks), " weeks of projections for ", n_seasons, " Season", if_else(n_seasons > 1, "s",""),"..."))
  
  proj = generate_custom_projections(adp_outcomes, latest_rankings, weeks = 1:17, n_seasons, ...)
  
  return(proj)
}



generate_custom_projections_df = function(conn, adp_outcomes, latest_rankings, 
                                          custom_ranks,
                                          custom_influence = .3, 
                                          weeks = 1:17, 
                                          n_seasons = 1000, 
                                          no_byes = F,
                                          flex_baseline = 218,
                                          te_flexmax_pct = .7, te_usable_rank = 6,
                                          qb_baseline = 30, qb_flexmax_pct = .75, qb_usable_rank = 12,
                                          skill_posmax_pct = .9, skill_usable_rank = 24,
                                          te_baseline_player = 12,
                                          qb_baseline_player = 24,
                                          flex_baseline_player = 60,
                                          ud_ranks = ud_ranks_df,
                                          posrank_cutoff = 5,
                                          team_bumps = c(), #c("LAC", "KC", "LV", "DEN", "BAL", "CIN", "CLE", "TB", "BUF"),
                                          player_bumps = c(), #c("DeVonta Smith", "ELijah Moore","Amon-Ra St. Brown","Marquise Brown","Rashod Bateman")
                                          ...
){
  
  options(dplyr.summarise.inform = FALSE)
  
  proj = generate_custom_sims(conn, adp_outcomes, latest_rankings, ud_ranks, custom_ranks)
  
  
  # Generate Metrics 
  message("Done!")
  message("Generating player range of outcomes...")
  
  
  proj_metrics = generate_projection_metrics(proj)
  
  message("Adjusting ADP Based Outcomes...")
  
  adjusted_adp = adjust_adp_outcomes(adp_outcomes)
  
  message("Joining Ranks w ADP...")
  
  proj_joined = join_rankings_with_adp(proj_metrics, latest_rankings, adjusted_adp)
  
  
  message("Some final calculations...") 
  
  te_baseline_player_ = te_baseline_player
  qb_baseline_player_ = qb_baseline_player
  flex_baseline_player_ = flex_baseline_player
  team_bumps_ = team_bumps
  player_bumps_ = player_bumps
  te_flexmax_pct_ = te_flexmax_pct
  posrank_cutoff_ = posrank_cutoff
  
  projections = generate_player_values(
    proj_joined, te_baseline_player = te_baseline_player_, 
    qb_baseline_player = qb_baseline_player_, 
    flex_baseline_player = flex_baseline_player_, 
    team_bumps = team_bumps_, 
    player_bumps = player_bumps_, 
    te_flexmax_pct = te_flexmax_pct_,
    posrank_cutoff = posrank_cutoff_)
  
  message("Cleaning up...")
  
  return(projections)
}
