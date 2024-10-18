options(tidyverse.quiet = TRUE)
options(nflreadr.verbose = FALSE)
# Helper Functions, Filepaths & Chart Themes -----
dropbox = ifelse(Sys.info()['sysname'] == "Windows", "C:/Users/matts/dropbox/", "~/Dropbox/")
#source(paste0(dropbox, "Matt Savoca/Projects/NFL 2022/ff_projections_helper_functions.R"))
source(paste0(dropbox, "Matt Savoca/Projects/theme_FE.R"))
source("ff_projections_gp_adjustments.R")
source("ff_projections_file_imports.R")
source("ff_projections_name_switches.R")
source("ff_projections_sims.R")
source("ff_projections_multiseason_sim.R")
theme_set(theme_light())


# League Settings ----
#dk_settings_id = 843552521401274368
ud_settings_id = 802734610873188352




# Sims -----

# connection
conn = get_UD_league_settings(ud_settings_id)

# ranks
ud_custom_ranks = generate_custom_premium_ranks(ud_ranks, 
                                             awesemo_ranks, 
                                             etr_bb_ranks, 
                                             awesemo_rb_influence = .1, etr_rb_influence = .9,
                                             awesemo_qb_influence = .1, etr_qb_influence = .9,
                                             awesemo_wr_influence = .1, etr_wr_influence = .9,
                                             awesemo_te_influence = .1, etr_te_influence = .9)

#projections
plan("multisession")

#map function -----

season_sim(conn)

tic()
sims = map_dfr(test_values$season, ~multiseason_lineup_sim(test_values), .id = "season")

sim_summary = sims %>%
  group_by(player_id, player, position) %>%
  summarize(exposure = mean(exposure, na.rm = T),
            appearances = n()/(50*150),
            uci = quantile(exposure, .972),
            lci = quantile(exposure, .025),
            .groups = "drop") %>%
  arrange(-exposure)

toc()

tic()
multiseason_sim = season_sim(conn, n_seasons = 200) %>% multiseason_lineup_sim(n_lineups = 2)
toc()

multiseason_lineup_sim = function(test_values, ...){
  suppressWarnings({
    season_values = 
      test_values %>%
        rename(season_data = data) %>%
        mutate(rosters = map(season_data, function(x){lineup_sim(x, ...)})) %>%
        unnest(rosters) %>%
        unnest(data) %>%
        select(-lineups, -exposure) %>%
      group_by(player_id, player, team, position) %>%
      mutate(uses = n()) %>%
      ungroup() %>%
      arrange(-uses) %>%
      group_by(player_id, player, team, position, uses) %>%
      summarize_if(is.numeric, mean) %>%
      ungroup() %>%
      arrange(-uses)
  })
  
  return(season_values)
}

lineup_sim = function(test_values, n_lineups = 1, ...){
  message("Pumping out a lineup!") 
  coach_values = test_values %>% 
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
      ),
      x_av = round_half_up(x_av) %>% as.integer(),
      team = replace_na(team, "FA")
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
    ungroup()
  
  fffl_model <- model_generic(coach_proj, 
                              #total_salary = rnorm(185, 5, n = 1), 
                              total_salary = 190, 
                              roster_size = 10, 
                              max_from_team = 10)
  
  #randomness <- function(x) rnorm(nrow(coach_values), x, coach_values$sd_proj)
  
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
  model_results = optimize_generic(coach_proj, fffl_model, L = n_lineups, max_exposure = .75, ...)
  
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
    ungroup() %>%
    nest()
}


season_sim = function(conn, ...){
  test_sim = generate_custom_sims(
    conn, custom_ranks = custom_ranks, custom_influence = .75, ud_ranks = ud_ranks_df, ...
  ) 
  
  test_metrics = test_sim %>%
    generate_seasonal_projection_metrics(te_flexmax_pct = .65, #the percentage of the Top FLEX player a Tight End must achieve for an 'Elite' Week
                                qb_baseline = 15, #the weekly rank for QB in order to produce a 'Baseline' week
                                qb_usable_rank = 12, #the weekly rank of the lowest startable QB
                                flex_baseline = 185,  #total WR, TE, RB expected to draft
                                skill_usable_rank = 24, #the total number of starters for RBs or WRs (if different, use the lower number)
                                ud_ranks = ud_ranks)
  
  
  test_adp = generate_custom_adp_outcomes(conn = conn) %>%
    adjust_adp_outcomes()
  
  test_rankings = generate_custom_rankings(latest_rankings_raw, custom_ranks, ud_ranks = ud_ranks_df, custom_influence = .75)
  
  test_adj_ranks = join_rankings_with_adp(proj_metrics = test_metrics, latest_rankings = test_rankings, adjusted_adp = test_adp)
  
  test_values = test_adj_ranks %>% generate_player_values(
    te_baseline_player = 16, # vbd baseline for TE
    qb_baseline_player = 17, #vbd baseline for QB
    flex_baseline_player = 185, #total WR, TE, RB expected to draft (can be moved to crerate a different VBD baseline for FLEX)
    posrank_cutoff = 0, #players with a positional ADP earlier (or equal) to this number will not receive team or player bumps 
    team_bumps = c("LAC", "KC", "DEN",  "CIN", "TB", "BUF", "GBP", "MIN", "LAR"), #bump players from a team up one tier in usability value
    player_bumps = c("D.J. Moore", "Baker Mayfield", "Julio Jones", "Lamar Jackson"), #c("DeVonta Smith", "ELijah Moore","Amon-Ra St. Brown","Marquise Brown","Rashod Bateman") #bump individual players up one tier in usability value (does not stack with team bumps)
  )
  
  test_values = test_values %>%
    group_by(season) %>%
    nest() %>%
    ungroup()
  
  return(test_values)
  
}



# Modular DF ------
tic()
test_sim = generate_custom_sims(
  conn, custom_ranks = custom_ranks, n_seasons = 1, custom_influence = .75, ud_ranks = ud_ranks_df
) 

test_metrics = test_sim %>%
  generate_seasonal_projection_metrics(te_flexmax_pct = .65, #the percentage of the Top FLEX player a Tight End must achieve for an 'Elite' Week
                              qb_baseline = 15, #the weekly rank for QB in order to produce a 'Baseline' week
                              qb_usable_rank = 12, #the weekly rank of the lowest startable QB
                              flex_baseline = 185,  #total WR, TE, RB expected to draft
                              skill_usable_rank = 24, #the total number of starters for RBs or WRs (if different, use the lower number)
                              ud_ranks = ud_ranks)


test_adp = generate_custom_adp_outcomes(conn = conn) %>%
  adjust_adp_outcomes()

test_rankings = generate_custom_rankings(latest_rankings_raw, custom_ranks, ud_ranks = ud_ranks_df, custom_influence = .75)

test_adj_ranks = join_rankings_with_adp(proj_metrics = test_metrics, latest_rankings = test_rankings, adjusted_adp = test_adp)

test_values = test_adj_ranks %>% generate_player_values(
  te_baseline_player = 16, # vbd baseline for TE
  qb_baseline_player = 17, #vbd baseline for QB
  flex_baseline_player = 185, #total WR, TE, RB expected to draft (can be moved to crerate a different VBD baseline for FLEX)
  posrank_cutoff = 0, #players with a positional ADP earlier (or equal) to this number will not receive team or player bumps 
  team_bumps = c("LAC", "KC", "DEN",  "CIN", "TB", "BUF", "GBP", "MIN", "LAR"), #bump players from a team up one tier in usability value
  player_bumps = c("D.J. Moore", "Baker Mayfield", "Julio Jones", "Lamar Jackson"), #c("DeVonta Smith", "ELijah Moore","Amon-Ra St. Brown","Marquise Brown","Rashod Bateman") #bump individual players up one tier in usability value (does not stack with team bumps)
)



coach_values = test_values %>% 
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
    ),
    x_av = round_half_up(x_av) %>% as.integer(),
    team = replace_na(team, "FA")
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
model_results = optimize_generic(coach_proj, fffl_model, L = 150, max_exposure = .75)

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

# Custom DF ------


ud_projections = generate_custom_projections_df(
  conn, 
  custom_ranks = custom_ranks, 
  te_flexmax_pct = .65, #the percentage of the Top FLEX player a Tight End must achieve for an 'Elite' Week
  custom_influence = .75, #the percentage influence of custom rankings, for the remaining percentage, positional ADP is used 
  qb_baseline = 15, #the weekly rank for QB in order to produce a 'Baseline' week
  qb_usable_rank = 12, #the weekly rank of the lowest startable QB
  flex_baseline = 185,  #total WR, TE, RB expected to draft
  skill_usable_rank = 24, #the total number of starters for RBs or WRs (if different, use the lower number)
  n_seasons = 1000, #number of seasons to simulate
  te_baseline_player = 16, # vbd baseline for TE
  qb_baseline_player = 17, #vbd baseline for QB
  flex_baseline_player = 185, #total WR, TE, RB expected to draft (can be moved to crerate a different VBD baseline for FLEX)
  posrank_cutoff = 0, #players with a positional ADP earlier (or equal) to this number will not receive team or player bumps 
  team_bumps = c("LAC", "KC", "DEN",  "CIN", "TB", "BUF", "GBP", "MIN", "LAR"), #bump players from a team up one tier in usability value
  player_bumps = c("D.J. Moore", "Baker Mayfield", "Julio Jones", "Lamar Jackson"), #c("DeVonta Smith", "ELijah Moore","Amon-Ra St. Brown","Marquise Brown","Rashod Bateman") #bump individual players up one tier in usability value (does not stack with team bumps)
  ud_ranks = ud_ranks
)

plan("sequential")

#prepare for export

ud_proj_export = ud_projections %>%
  filter(!is.na(ud_id)) %>%
  create_projections_df() %>%
  left_join(
    ud_ranks %>% select(id = ud_id, adp)
  ) %>%
  mutate(
    usable_value = 
      case_when(
        round(usable_value) < -1.5 ~ -2,
        usable_value > -1 ~ round(usable_value/.25)*.25, 
        T ~ round(usable_value,0)
      )
  )


# Export to CSV -----

ud_proj_export %>%
  write_csv("my_ud_bbm_proj.csv")

# Export to gSheets ------
ud_proj_export %>%
  mutate(
    usable_value = round(usable_value),
    adp = as.numeric(adp),
  ) %>%
  arrange(adp) %>%
  googlesheets4::range_write(
    ss = "https://docs.google.com/spreadsheets/d/1s3yA1KYRoN-B_BPKPWlm70I4kpzfnxsMPsSsqtQdFFE/edit?usp=sharing",
    sheet = "hppr_staging",
    range = "A:U"
  )