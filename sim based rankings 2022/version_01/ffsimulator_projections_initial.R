library(easypackages)
options(nflreadr.verbose = FALSE)
plan("multisession")
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
  prompt = F
)
dropbox = ifelse(Sys.info()['sysname'] == "Windows", "C:/Users/matts/dropbox/", "~/Dropbox/")
source(paste0(dropbox, "Matt Savoca/Projects/theme_FE.R"))
projects_folder = paste0(dropbox, "Matt Savoca/Projects/")
downloads_folder = paste0(dropbox, "Matt Savoca/Droploads/")
theme_set(theme_FE)
# cl <- makePSOCKcluster(parallel::detectCores() -2)
# registerDoParallel(cl)
# stopImplicitCluster()


# scoring settings
UD_12TM = 802734610873188352

conn =  sleeper_connect(season = 2022, league_id = UD_12TM)
  
scoring_history = ff_scoringhistory(conn)

latest_rankings = ffs_latest_rankings() %>%
  group_by(pos) %>%
  mutate(rank = min_rank(ecr)) %>%
  ungroup()

league_info = ffscrapr::ff_league(conn) %>% mutate(best_ball = TRUE)

adp_outcomes_raw = ffs_adp_outcomes(
  scoring_history = scoring_history,
  gp_model = "simple" # or "none"
)

proj_2022 = ffs_generate_projections(adp_outcomes_raw, latest_rankings, weeks = 1:17, n_seasons = 5000) %>% 
  as.data.frame() %>%
  mutate(bye = 18,
         flex = if_else(pos %in% c("RB", "WR", "TE"), T, F),
         projected_score = projection * gp_model * 
           (week != bye)) %>%
  group_by(season, week, pos) %>%
  mutate(weekly_pos_rank = min_rank(-projected_score),
         max_projected_score = max(projected_score)) %>%
  group_by(season, week, flex) %>%
  mutate(
    weekly_flex_rank = min_rank(-projected_score),
    flex_baseline = nth(projected_score, order_by = desc(projected_score), 72),
    qb_baseline = nth(projected_score, order_by = desc(projected_score), 12),
    max_flex_projection = max(projected_score),
  ) %>%
  ungroup() %>% 
  mutate(
    vbd_baseline = if_else(flex, flex_baseline, qb_baseline),
    weekly_value = projected_score - vbd_baseline,
    baseline_week = weekly_value >= 0,
    pos_optimal = projected_score == max_projected_score,
    pct_of_pos_max = projected_score/max_projected_score,
    pct_of_flex_max = projected_score/max_flex_projection,
    elite_week = if_else(
      weekly_pos_rank == 1 & pct_of_flex_max > .7|
      (pos != "QB" & pct_of_flex_max>= .75 )|
        (pos != "TE" & (weekly_pos_rank <= 3 | pct_of_pos_max >= .9)), 
      1, 0),
    usable_week = if_else(
      (pos %in% c("QB") & weekly_pos_rank <= 12) |
        (pos %in% c("TE") & weekly_pos_rank <= 6) |
        (pos %in% c("WR","RB") & weekly_pos_rank <= 24), 1, 0)
  ) %>% 
  select(-flex_baseline, -qb_baseline) %>%
  group_by(player, fantasypros_id, flex, pos, team) %>%
  summarize(
    injury_prob = 1-mean(gp_model),
    value = mean(weekly_value),
    lo_value = mean(weekly_value) - sd(weekly_value)/2,
    hi_value = mean(weekly_value) + sd(weekly_value)/2,
    lo_proj = mean(projected_score) - sd(projected_score)/2,
    projection = mean(projected_score),
    hi_proj = mean(projected_score) + sd(projected_score)/2,
    lo_elite = mean(elite_week) - sd(elite_week)/2,
    elite_rate = mean(elite_week, na.rm =T),
    hi_elite = mean(elite_week) + sd(elite_week)*2,
    lo_usable = mean(usable_week, na.rm =T)- sd(usable_week, na.rm =T)/2,
    usable_rate = mean(usable_week, na.rm =T),
    hi_usable = mean(usable_week, na.rm =T)- sd(usable_week, na.rm =T)/2,
    lo_baseline = mean(baseline_week, na.rm =T) - sd(baseline_week, na.rm =T)/2,
    baseline_rate = mean(baseline_week, na.rm =T),
    hi_baseline = mean(baseline_week, na.rm =T) + sd(baseline_week, na.rm =T)/2,
    lo_optimal = mean(pos_optimal)- sd(pos_optimal)/2,
    optimal_rate = mean(pos_optimal),
    hi_optimal = mean(pos_optimal) + sd(pos_optimal)/2,
    pct_of_pos_max = mean(pct_of_pos_max, na.rm = T),
    x_injured = round(injury_prob * 16, 1),
    lo_x_elite = round(lo_elite* 16,1),
    x_elite = round(elite_rate* 16,1),
    hi_x_elite = round(hi_elite* 16,1),
    lo_x_usable = round(lo_usable * 16, 1),
    x_usable = round(usable_rate * 16, 1),
    hi_x_usable = round(hi_usable * 16, 1),
    lo_x_baseline = round(lo_baseline * 16, 1),
    x_baseline = round(baseline_rate * 16, 1),
    hi_x_baseline = round(hi_baseline * 16, 1),
  ) %>%
  ungroup() %>%
  arrange(-x_elite) %>%
  left_join(
    latest_rankings
  ) %>%
  left_join(
    adp_outcomes2 %>% select(pos, rank, prank_x_score = inj_adj_mean_score)
  ) %>%
  distinct() %>%
  mutate(leverage = projection - prank_x_score) %>%
  arrange(-x_elite)

proj_22_values = proj_2022 %>%
    mutate(
      vbd_baseline_rank = if_else(flex, 96, 15)
    ) %>% 
    group_by(flex) %>%
    mutate(
      qb_vbd_baseline_usable = nth(x_usable, 15),
      qb_vbd_baseline_elite = nth(hi_x_elite, 15),
      flex_vbd_baseline_usable = nth(x_usable, 96),
      flex_vbd_baseline_elite = nth(hi_x_elite, 96)
    ) %>%
    ungroup() %>%
    mutate(
      vbd_baseline_usable = if_else(pos == "QB", qb_vbd_baseline_usable, flex_vbd_baseline_usable),
      vbd_baseline_elite = if_else(pos == "QB", qb_vbd_baseline_elite, flex_vbd_baseline_elite),
      usable_value = x_usable - vbd_baseline_usable,
      elite_value = hi_x_elite - vbd_baseline_elite,
      usable_grp = if_else(round(usable_value) > 5, 5, round(usable_value))
    ) %>%
    group_by(flex, usable_grp) %>%
    mutate(group_elite_baseline = mean(hi_x_elite, na.rm = T)) %>%
    ungroup() %>%
    mutate(upside_value = hi_x_elite - group_elite_baseline) %>%
    arrange(-usable_value)




proj_22_values %>%
  mutate(
    usable_value = if_else(
      team %in% c("LAC", "KC", "LV", "DEN", "BAL", "CIN", "CLE","TB"), 
      usable_value+.5, usable_value),
    hi_x_elite = if_else(
      team %in% c("LAC", "KC", "LV", "DEN", "BAL", "CIN", "CLE","TB"), 
      hi_x_elite+.5, hi_x_elite),
    usable_value = round(usable_value),
    hi_x_elite = round(hi_x_elite),
    primary_tier = dense_rank(-usable_value) %>% as.numeric()
  ) %>%
  arrange(-usable_value, -hi_x_elite) %>%
  
  select(player, pos, flex, team, expected_usable = x_usable, xWAR = usable_value, ceiling = hi_x_elite, tier) %>%
  glimpse() #write_csv("nfl_tiers_2022.csv")


## proj 2022 charting ------

proj_values_plot = ggplotly(
  proj_22_values %>%
  mutate(
         usable_value = if_else(
           team %in% c("LAC", "KC", "LV", "DEN", "BAL", "CIN", "CLE","TB"), 
           usable_value+.5, usable_value),
         usable_val = round(usable_value,1),
         usable_value = round(usable_value),
         elite_value = round(elite_value, 1)
         ) %>%
  arrange(usable_value, elite_value) %>%
  mutate(
        player = factor(player, levels = player)) %>%
  filter(usable_value > 0) %>%
  ggplot()+
  aes(xmin = usable_value, x = usable_value, xmax = elite_value, player)+
  geom_errorbarh(height = 0, aes(color = pos))+
  geom_point(aes(fill = pos, text = paste0(player, 
                                          "\nExp. Usable Weeks: ", x_usable, " Gms (",if_else(usable_val>0,"+",""), usable_val," vs. Pos. Baseline),",
                                          "\nTop-2 Percentile Outcome: ", round(hi_x_elite), " Elite Gms (", if_else(elite_value>0,"+",""), elite_value, " vs. Pos. Baseline)")))+
  scale_y_reordered()+
  scale_x_continuous(breaks = seq(0, 10, 2))+
  labs(
    title = "Projected WAR: Expected Usable & Elite Games vs. Replacement",
    subtitle = "Dot: Usable Week Value, Bar: Upside Value",
    x = "Expected Value",
    y = ""
  )+
  theme(legend.position = "none")
  #facet_wrap(usable_value~., scales = "free")
    ,
tooltip = c("text")
)

saveWidget(proj_values_plot, "nfl_proj_22.html",selfcontained = T)


ggplotly(
proj_22_values %>% 
  ggplot()+
  aes(elite_value, usable_value)+
  geom_point(aes(text = player, color = pos))+
  geom_smooth(se = F)+
  scale_y_continuous(breaks = seq(-5, 8, 1))+
  scale_x_continuous(breaks = seq(-10, 10, 2))+
  labs(
    title = "2022 Fantasy Value Scores",
    y = "Expected Value",
    x = "Ceiling Value"
  ),
tooltip = "text"
)
proj_2022 %>%
  top_n(250, value) %>%
  arrange(-value) %>%
  mutate(player = reorder(player, value)) %>%
  ggplot()+
  aes(xmin = lo_value, x = value, xmax = hi_value, y = player)+
  geom_errorbarh()+
  facet_wrap(flex ~ ., scales = "free")





#rosters <- ffs_rosters(conn) (won't work pre-draft)

# charting outcomes -------

adp_outcomes = adp_outcomes_raw %>%
  rowwise() %>%
  mutate(mean_score = week_outcomes %>% unlist() %>% mean(),
         inj_adj_mean_score = mean_score * prob_gp) %>%
  ungroup() %>%
  filter(
    (pos %in% c("WR", "RB") & rank < 70) |
      (pos %in% c("QB", "TE") & rank < 32)
  )

adp_outcomes2 =
  adp_outcomes_raw %>%
  rowwise() %>%
  mutate(mean_score = week_outcomes %>% unlist() %>% mean(),
         inj_adj_mean_score = mean_score * prob_gp) %>%
  ungroup() %>%
  unnest(c(player_name, fantasypros_id)) %>%
  unnest(week_outcomes) %>%
  #select(-fantasypros_id, -player_name) %>%
  distinct()

adp_outcomes2 %>%
  filter(pos %in% c("RB", "WR") & rank <= 40 & rank >= 15) %>%
  arrange(rank) %>%
  mutate(fct_rank = factor(rank, levels = rev(rank %>% unique())), 
         fct_ramk = fct_reorder(fct_rank, rank, .desc = T)) %>%
  ggplot()+
  aes(week_outcomes, rank, fill = pos)+
  geom_density_ridges2(aes(week_outcomes, y = fct_rank, vline_color = pos),
                       # vline_color = "black",
                       # jittered_points = T, point_alpha = .05, point_shape = 21,
                       quantile_lines = TRUE, quantiles = c(.5), 
                       rel_min_height=0.01, 
                       # position = position_raincloud(adjust_vlines = TRUE),
                       alpha = .3, scale = .9)+
  theme_light()+
  scale_x_continuous(breaks = seq(0, 60, 5), expand = c(0.01, 0))+
  scale_y_discrete(expand = c(0.01, 0))+
  # geom_jitter(color = "black") +
  # coord_cartesian(xlim = c(1, 50))+
  labs(fill = "",
       y = "Positional ADP",
       x = "Fantasy Points",
       title = "Fantasy Points by Positional ADP",
       subtitle = "Regular Season, 2012 - 2020",
       vline_color = "")+
  guides(fill = F)
plan("sequential")


adp_outcomes2 %>%
  filter(pos %in% c("RB", "WR", "TE") & rank <= 12 & rank >= 1) %>%
  arrange(rank) %>%
  mutate(fct_rank = factor(rank, levels = rev(rank %>% unique())),
         fct_rank = reorder_within(fct_rank, desc(week_outcomes), within = pos, fun = mean)) %>%
  ggplot()+
  aes(rank, week_outcomes, group = fct_rank, fill = pos) +
  geom_violin(draw_quantiles = c(.5, .9), violinwidth = .75)+
  theme_light()+
  scale_x_continuous(breaks = seq(1,400,1))



# graphing RB dead zone -------

adp_outcomes %>%
  mutate(inj_adj_mean_score = round(inj_adj_mean_score)) %>%
  ggplot()+
  aes(rank, inj_adj_mean_score, color = pos)+
  annotate("rect", xmin = 15, xmax = 38, ymin = 5.75, ymax = 11, color = "dark grey", alpha = .3, fill = "white", size = 1.5)+
  annotate("text", x = 25, y = 8.75, label = "RB Dead-Zone")+
  geom_line() +
  coord_cartesian(xlim = c(1, 50))+
  labs(color = "",
       x = "Positional ADP",
       y = "Injury Adjusted Weekly FPts/Gm",
       title = "Average Fantasy Pts. Per Game by Positional ADP",
       subtitle = "Half-PPR Scoring, 2012 - 2020")+
  theme_light()


# exp_score model -----


adp_splits = initial_split(adp_outcomes2)
adp_training = training(adp_splits)
adp_testing = testing(adp_splits)

adp_resamples = vfold_cv(adp_training,  5)

xgboost_recipe <- 
  recipe(formula =  week_outcomes ~ pos + rank + inj_adj_mean_score, data = adp_training) %>% 
  step_dummy(all_nominal(), -all_outcomes(), one_hot = TRUE)

xgboost_recipe %>% prep() %>% juice()

xgboost_spec <- 
  boost_tree(trees = tune(), min_n = tune(), tree_depth = tune(), learn_rate = tune(), 
             loss_reduction = tune(), sample_size = tune()) %>% 
  set_mode("regression") %>% 
  set_engine("xgboost", monotone_constraints = c(0, -1, 1)) 

xgboost_workflow <- 
  workflow() %>% 
  add_recipe(xgboost_recipe) %>% 
  add_model(xgboost_spec) 

xgboost_grid = 
  xgboost_spec %>%
  parameters() %>%
  grid_max_entropy(size = 30)


tic()
xgboost_tune <-
  tune_race_anova(xgboost_workflow, resamples = adp_resamples, 
                  metrics = metric_set(rmse, mae, rsq),
                  grid = xgboost_grid)
toc()

xgboost_tune %>%
  collect_metrics()


xgb_best_rmse = xgboost_tune %>% select_best("rmse")

# trees = 1791
# min_n = 8
# tree_depth = 4
# learn_rate = 0.0248
# loss_reduction = 15.4
# sample_size = 0.991


xgb_model = xgboost_workflow %>%
  finalize_workflow(xgb_best_rmse)

xgb_model %>%
  last_fit(adp_splits) %>%
  collect_metrics()


xg_fit = xgb_model %>%
  fit(adp_training)

importances = xgboost::xgb.importance(model  = extract_fit_engine(xg_fit))

importances %>%
  mutate(Feature = reorder(Feature, Gain)) %>%
  ggplot()+
  aes(Gain, Feature) %>%
  geom_col()


xg_fit_full = xgb_model %>% fit(adp_outcomes2)

predictions = xg_fit_full %>%
  augment(adp_outcomes)

xgboost::xgb.save(
  extract_fit_engine(xg_fit_full),
  "ff_predictions_22_model"
)


predictions %>%
  ggplot()+
  aes(rank, .pred, color = pos)+
  geom_line()+
  geom_line(size = 1.4, aes(x = rank,  mean_score, color = pos),  alpha = .1 )


predictions %>%
  filter(pos != "QB" | rank <= 32) %>%
  arrange(-.pred) %>%
  mutate(sflex_baseline = nth(.pred[pos != "QB"], n = 125),
         qb_baseline = nth(.pred[pos == "QB"], n = 15)) %>%
  rowwise()%>%
  mutate(pred_sf_vbd = .pred - sflex_baseline,
         pred_qb1_vbd = if_else(pos != "QB", NA_real_, .pred - qb_baseline),
         pred_vbd = if_else(pos != "QB", pred_sf_vbd, pred_qb1_vbd)) %>%
  ungroup() %>%
  mutate(
    total_vbd = sum(pred_vbd[pred_vbd > 0]),
    excess_budget = 12*200-(12*1*15),
    vbd_value = excess_budget/total_vbd,
    est_av = vbd_value  * pred_vbd
  ) %>%
  arrange(-vbd_value) %>%
  ggplot()+
  aes(rank, pred_vbd, color = pos)+
  #geom_hline(yintercept = 0, lty = 2, color = "black")+
  geom_line()


ggplotly(

  predictions %>%  
    filter(pos != "QB" | rank <= 32) %>%
    arrange(-.pred) %>%
    mutate(sflex_baseline = nth(.pred[pos != "QB"], n = 125),
           qb_baseline = nth(.pred[pos == "QB"], n = 15)) %>%
    rowwise()%>%
    mutate(pred_sf_vbd = .pred - sflex_baseline,
           pred_qb1_vbd = if_else(pos != "QB", NA_real_, .pred - qb_baseline),
           pred_sf_vbd = if_else(pos != "QB", pred_sf_vbd, pred_qb1_vbd)) %>%
    ungroup() %>%
    filter(pos %in% c("QB", "TE")) %>%
    ggplot()+
    aes(rank, pred_sf_vbd, fill = pos)+
    geom_col(position = "dodge")+
    #geom_line()+
    #geom_hline(lty = 2, color = "red", yintercept = 0)+
    #scale_x_continuous(breaks = seq(0, 70,  5))+
    labs(color = "",
         x = "Positional ADP",
         y = "Est. Weekly Value")
)


# generating projections -----


plan("multisession")

projected_scores <- ffs_generate_projections(
  adp_outcomes = adp_outcomes_raw,
  latest_rankings = latest_rankings,
  n_seasons = 100, # number of seasons
  weeks = 1:16 # weeks per season
  )  %>% mutate(projected_score = projection * gp_model)

summarize_projections = function(tbl){
  tbl %>%
    summarize(
      proj = median(projected_score, na.rm  = T),
      .pred = median(.pred, na.rm  = T),
      sd_proj = sd(projected_score, na.rm  = T),
    ) %>%
    mutate(
      proj_low = proj - sd_proj/2,
      proj_hi = proj + sd_proj/2
    )
}

#skill players ----

proj_df = projected_scores %>% 
  left_join(predictions %>% select(pos, rank, .pred)) %>%
  filter(pos %in% c("RB","WR", "TE"), !player %in% c("Jamie Newman, Jacob Eastman")) %>%
  group_by(player, pos) %>% 
  summarize_projections() %>%
  ungroup() %>%
  mutate(player = reorder(player, proj),
         .pred = if_else(is.na(.pred), proj, .pred)) 

proj_df  %>%
  arrange(-.pred, -proj) %>%
  mutate(pred_factor = as.factor(round(.pred)),
         player = reorder_within(player, proj, pred_factor)) %>%
  top_n(36, proj) %>%
  ggplot()+
  aes(proj, player, xmin = proj_low, xmax = proj_hi)+
  geom_errorbarh(aes(color = pos))+
  geom_point()+
  geom_point(aes(round(.pred,0), player),color = "red")+
  scale_y_reordered()+
  labs(x = "", y = "")

## QBs ----
projected_scores %>% 
  filter(pos %in% c("QB"), !player %in% c("Jamie Newman, Jacob Eastman")) %>%
  group_by(player, pos) %>% 
  summarize_projections() %>%
  ungroup() %>%
  mutate(player = reorder(player, proj_hi)) %>%
  top_n(36, proj) %>%
  ggplot()+
  aes(proj, player, xmin = proj_low, xmax = proj_hi)+
  geom_errorbarh(aes(color = pos))+
  geom_point()


projected_scores %>%
  filter(pos %in% c("WR", "RB")) %>%
  group_by(player, sportradar_id) %>%
  summarise(
    bye = mean(bye),
    ecr = mean(ecr),
    sd = mean(sd),
    rank = mean(rank),
    sd_projection = sd(projection, na.rm = T),
    projection = mean(projection),
    injury_model = mean(injury_model),
    projected_score = mean(projected_score),
    low_proj = projection-sd_projection,
    hi_proj = projection+sd_projection
  ) %>%
  arrange(-projected_score) %>%
  ungroup() %>%
  mutate(player = reorder(player, projection)) %>%
  top_n(30, projected_score) %>%
  ggplot()+
  aes(xmin = low_proj, xmax = hi_proj, x = projection, y = player)+
  geom_errorbarh()+
  geom_point()

projected_scores %>%
  # mutate(projected_score = round(projected_score/.5)*.5) %>%
  # group_by(pos, rank) %>%
  # mutate(projected_score = mean(projected_score, na.rm = T)) %>%
  ggplot()+
  aes(rank, projected_score, color = pos)+
  # geom_jitter()+
  geom_smooth()
plan("sequential")



# roster scores -----

roster_scores = ffs_score_rosters(
  projected_scores = projected_scores,
  rosters = rosters
)

optimal_scores = ffs_optimise_lineups(
  roster_scores = roster_scores,
  lineup_constraints = lineup_constraints,
  lineup_efficiency_mean = 0.775,
  lineup_efficiency_sd = 0.05,
  best_ball = FALSE, # or TRUE
  parallel = TRUE # or TRUE 
)


# schedules -----
schedules =ffs_build_schedules(
  n_teams = length(unique(rosters$franchise_id)), 
  n_seasons = n_seasons,
  n_weeks = n_weeks,
  seed = NULL
)


# aggregating results -----

summary_week = ffs_summarise_week(optimal_scores, schedules)
summary_season = ffs_summarise_season(summary_week)
summary_simulation = ffs_summarise_simulation(summary_season)



## ------- custom proj scratchwork


nflfastr_offense_long = function(season){
  ps <- nflfastr_weekly(seasons = season, type = "offense") %>% 
    dplyr::select(dplyr::any_of(c("season", "week", 
                                  "player_id", "attempts", "carries", 
                                  "completions", "interceptions", "passing_2pt_conversions", 
                                  "passing_first_downs", "passing_tds", 
                                  "passing_yards", "receiving_2pt_conversions", 
                                  "receiving_first_downs", "receiving_fumbles", 
                                  "receiving_fumbles_lost", "receiving_tds", 
                                  "receiving_yards", "targets","receptions", "rushing_2pt_conversions", 
                                  "rushing_first_downs", "rushing_fumbles", 
                                  "rushing_fumbles_lost", "rushing_tds", 
                                  "rushing_yards", "sack_fumbles", "sack_fumbles_lost", 
                                  "sack_yards", "sacks", "special_teams_tds", 
                                  "targets"))) %>% tidyr::pivot_longer(names_to = "metric", 
                                                                       cols = -c("season", "week", "player_id"))
  return(ps)
}


nflfastr_offense_long(2021) %>% filter(metric == "targets") %>% arrange(-value)
