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
  prompt = F
)




# filepaths -------
dropbox = ifelse(Sys.info()['sysname'] == "Windows", "C:/Users/matts/dropbox/", "~/Dropbox/")
projects_folder = paste0(dropbox, "Matt Savoca/Projects/")
downloads_folder = paste0(dropbox, "Matt Savoca/Droploads/")

ud_ranks_raw = read_csv(paste0(dropbox, "Matt Savoca/Projects/NFL 2022/ud_adp/rankings-f659a9be-fd34-4a1e-9c43-0816267e603d-ccf300b0-9197-5951-bd96-cba84ad71e86.csv")) %>% clean_names()
ud_sf_ranks_raw = read_csv(paste0(dropbox, "Matt Savoca/Projects/NFL 2022/ud_adp/superflex/rankings-5549297e-8cd2-4dfa-b197-2c472813bfb7-ccf300b0-9197-5951-bd96-cba84ad71e86.csv")) %>% clean_names()


ud_ranks = ud_ranks_raw %>%
  transmute(
    player = paste0(first_name, " ", last_name),
    ud_id = id,
    pos = slot_name,
    team_name,
    adp
  ) %>%
  group_by(pos) %>%
  mutate(pos_adp = row_number()) %>%
  ungroup()

ud_ranks_df = ud_ranks


ud_sf_ranks = ud_sf_ranks_raw %>%
  transmute(
    player = paste0(first_name, " ", last_name),
    ud_id = id,
    pos = slot_name,
    team_name,
    adp
  ) %>%
  group_by(pos) %>%
  mutate(pos_adp = row_number()) %>%
  ungroup()


# FP to Underdog ----

fp_to_underdog_name_switch = function(name){
  nm = gsub("Patrick Mahomes II", "Patrick Mahomes", name)
  nm = gsub("Gardner Minshew II", "Gardner Minshew", nm)
  nm = gsub("Travis Etienne Jr.", "Travis Etienne", nm)
  nm = gsub("Melvin Gordon III", "Melvin Gordon", nm)
  nm = gsub("Ronald Jones II", "Ronald Jones", nm)
  nm = gsub("DK Metcalf", "D.K. Metcalf", nm)
  nm = gsub("Darrell Henderson Jr.", "Darrell Henderson", nm)
  nm = gsub("Brian Robinson Jr.", "Brian Robinson", nm)
  nm = gsub("Ken Walker III", "Ken Walker", nm)
  nm = gsub("Duke Johnson Jr.", "Duke Johnson", nm)
  nm = gsub("Wayne Gallman Jr.", "Wayne Gallman", nm)
  nm = gsub("Pierre Strong Jr.", "Pierre Strong", nm)
  nm = gsub("D.J. Chark Jr.", "D.J. Chark", nm)
  nm = gsub("DJ. Chark", "D.J. Chark", nm)
  nm = gsub("DJ Chark Jr.", "D.J. Chark", nm)
  nm = gsub("William Fuller V", "William Fuller", nm)
  nm = gsub("Odell Beckham Jr.", "Odell Beckham", nm)
  nm = gsub("Laviska Shenault Jr.", "Laviska Shenault", nm)
  nm = gsub("Marvin Jones Jr.", "Marvin Jones", nm)
  nm = gsub("Terrace Marshall Jr.", "Terrace Marshall", nm)
  nm = gsub("Michael Pittman Jr.", "Michael Pittman", nm)
  nm = gsub("Allen Robinson II", "Allen Robinson", nm)
  nm = gsub("Mark Ingram II", "Mark Ingram", nm)
  nm = gsub("Jeff Wilson Jr.", "Jeff Wilson", nm)
  nm = gsub("Cedrick Wilson Jr.", "Cedrick Wilson", nm)
  nm = gsub("Keelan Cole Sr.", "Keelan Cole", nm)
  nm = gsub("Calvin Austin III", "Calvin Austin", nm)
  nm = gsub("Cyril Grayson Jr.", "Cyril Grayson", nm)
  nm = gsub("Mohamed Sanu Sr.", "Mohamed Sanu", nm)
  nm = gsub("Irv Smith Jr.", "Irv Smith", nm)
  nm = gsub("Donald Parham Jr.", "Donald Parham", nm)
  nm = gsub("Chris Herndon IV", "Chris Herndon", nm)
  nm = gsub("Anthony McFarland Jr.", "Anthony McFarland", nm)
  nm = gsub("John Metchie III", "John Metchie", nm)
  nm = gsub("Velus Jones Jr.", "Velus Jones", nm)
  nm = gsub("James Proche II", "James Proche", nm)
  nm = gsub("Scotty Miller", "Scott Miller", nm)
  nm = gsub("Deonte Harty", "Deonte Harris", nm)
  nm = gsub("Eli Mitchell", "Elijah Mitchell", nm)
  nm = gsub("Tyquan Thorton", "Tyquan Thornton", nm)
  nm = gsub("Dee Eskridge", "D'Wayne Eskridge", nm)
  nm = gsub("Damoun Patterson", "Damon Hazelton", nm)
  nm = gsub("Mitch Trubisky", "Mitchell Trubisky", nm)
  nm = gsub("Robbie Anderson", "Robby Anderson", nm)
  nm = gsub("Jakeem Grant Sr.", "Jakeem Grant", nm)
  nm = gsub("Tony Jones Jr.", "Tony Jones", nm)
  nm = gsub("Larry Rountree III", "Larry Rountree", nm)
  nm = gsub("Demetric Felton Jr.", "Demetric Felton", nm)
  nm = gsub("P.J. Walker", "Phillip Walker", nm)
  nm = gsub("Isiah Pacheco", "Isaih Pacheco", nm)
  nm = gsub("Phillip Dorsett II", "Phillip Dorsett", nm)
  nm = gsub("Benny Snell Jr.", "Benny Snell", nm)
  return(nm)
}

# import premium projections  -----
etr_bb_ranks_raw = read_csv(paste0(dropbox, "Matt Savoca/Projects/NFL 2022/ETR Underdog Rankings.csv")) %>% clean_names()
etr_sf_ranks_raw = read_csv(paste0(dropbox, "Matt Savoca/Projects/NFL 2022/ETR Underdog SuperFlex Rankings and ADP.csv")) %>% clean_names()

etr_2qb_ranks_raw = read_csv(paste0(dropbox, "Matt Savoca/Projects/NFL 2022/ETR 2QB PPR Rankings and ADP.csv")) %>% clean_names()

etr_bb_ranks = etr_ranks_raw %>%
  group_by(position) %>%
  mutate(etr_rank = min_rank(etr_rank)) %>%
  ungroup() %>%
  select(player, pos = position, etr_rank, ud_id = id) 




etr_sf_ranks = etr_sf_ranks_raw %>%
  group_by(position) %>%
  mutate(etr_rank = min_rank(etr_rank)) %>%
  ungroup() %>%
  select(player, pos = position, etr_rank, ud_id = id) 


etr_2qb_ranks = etr_2qb_ranks_raw %>%
  left_join(etr_sf_ranks %>% select(player, ud_id)) %>%
  group_by(position) %>%
  mutate(etr_rank = min_rank(etr_rank)) %>%
  ungroup() %>%
  select(player, pos = position, etr_rank, ud_id)

awesemo_ranks_raw = read_csv(paste0(dropbox, "Matt Savoca/Projects/NFL 2022/Underdog Best Ball Rankings.csv")) %>% 
  clean_names() 

awesemo_sf_ranks_raw = read_csv(paste0(dropbox, "Matt Savoca/Projects/NFL 2022/Stokastic Underdog Best Ball Rankings SuperFlex.csv")) %>% 
  clean_names() 





awesemo_superflex_df_adjustments = function(awesemo_sf_ranks_raw){
  awesemo_sf_ranks_raw %>% 
    filter(name != "Jarvis Landry" | team != "FA") %>%
    group_by(position) %>%
    mutate(awesemo_posrank = row_number()) %>%
    ungroup() %>%
    select(player = name, pos = position, ud_id = id, awesemo_rank = awesemo_posrank) %>%
    filter(pos %in% c("QB","RB","WR","TE"), player != 0) %>%
    mutate(
      player = fp_to_underdog_name_switch(player),
      player = case_when(
        player == "Ray-Ray McCloud III" ~ "Ray-Ray McCloud",
        player == "Teeagan Quitoriano" ~ "Teagan Quitoriano",
        T ~ player
      ),
      ud_id = case_when(
        player == "Odell Beckham" ~ "0c21db67-265b-4b54-be3b-f79326ee35bf",
        player == "Greg Dulcich" ~ "9bcf9b0e-ef6a-41ad-ab4c-b503d26b9f5c",
        player == "Giovanni Ricci" ~ "b5b06ba4-1c73-4e31-abf7-3edce9118d84",
        player == "Teagan Quitoriano" ~ "38e64080-d555-4ae8-af92-a400976affa2",
        player == "Tyquan Thornton" ~ "52f9e165-6ace-445e-95e3-cc2c2b5ed5ca",
        player == "Ray-Ray McCloud" ~ "550da15a-f6a7-40e4-adab-c5ab9a05464c",
        T ~ ud_id))
}

awesemo_sf_ranks = awesemo_sf_ranks_raw %>% 
  awesemo_superflex_df_adjustments()

switch_ud_id_to_sf = function(awesemo_ranks){
  awesemo_ranks %>%
    left_join(awesemo_sf_ranks %>% select(player, sf_ud_id = ud_id)) %>%
    select(-ud_id) %>%
    rename(ud_id = sf_ud_id)
}


awesemo_df_adjustments = function(awesemo_ranks){
  awesemo_ranks_raw %>% 
    filter(name != "Jarvis Landry" | team != "FA") %>%
    group_by(position) %>%
    mutate(awesemo_posrank = row_number()) %>%
    ungroup() %>%
    select(player = name, pos = position, ud_id = id, awesemo_rank = awesemo_posrank) %>%
    filter(pos %in% c("QB","RB","WR","TE"), player != 0) %>%
    mutate(
      player = fp_to_underdog_name_switch(player),
      player = case_when(
        player == "Ray-Ray McCloud III" ~ "Ray-Ray McCloud",
        player == "Teeagan Quitoriano" ~ "Teagan Quitoriano",
        T ~ player
      ),
      ud_id = case_when(
        player == "Odell Beckham" ~ "149eac0f-6fd5-4e34-b2ed-3e3149f0d58d",
        player == "Greg Dulcich" ~ "dfa21407-6029-4f82-b27f-b50e0f55e236",
        player == "Giovanni Ricci" ~ "f0c3faa3-6ec0-442d-949a-1c0674abf5c2",
        player == "Teagan Quitoriano" ~ "eb9ed781-537b-4f38-b2bf-6edf503851a4",
        player == "Tyquan Thornton" ~ "0a366caa-62a9-488b-9253-0bbc9770023e",
        player == "Ray-Ray McCloud" ~ "7b5f1faa-1e54-4fd0-92dd-87ca13bdf2a5",
        T ~ ud_id))
}

awesemo_ranks = awesemo_ranks_raw %>% awesemo_df_adjustments()

awesemo_2qb_ranks = awesemo_ranks_raw %>% awesemo_df_adjustments() %>% switch_ud_id_to_sf()



generate_custom_premium_ranks = function(ud_ranks, awesemo_ranks, etr_ranks,
                                         awesemo_qb_influence = .5,
                                         awesemo_rb_influence = .5,
                                         awesemo_wr_influence = .5,
                                         awesemo_te_influence = .5,
                                         etr_qb_influence = .5,
                                         etr_rb_influence = .5,
                                         etr_wr_influence = .5,
                                         etr_te_influence = .5){
  custom_ranks = 
    ud_ranks %>%
    left_join(awesemo_ranks %>% select(ud_id, contains("awesemo"))) %>%
    left_join(etr_bb_ranks %>% select(ud_id, contains("etr"))) %>%
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
      custom_rank = (awesemo_rank * awesemo_inf) + (etr_rank * etr_inf),
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


custom_ranks = generate_custom_premium_ranks(ud_ranks, awesemo_ranks, etr_bb_ranks)





get_UD_league_settings = function(id = 802734610873188352, season = 2022){
  UD_12TM = id
  conn =  sleeper_connect(season = 2022, league_id = UD_12TM)
  return(conn)
}


get_UD_league_info = function(best_ball = T){
  league_info = ffscrapr::ff_league(conn) %>% mutate(best_ball = best_ball)
  return(league_info)
}


x_games_adj = function(player, x){
  val = case_when(
    player == "Chris Godwin" ~ x - (.4*4),
    player == "Robert Woods" ~ x - (.05*4),
    player == "Michael Thomas" ~ x - (.1*4),
    player == "Alvin Kamara" ~ x - (.9*6),
    player == "James Robinson" ~ x - (.7*4),
    player == "Deshaun Watson" ~ x - (.8*17),
    player == "DeAndre Hopkins" ~ x - 6,
    player == "Rob Gronkowski" ~ x - (.5*17),
    T ~ x
  )
  return(val)
}

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

generate_custom_projections = function(adp_outcomes, latest_rankings, weeks = 1:17, n_seasons){
  ffsimulator::ffs_generate_projections(
    adp_outcomes, latest_rankings, weeks = weeks, n_seasons = n_seasons) %>% 
    as.data.frame() %>%
    mutate(bye = if_else(is.na(bye), 18, bye),
           flex = if_else(pos %in% c("RB", "WR", "TE"), T, F),
           projected_score = projection * gp_model *(week != bye))
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
                                          player_bumps = c() #c("DeVonta Smith", "ELijah Moore","Amon-Ra St. Brown","Marquise Brown","Rashod Bateman")
                                          ){
  
  message("generating scoring history....")
  
  options(dplyr.summarise.inform = FALSE)
  # Scoring History 
  scoring_history = ff_scoringhistory(conn)
  
  
  # Latest Rankings 
  
  ## FantasyPros Rankings
  message("Done!")
  message("accessing latest rankings...")
  
  latest_rankings_raw = ffs_latest_rankings()
  
  ud_ranks_ = ud_ranks
  
  generate_custom_rankings = function(latest_rankings_raw, custom_ranks, custom_influence, ud_ranks = ud_ranks_){
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
  
  latest_rankings = generate_custom_rankings(latest_rankings_raw, custom_ranks, custom_influence)
  
  
  # Historical ADP Outcomes 
  message("Done!")
  message("Accessing historical ADP outcomes...")
  
  adp_outcomes = ffs_adp_outcomes(
    scoring_history = scoring_history,
    gp_model = "simple" # or "none"
  )
  
  
  #Projections
  message("Done!")
  message(paste0("Generating ", length(weeks), " weeks of projections for ", n_seasons, " Season", if_else(n_seasons > 1, "s",""),"..."))
  generate_custom_projections = function(adp_outcomes, latest_rankings, weeks = 1:17, n_seasons){
    ffsimulator::ffs_generate_projections(
      adp_outcomes, latest_rankings, weeks = weeks, n_seasons = n_seasons) %>% 
      as.data.frame() %>%
      mutate(bye = if_else(is.na(bye), 18, bye),
             flex = if_else(pos %in% c("RB", "WR", "TE"), T, F),
             projected_score = projection * gp_model *(week != bye))
  }
  
  proj = generate_custom_projections(adp_outcomes, latest_rankings, weeks = 1:17, n_seasons)
  
  
  # Generate Metric 
  message("Done!")
  message("Generating player range of outcomes...")
  
  generate_projection_metrics = function(proj){
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

    proj_metrics = generate_projection_metrics(proj)
    
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
    
    adjusted_adp = adjust_adp_outcomes(adp_outcomes)
    
    
 
  join_rankings_with_adp = function(proj_metrics, latest_rankings, adjusted_adp){
    suppressMessages(proj_metrics %>%
                       left_join(
                         latest_rankings
                       ) %>%
                       left_join(
                         adjusted_adp %>% select(pos, rank, prank_x_score = inj_adj_mean_score)
                       ) %>%
                       distinct() %>%
                       arrange(-x_elite))
  }
    
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

ff_usable_weeks_plot = function(projections){
  ggplotly(
    projections %>%
      mutate(
        usable_val = round(usable_value,1),
        usable_value = round(usable_value),
        elite_value = round(elite_value, 1)
      ) %>%
      arrange(usable_value, elite_value) %>%
      mutate(
        player_level = player,
        player = factor(player, level = player_level)) %>%
      filter(usable_val > 0) %>%
      ggplot()+
      aes(xmin = usable_value, x = usable_value, xmax = elite_value, player)+
      geom_errorbarh(height = 0, aes(color = pos))+
      geom_point(aes(fill = pos, text = paste0(player, 
                                               "\nExp. Usable Weeks: ", x_usable, " Gms (",if_else(usable_val>0,"+",""), usable_val," vs. Pos. Baseline). Elite Wks: ", x_elite,
                                               "\nTop-2 Percentile Outcome: ", round(hi_x_usable), " Usable Weeks, ", round(hi_x_elite), " Elite Gms (", if_else(elite_value>0,"+",""), elite_value, " vs. Pos. Baseline)")))+
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
}  
  
ff_laterounds_plot = function(projections){
    ggplotly(
      projections %>%
        mutate(
          usable_val = round(usable_value,1),
          usable_value = round(usable_value),
          elite_value = round(elite_value, 1)
        ) %>%
        arrange(usable_value, elite_value) %>%
        mutate(
          player_level = player,
          player = factor(player, level = player_level)) %>%
        filter(usable_val <= 0) %>%
        ggplot()+
        aes(xmin = usable_value, x = usable_value, xmax = elite_value, player)+
        geom_errorbarh(height = 0, aes(color = pos))+
        geom_point(aes(fill = pos, text = paste0(player, 
                                                 "\nExp. Usable Weeks: ", x_usable, " Gms (",if_else(usable_val>0,"+",""), usable_val," vs. Pos. Baseline),",
                                                 "\nTop-2 Percentile Outcome: ", round(hi_x_usable), " Usable Weeks, ", round(hi_x_elite), " Elite Gms (", if_else(elite_value>0,"+",""), elite_value, " vs. Pos. Baseline)")))+
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
    
  
}

# PRR Expected Points df -----

pff_to_underdog_name_switch = function(name){
  nm = gsub("Patrick Mahomes II", "Patrick Mahomes", name)
  nm = gsub("D.J. Moore", "DJ Moore", nm)
  nm = gsub("AJ Dillon", "A.J. Dillon", nm)
  nm = gsub("Ronald Jones II", "Ronald Jones", nm)
  nm = gsub("Todd Gurley II", "Todd Gurley", nm)
  nm = gsub("Darrell Henderson", "Darrell Henderson", nm)
  nm = gsub("William Fuller V", "Will Fuller V", nm)
  nm = gsub("Joshua Palmer", "Josh Palmer", nm)
  nm = gsub("KJ Hamler", "K.J. Hamler", nm)
  nm = gsub("Cedrick Wilson Jr.", "Cedrick Wilson", nm)
  nm = gsub("Wayne Gallman Jr.", "Wayne Gallman", nm)
  nm = gsub("William Fuller V", "Will Fuller V", nm)
  nm = gsub("Keelan Cole Sr.", "Keelan Cole", nm)
  nm = gsub("Mohamed Sanu Sr.", "Mohamed Sanu", nm)
  nm = gsub("Chris Herndon IV", "Chris Herndon", nm)
  nm = gsub("Cyril Grayson Jr.", "Cyril Grayson", nm)
  nm = gsub("Donald Parham Jr.", "Donald Parham", nm)
  nm = gsub("Michael Pittman Jr.", "Michael Pittman", nm)
  nm = gsub("Jeff Wilson Jr.", "Jeff Wilson", nm)
  nm = gsub("Melvin Gordon III", "Melvin Gordon", nm)
  nm = gsub("D.J. Chark Jr.", "D.J. Chark", nm)
  nm = gsub("A.J. Dillon", "AJ Dillon", nm)
  nm = gsub("Marvin Jones Jr.", "Marvin Jones", nm)
  nm = gsub("Mark Ingram II", "Mark Ingram", nm)
  nm = gsub("Duke Johnson Jr.", "Duke Johnson", nm)
  nm = gsub("Allen Robinson II", "Allen Robinson", nm)
  nm = gsub("Odell Beckham Jr.", "Odell Beckham", nm)
  nm = gsub("Laviska Shenault Jr.", "Laviska Shenault", nm)
  nm = gsub("Victor Bolden Jr.", "Victor Bolden", nm)
  nm = gsub("Paul Richardson Jr.", "Paul Richardson", nm)
  nm = gsub("K.J. Hamler", "KJ Hamler", nm)
  nm = gsub("Ray-Ray McCloud", "Ray-Ray McCloud III", nm)
  nm = gsub("Irv Smith Jr.", "Irv Smith", nm)
  nm = gsub("Lynn Bowden Jr.", "Lynn Bowden", nm)
  nm = gsub("Richie James Jr.", "Richie James", nm)
  nm = gsub("Steven Mitchell Jr.", "Steven Mitchell", nm)
  nm = gsub("Willie Snead IV", "Willie Snead", nm)
  nm = gsub("Terrace Marshall Jr.", "Terrace Marshall", nm)
  nm = gsub("Joe Webb III", "Joe Webb", nm)
  nm = gsub("Tyquan Thorton", "Tyquan Thornton", nm)
  nm = gsub("Demetric Felton Jr.", "Demetric Felton", nm)
  nm = gsub("Jakeem Grant Sr.", "Jakeem Grant", nm)
  nm = gsub("Tony Jones Jr.", "Tony Jones", nm)
  nm = gsub("Larry Rountree III", "Larry Rountree", nm)
  nm = gsub("P.J. Walker", "Phillip Walker", nm)
  nm = gsub("Trestan Ebner", "Trestan Ebnar", nm)
  nm = gsub("Isaih Pacheco", "Isiah Pacheco", nm)
  return(nm)
}

pull_pff_ep = function(pff_skill_ep, pff_qb_ep, recency = 12){
  
  
  
  adj_ep_df = function(df){
    
    df %>%
      arrange(-year, -week) %>%
      group_by(player_id) %>%
      mutate(game_recency  = as.numeric(row_number())) %>%
      ungroup() %>%
      filter(game_recency <= recency) %>%
      group_by(player, player_id, position) %>%
      summarize_if(is.numeric, mean) %>%
      ungroup() %>%
      select(player, pff_id = player_id, position, expected_points, diff) %>%
      arrange(-expected_points) %>%
      mutate_if(is.numeric, round, 2)
    
  }
  
  skill_ep_df = adj_ep_df(pff_skill_ep)
  qb_ep_df = pff_qb_ep %>% mutate(position = "QB") %>% adj_ep_df()
  ep_df = rbind(skill_ep_df, qb_ep_df) %>% rename(pos = position)
  return(ep_df)
}

pff_skill_ep = read_csv(paste0(projects_folder, "NFL 2022/pff_expected_points/pff_skill_ep.csv"))
pff_qb_ep = read_csv(paste0(projects_folder, "NFL 2022/pff_expected_points/pff_qb_ep.csv"))

ep_df = pull_pff_ep(pff_skill_ep, pff_qb_ep, recency = 16) %>%
  mutate(player = pff_to_underdog_name_switch(player),
         pos = case_when(
           player == "Demetric Felton Jr." ~ "RB",
           T ~ pos
         ))

# projections df -----

create_projections_df = function(df){
  df %>%
    arrange(-usable_value, -elite_value) %>%
    mutate(my_rank = row_number(),
           ecr_rank = paste0(pos, rank),
           projection = round(projection, 2),
           injury_prob = 100*round(injury_prob, 2)) %>%
    group_by(pos) %>%
    mutate(pos_rank = row_number(),
           pos_rank = paste0(pos, pos_rank),
    ) %>%
    ungroup() %>%
    select(player, pos, team, projection, x_usable, hi_x_usable, x_elite, injury_probability = injury_prob, pos_rank, ecr_rank, my_rank, id = ud_id, usable_value, contains("wk"), contains("playoff")) %>%
    mutate(player = fp_to_underdog_name_switch(player)) %>%
    left_join(ep_df) %>%
    mutate(fp_eff = 100*round(diff/expected_points, 2)) %>%
    select(player, pos, team, projection, xfp_gm = expected_points, fp_eff,
           x_usable, hi_x_usable, x_elite, injury_prob = injury_probability, pos_rank, ecr_rank, my_ovr = my_rank, id, usable_value, contains("wk"), contains("playoff")) %>%
    mutate(player = fp_to_underdog_name_switch(player),
           injury_probability = round(injury_prob,2))
  
}


underdog_to_fdc_nameswitch = function(player){
  fdc_player = case_when(
    player == "Patrick Mahomes" ~ "Patrick Mahomes II",
    player == "D.K. Metcalf" ~ "DK Metcalf",
    player == "Travis Etienne" ~ "Travis Etienne Jr.",
    player == "Michael Pittman" ~ "Michael Pittman Jr.",
    player == "Melvin Gordon" ~ "Melvin Gordon III",
    player == "Kenneth Walker" ~ "Ken Walker III",
    player == "Allen Robinson" ~ "Allen Robinson II",
    player == "Darrell Henderson" ~ "Darrell Henderson Jr.",
    player == "Rob Gronkowski" ~ "Codey McElroy",
    player == "Ronald Jones" ~ "Ronald Jones II",
    player == "Robby Anderson" ~ "Robbie Anderson",
    player == "Ronald Jones" ~ "Ronald Jones II",
    player == "Odell Beckham" ~ "Deonte Harty",
    player == "Darrel Williams" ~ "Dwayne Washington",
    player == "Julio Jones" ~ "Stanley Morgan Jr.",
    player == "Mark Ingram" ~ "Mark Ingram II",
    player == "Marvin Jones" ~ "Marvin Jones Jr.",
    player == "D.J. Chark" ~ "DJ Chark Jr.",
    player == "Irv Smith" ~ "Irv Smith Jr.",
    player == "Laviska Shenault" ~ "Laviska Shenault Jr.",
    player == "John Metchie" ~ "John Metchie III",
    player == "Cedrick Wilson" ~ "Cedrick Wilson Jr.",
    player == "Donald Parham" ~ "Donald Parham Jr.",
    player == "Jeff Wilson" ~ "Jeff WIlson Jr.",
    player == "Duke Johnson" ~ "Duke Johnson Jr.",
    player == "Terrace Marshall" ~ "Terrace Marshall Jr.",
    player == "Velus Jones" ~ "Velus Jones Jr.",
    player == "Demetric Felton" ~ "Demetric Felton Jr.",
    player == "D'Wayne Eskridge" ~ "Dee Eskridge",
    player == "Justyn Ross" ~ "Josh Gordon",
    player == "Antonio Brown" ~ "Dezmon Patmon",
    player == "William Fuller" ~ "Bo Melton",
    T ~ player
  )
  return(fdc_player)
}


fdc_needed_names = c(
  "Jeff Smith",
  "Dwayne Washington",
  "Josh Gordon",
  "Bo Melton",
  "Codey McElroy",
  "Dezmon Patmon"
)


# Awesemo Name Adjust -----
awesemo_to_ff_nameadjust = function(player){
  nm = gsub("Eli Mitchell","Elijah Mitchell", player)
  nm = gsub("JJ Arcega-Whiteside", "J.J. Arcega-Whiteside", nm)
  nm = gsub("Benny Snell Jr.", "Benny Snell", nm)
  nm = gsub("DJ Chark Jr.", "D.J. Chark", nm)
  nm = gsub("Kenneth Walker", "Ken Walker III", nm)
  nm = gsub("Ray-Ray McCloud III", "Ray-Ray McCloud", nm)
  nm = gsub("Tyquan Thorton", "Tyquan Thornton", nm)
  nm = gsub("Brian Robinson Jr.", "Brian Robinson", nm)
  nm = gsub("Brian Robinson Jr. Jr.", "Brian Robinson", nm)
  nm = gsub("Chig Okonkwo", "Chigoziem Okonkwo", nm)
  return(nm)
}


etr_to_fp_nameadjust = function(player){
  nm = gsub("DK Metcalf", "D.K. Metcalf", player)
  nm = gsub("Kenneth Walker III", "Ken Walker IIr", nm)
  nm = gsub("Will Fuller", "William Fuller", nm)
  nm = gsub("Robbie Anderson", "Robby Anderson", nm)
  nm = gsub("Josh Palmer", "Joshua Palmer", nm)
  nm = gsub("K.J. Hamler", "KJ Hamler", nm)
}




# PFF Missing Names -----

# pff_players = ep_df %>%
#   select(player, pff_id, pos) %>%
#   clean_names() %>%
#   mutate(player = paste0(first_name, " ", last_name), 
#          pos = if_else(position == "HB", "RB", position),
#          pos = case_when(
#            player == "Taysom Hill" ~ "TE",
#            player == "Ty Montgomery" ~ "WR",
#            player == "J.J. Arcega-Whiteside" ~ "TE",
#            T ~ pos
#          )
#   ) %>%
#   select(pff_id = player_id, player, pos)


# pff_missing_df = pff_players %>%
#   mutate(player = pff_to_underdog_name_switch(player)) %>%
#   left_join(ud_players) %>%
#   filter(is.na(underdog_id))
