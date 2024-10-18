# underdog ranks -----
ud_ranks_raw = read_csv(paste0(dropbox, "Matt Savoca/Projects/NFL 2022/ud_adp/rankings-f659a9be-fd34-4a1e-9c43-0816267e603d-ccf300b0-9197-5951-bd96-cba84ad71e86.csv")) %>% clean_names()
ud_sf_ranks_raw = read_csv(paste0(dropbox, "Matt Savoca/Projects/NFL 2022/ud_adp/superflex/rankings-5549297e-8cd2-4dfa-b197-2c472813bfb7-ccf300b0-9197-5951-bd96-cba84ad71e86.csv")) %>% clean_names()


#drafters ranks -----

drafters_ranks_raw = read_csv(paste0(dropbox, "Matt Savoca/Projects/NFL 2022/drafters_adp/drafters_players.csv")) %>% clean_names()

# import premium projections  -----
etr_bb_ranks_raw = read_csv(paste0(dropbox, "Matt Savoca/Projects/NFL 2022/ETR Ranks/ETR Underdog Rankings.csv")) %>% clean_names()

etr_sf_ranks_raw = read_csv(paste0(dropbox, "Matt Savoca/Projects/NFL 2022/ETR Ranks/ETR Underdog SuperFlex Rankings and ADP.csv")) %>% clean_names()

etr_drafters_ranks_raw = read_csv(paste0(dropbox, "Matt Savoca/Projects/NFL 2022/ETR Ranks/ETR Drafters Rankings and ADP.csv")) %>% clean_names()

etr_2qb_ranks_raw = read_csv(paste0(dropbox, "Matt Savoca/Projects/NFL 2022/ETR Ranks/ETR 2QB PPR Rankings and ADP.csv")) %>% clean_names()

awesemo_ranks_raw = read_csv(paste0(dropbox, "Matt Savoca/Projects/NFL 2022/Stokastic Ranks/Underdog Tourney Best Ball Rankings.csv")) %>% 
  clean_names() 

awesemo_sf_ranks_raw = read_csv(paste0(dropbox, "Matt Savoca/Projects/NFL 2022/Stokastic Ranks/Stokastic Underdog Best Ball Rankings SuperFlex.csv")) %>% 
  clean_names() 

awesemo_drafters_ranks_raw = read_csv(paste0(dropbox, "Matt Savoca/Projects/NFL 2022/Stokastic Ranks/Drafters Best Ball Rankings.csv")) %>% 
  clean_names() 


# awesemo projection file adjustments ------

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
        player == "Greg  Dulcich" ~ "9bcf9b0e-ef6a-41ad-ab4c-b503d26b9f5c",
        player == "Giovanni Ricci" ~ "b5b06ba4-1c73-4e31-abf7-3edce9118d84",
        player == "Teagan Quitoriano" ~ "38e64080-d555-4ae8-af92-a400976affa2",
        player == "Tyquan Thornton" ~ "52f9e165-6ace-445e-95e3-cc2c2b5ed5ca",
        player == "Ray-Ray McCloud" ~ "550da15a-f6a7-40e4-adab-c5ab9a05464c",
        T ~ ud_id))
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
        player == "Greg  Dulcich" ~ "dfa21407-6029-4f82-b27f-b50e0f55e236",
        player == "Giovanni Ricci" ~ "f0c3faa3-6ec0-442d-949a-1c0674abf5c2",
        player == "Teagan Quitoriano" ~ "eb9ed781-537b-4f38-b2bf-6edf503851a4",
        player == "Tyquan Thornton" ~ "0a366caa-62a9-488b-9253-0bbc9770023e",
        player == "Ray-Ray McCloud" ~ "7b5f1faa-1e54-4fd0-92dd-87ca13bdf2a5",
        T ~ ud_id))
}


awesemo_drafters_df_adjustments = function(awesemo_drafters_ranks){
  awesemo_drafters_ranks %>% 
    group_by(position) %>%
    mutate(awesemo_posrank = row_number()) %>%
    ungroup() %>%
    select(player = name, pos = position, ud_id = id, awesemo_rank = awesemo_posrank) %>%
    filter(pos %in% c("QB","RB","WR","TE"), player != 0)
}


## Awesemo Single-QB ID to Superflex ID (Currently Broken) -----

switch_ud_id_to_sf = function(awesemo_ranks){
  awesemo_ranks %>%
    left_join(awesemo_sf_ranks %>% select(player, sf_ud_id = ud_id)) %>%
    select(-ud_id) %>%
    rename(ud_id = sf_ud_id)
}


## Adjusted Projection Docs -----


awesemo_ranks = awesemo_ranks_raw %>% awesemo_df_adjustments()


awesemo_drafters_ranks = awesemo_drafters_ranks_raw %>% awesemo_drafters_df_adjustments()

awesemo_sf_ranks = awesemo_sf_ranks_raw %>% 
  awesemo_superflex_df_adjustments()

awesemo_2qb_ranks = awesemo_ranks_raw %>% awesemo_df_adjustments() %>% switch_ud_id_to_sf()




#Drafters Ranks Adjustments ------

drafters_ranks = drafters_ranks_raw %>%
  transmute(
    player = name,
    ud_id = id,
    pos = case_when(
      position == "RB,FB" ~ "RB",
      position == "WR,CB" ~ "WR",
      position == "T, TE" ~ "TE",
      T ~ position),
    team_name = x5,
    adp
  ) %>%
  group_by(pos) %>%
  mutate(pos_adp = row_number()) %>%
  ungroup() %>%
  filter(pos %in% c("QB","RB","WR","TE"))

# Underdog Ranks Adjustments -------

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


# ETR Ranks -------

etr_bb_ranks = etr_bb_ranks_raw %>%
  group_by(position) %>%
  mutate(etr_rank = min_rank(etr_rank)) %>%
  ungroup() %>%
  select(player, pos = position, etr_rank, ud_id = id) 


etr_drafters_ranks = etr_drafters_ranks_raw %>%
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



