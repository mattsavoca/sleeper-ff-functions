# ETL ----------
fffl_conn = sleeper_connect(season = 2022, league_id = 843584157354401792)

fffl_rosters_final = ffscrapr::ff_rosters(fffl_conn)

sleeper_players = sleeper_players()

sleeper_players %>% filter(player_name == "Robert Woods") %>% glimpse()

gsis_rosters_22 = nflreadr::load_rosters(seasons = 2022)

nfl_rosters = gsis_rosters_22 %>% select(sportradar_id, first_name, last_name, team)

nfl_rosters %>% filter(first_name == "Robert", last_name == "Woods") %>% glimpse()

fffl_draft_rosters_22 = ffscrapr::ff_draft(fffl_conn) %>% 
  select(franchise_id, franchise_name, 
         player_id, player_name, pos, age, team) %>% 
  mutate(franchise_id = as.character(franchise_id))

# Data ------

fffl_final_rosters = fffl_rosters_final %>%
  rename(team_23 = team) %>%
  left_join(sleeper_players %>% select(
    -age, 
    player_name,
    -pos, 
    -team,
    -contains(c("swish", "fantasy_data", "stats_id", "rotowire_id", 
                "espn_id", "rotoworld", "pandascore")))) %>% 
  rename(sleeper_id = player_id) %>% 
  left_join(nfl_rosters) 

fffl_draft_roster = fffl_draft_rosters_22 %>% 
  rename(team_23 = team) %>%
  left_join(sleeper_players %>% select(
    -age, 
    -player_name, 
    -pos, -team, 
    -contains(
      c("swish", "fantasy_data", "stats_id", "rotowire_id", "espn_id", 
        "rotoworld", "pandascore")))) %>% 
  rename(sleeper_id = player_id) %>% 
  left_join(nfl_rosters) 
  

# Check Mean Players Per Team ----------
# (Draft: 5.626, Final: 5.75)
fffl_draft_roster %>%
  mutate(team = case_when(player_name == "Sony Michel" ~ "LA", player_name == "Darrell Henderson" ~ "LA", T ~ team)) %>%
  group_by(team) %>%
  count() %>%
  arrange(-n) %>%
  pull(n) %>%
  mean()

sfffl_final_rosters %>%
  group_by(team) %>%
  count() %>%
  arrange(-n) %>%
  pull(n) %>%
  mean()

# Graphing Players Per Team ----------
fffl_final_rosters %>%
  group_by(team) %>%
  count() %>%
  ungroup() %>%
  arrange(-n) %>%
  mutate(team = reorder(team, n)) %>%
  ggplot()+
  aes(n, team, fill = team) +
  geom_col() +
  scale_fill_nfl()+
  theme_light()+
  geom_vline(aes(xintercept = mean(n)), lty = 2, color = "red")+
  scale_x_continuous(breaks = seq(0, 10))+
  labs(
    y = "",
    x = "Players Rostered"
  )


fffl_draft_roster %>% # filter(is.na(team)) %>% View()
  mutate(team = case_when(player_name == "Sony Michel" ~ "LA", player_name == "Darrell Henderson" ~ "LA", T ~ team)) %>%
  group_by(team) %>%
  count() %>%
  ungroup() %>%
  arrange(-n) %>%
  mutate(team = reorder(team, n)) %>%
  ggplot()+
  aes(n, team, fill = team) +
  geom_col() +
  scale_fill_nfl()+
  theme_light()+
  geom_vline(aes(xintercept = mean(n)), lty = 2, color = "red")+
  scale_x_continuous(breaks = seq(0, 10))+
  labs(
    y = "",
    x = "Players Rostered"
  )


fffl_final_rosters %>% 
  mutate(
    # Fixing NAs for Team
    team = case_when(
      player_name == "Sony Michel" ~ "LA", 
      player_name == "Darrell Henderson" ~ "LA", 
      T ~ team)) %>%
  group_by(team) %>%
  count() %>%
  ungroup() %>%
  arrange(-n) %>%
  mutate(team = reorder(team, n)) %>%
  ggplot()+
  aes(n, team, fill = team) +
  geom_col() +
  scale_fill_nfl()+
  theme_light()+
  geom_vline(aes(xintercept = mean(n)), lty = 2, color = "red")+
  scale_x_continuous(breaks = seq(0, 10))+
  labs(
    y = "",
    x = "Players Rostered"
  )


# 10 Chiefs Lol -------
fffl_rosters %>%
  filter(team == "KC") %>%
  View()
