options(tidyverse.quiet = TRUE)
options(nflreadr.verbose = FALSE)
# Helper Functions, Filepaths & Chart Themes -----
dropbox = ifelse(Sys.info()['sysname'] == "Windows", "C:/Users/matts/dropbox/", "~/Dropbox/")
source(paste0(dropbox, "Matt Savoca/Projects/NFL 2022/sim_projections_helpers.R"))
source(paste0(dropbox, "Matt Savoca/Projects/theme_FE.R"))
theme_set(theme_light())

plan("multisession")

# League Settings ----
conn = get_UD_league_settings()


custom_ranks = generate_custom_premium_ranks(ud_ranks, awesemo_ranks, etr_ranks, awesemo_rb_influence = .3, etr_rb_influence = .7)


# Generate projections --- 

ud_projections = generate_custom_projections_df(conn, 
  adp_outcomes = adp_outcomes_raw, 
  custom_ranks = custom_ranks, 
  te_flexmax_pct = .65,
  custom_influence = .9,
  n_seasons = 5000, 
)



ud_projections %>% 
  select(player, pos, contains("fpts_")) %>%
  mutate_if(is.numeric, round,2) %>%
  mutate(flex = if_else(pos == "QB", 0, 1))%>%
  View()

plan("sequential")

# Export to Ranks Sheet  ------

ud_proj_export = ud_projections %>%
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

ud_proj_export %>%
  write_csv("my_ud_ranks.csv")


ud_proj_export %>%
  mutate(
    #usable_value = if_else(pos == "QB", usable_value * 1.5, usable_value), #Superflex Toggle
    usable_value = round(usable_value),
    adp = as.numeric(adp),
  ) %>%
  arrange(adp) %>%
  googlesheets4::range_write(
    ss = "https://docs.google.com/spreadsheets/d/1s3yA1KYRoN-B_BPKPWlm70I4kpzfnxsMPsSsqtQdFFE/edit?usp=sharing",
    sheet = "my_sheet_staging",
    range = "A:Q"
  )




# FDC Export -----

fdc_names_raw = read_csv("fdc_names.csv") %>% clean_names()


# Projections with fdc_name as player name
ud_fc_joined = 
  ud_proj_export %>%
  left_join(
    fdc_names_raw %>% 
      mutate(
        player_raw = player, 
        #player = underdog_to_fdc_nameswitch(player),
        pos = case_when(
          player == "Taysom Hill" ~ "TE",
          T ~ pos
        )) %>%
      select(-adp, -team)) %>%
  mutate(
    fdc_player = if_else(
      !is.na(player_raw), player_raw,
      underdog_to_fdc_nameswitch(player)
    )
  )


fdc_export = fdc_names_raw %>%
  select(-team, -proj, -adp) %>%
  left_join(
    ud_fc_joined %>% 
      filter(!player %in% fdc_needed_names) %>%
      mutate(player_orig = player) %>% 
      select(player = fdc_player, team, pos, proj, adp, id, player_orig)
  ) %>% 
  mutate(
    adp = if_else(is.na(as.numeric(adp)), 0.0, as.numeric(adp)),
  ) %>%
  select(-proj) %>%
  left_join(
    ud_proj_export %>% 
      filter(!player %in% fdc_needed_names) %>% 
      select(id, proj = x_usable)) %>%
  transmute(player, team, pos, 
            proj = if_else(is.na(as.numeric(proj)), 0.0, as.numeric(proj)*10), 
            adp, player_orig, id) %>% 
  distinct()



fdc_dupes = fdc_export %>% group_by(player, pos) %>% summarize(n = n()) %>% filter(n > 1)

fdc_ids = fdc_export %>% pull(id)

fdc_missing = ud_proj_export %>% filter(!id %in% fdc_ids)

fdc_export %>% 
  googlesheets4::range_write(
    ss = "https://docs.google.com/spreadsheets/d/1lhGcl1aSSeu2Xe5khXqXsKK3u0YwAc6QjO6D86aJ318/edit?usp=sharing",
    sheet = "fdc_export", range = "A:G"
  )

# Charting projections -------
usable_weeks_plot = ud_projections %>% ff_usable_weeks_plot()
ud_projections %>% ff_laterounds_plot()

 


api_create(
  x = usable_weeks_plot,
  filename = "NFL usable weeks plot",
  fileopt = "overwrite",
  sharing = c("public"),
  world_readable=T
)

ggplotly(
  ud_projections %>%
    mutate(
      player = fp_to_underdog_name_switch(player),
      #usable_val = round(usable_value,1),
      #usable_value = round(usable_value),
      elite_value = round(elite_value, 1),
      player_value = if_else(usable_value < 0, elite_value, usable_value)
    ) %>%
    left_join(ud_ranks) %>%
    mutate(adp = adp %>% as.numeric()) %>%
    arrange(usable_value, elite_value) %>%
    mutate(
      player = factor(player, levels = player)) %>%
    filter(x_usable > 0) %>%
    ggplot()+
    aes(
      y = adp, x = player_value, color = pos,
      label = player
    )+
    geom_text(
      size = 2.5,
      #min.segment.length = 5,
      #max.overlaps = getOption("ggrepel.max.overlaps", default = 500),
    )+
    scale_x_reverse(breaks = seq(11, -8, -1))+
    scale_y_reverse(breaks = seq(0, 260, 12), labels = seq(1,22,1))+
    labs(
      x = "Usability Value",
      y = "ADP"
    )
)


# FDC Name Changes -----

fdc_export %>%
  filter(player_orig != player) %>%
  View()

# Check for Missing IDs -----------
ud_proj_temp = generate_custom_projections_df(conn, 
                                              adp_outcomes = adp_outcomes_raw, 
                                              custom_ranks = custom_ranks, 
                                              te_flexmax_pct = .65,
                                              custom_influence = .9,
                                              n_seasons = 10)

ud_proj_temp %>%
  create_projections_df() %>%
  left_join(
    ud_ranks %>% select(id = ud_id, adp)
  ) %>%
  filter(is.na(id))




# scratch work | COACH ---

ud_projections %>%
  select(
    player,
    fantasypros_id,
    ecr,
    flex,
    pos,
    team,
    lo_proj,
    proj = projection,
    hi_proj,
    prank_x_score
  ) %>%
  mutate(vbd_baseline = nth(proj, 168),
         qb_baseline = nth(proj[pos == "QB"], 24),
         #qb_baseline = vbd_baseline
  ) %>%
  rowwise() %>%
  mutate(vbd = if_else(pos == "QB", proj - qb_baseline, proj - vbd_baseline), 
         lo_vbd = if_else(pos == "QB", lo_proj - qb_baseline, lo_proj - vbd_baseline),
         hi_vbd = if_else(pos == "QB", hi_proj - qb_baseline, hi_proj - vbd_baseline)) %>%
  ungroup() %>%
  arrange(-vbd) %>%
  mutate(vbd  = if_else(vbd <0, 0, vbd),
         lo_vbd = if_else(lo_vbd < 0, 0, lo_vbd),
         hi_vbd = if_else(hi_vbd < 0, 1, hi_vbd),
         pos_proj_rk = min_rank(-vbd)) %>%
  mutate(
    total_vbd = sum(vbd[vbd >= 0]),
    excess_budget = 12*200-(12*1*15),
    vbd_value = excess_budget/total_vbd,
    av = round(vbd_value  * vbd, 0),
    lo_av=  round(vbd_value  * lo_vbd, 0),
    hi_av =  round(vbd_value  * hi_vbd, 0)
  ) %>% 
  group_by(pos) %>%
  mutate(pos_av_rk = min_rank(-proj),
         pos_av_sum = sum(av),
         pos_av_share = av/pos_av_sum) %>%
  ungroup() %>%
  mutate(qb1_prem_av = sum(av[pos == "QB" & pos_av_rk >= 12]),
         qb1_prem_av_share = qb1_prem_av/pos_av_sum) %>%
  rowwise() %>%
  mutate(qb_extra_value = round(pos_av_share * qb1_prem_av),
         av = case_when(
           pos == "QB" ~ av + qb_extra_value,
           T ~ av)) %>%
  filter(pos == "QB") %>%
  glimpse()
  
  

# scratch work ------





# 
#   
# 
# 
# ud_players = read_csv(paste0(dropbox, "Matt Savoca/Projects/NFL 2022/ud_players.csv")) %>%
#   clean_names() %>%
#   mutate(player = paste0(first_name, " ", last_name)) %>%
#   select(underdog_id = id, player, pos = slot_name)
# 
# 
# 
# 
# fp_players = ffs_latest_rankings() %>%
#   filter(pos %in% c("QB","RB","WR","TE")) %>%
#   select(player, fantasypros_id, pos) %>%
#   mutate(
#     player = fp_to_underdog_name_switch(player),
#     )
#     
# 
# 
# 
#  
# 
# 
# 



# not_in_awesemo = ud_ranks %>%
#   mutate(adp = as.numeric(adp)) %>%
#   left_join(awesemo_ranks %>% select(ud_id, awesemo_rank)) %>%
#   filter(is.na(awesemo_rank), adp < 225)
