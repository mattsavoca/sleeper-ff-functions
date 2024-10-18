options(tidyverse.quiet = TRUE)
options(nflreadr.verbose = FALSE)
# Helper Functions, Filepaths & Chart Themes -----
dropbox = ifelse(Sys.info()['sysname'] == "Windows", "C:/Users/matts/dropbox/", "~/Dropbox/")
source(paste0(dropbox, "Matt Savoca/Projects/NFL 2022/ff_projections_helper_functions.R"))
source(paste0(dropbox, "Matt Savoca/Projects/theme_FE.R"))
theme_set(theme_light())


# League Settings ----
drafters_settings_id = 848567866847510528


# Sims -----

# connection
conn = get_UD_league_settings(ud_settings_id)

# ranks
custom_drafters_ranks = generate_custom_premium_ranks(drafters_ranks, 
                                             awesemo_drafters_ranks, 
                                             etr_drafters_ranks, 
                                             awesemo_rb_influence = .1, etr_rb_influence = .9,
                                             awesemo_qb_influence = .1, etr_qb_influence = .9,
                                             awesemo_wr_influence = .1, etr_wr_influence = .9,
                                             awesemo_te_influence = .1, etr_te_influence = .9)

#projections
plan("multisession")


drafters_projections = generate_custom_projections_df(
  conn, 
  custom_ranks = custom_drafters_ranks, 
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

drafters_proj_export = drafters_projections %>%
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

drafters_proj_export %>%
  write_csv("my_drafters_proj.csv")

# Export to gSheets ------
drafters_proj_export %>%
  mutate(
    usable_value = round(usable_value),
    adp = as.numeric(adp),
  ) %>%
  arrange(adp) %>%
  googlesheets4::range_write(
    ss = "https://docs.google.com/spreadsheets/d/1s3yA1KYRoN-B_BPKPWlm70I4kpzfnxsMPsSsqtQdFFE/edit?usp=sharing",
    sheet = "drafters_staging",
    range = "A:U"
  )