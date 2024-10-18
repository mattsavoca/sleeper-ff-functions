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



# Charting Results -----

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
