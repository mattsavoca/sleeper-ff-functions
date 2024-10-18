current_week = 1
library(easypackages)
packages("plotly","htmlwidgets",prompt = F)
dropbox = ifelse(Sys.info()['sysname'] == "Windows", "C:/Users/matts/dropbox/", "~/Dropbox/")
source(paste0(dropbox, "Matt Savoca/Projects/theme_FE.R"))
projects_folder = paste0(dropbox, "Matt Savoca/Projects/")

week_folder = paste0(projects_folder, "NFL 2022/Week ", 
                     if_else(current_week < 10, "", ""), 
                     as.character(current_week))


# name switch function ------
player_name_adjust = function(player){
  case_when(
    player == "Melvin Gordon III" ~ "Melvin Gordon",
    player == "Michael Pittman Jr." ~ "Michael Pittman",
    player == "Leviska Shenault Jr." ~ "Laviska Shenault",
    player == "Mark Ingram II" ~ "Mark Ingram",
    player == "Ray-ray McLoud III" ~ "Ray-ray McLoud",
    player == "Terrace Marshall Jr." ~ "Terrace Marshall",
    player == "AJ Dillon" ~"A.J. Dillon",
    player == "Willie Snead IV" ~"Willie Snead",
    TRUE ~ player
  )
}


# DK Values ------


proj_dk_raw = read_csv(paste0(week_folder,"/4for4_dk_proj.csv")) %>% clean_names()


proj_dk = proj_dk_raw %>%
  rename(salary = dk,
         proj_ff_pts = ff_pts)


qb_xfp_dk_raw = read_csv(glue("{week_folder}/qb_xfp_dk.csv")) %>% clean_names()
rure_xfp_dk_raw = read_csv(glue("{week_folder}/rure_xfp_dk.csv")) %>% clean_names()
#breakout_xfp = read_csv(glue("{week_folder}/aybl_raw.csv")) %>% clean_names()
rosters = nflreadr::load_rosters()



## DK XFP -----
pff_dk_xfp =  qb_xfp_dk_raw %>%
  select(
    player,
    xfp = points_per_game_x_ppg,
    pff_fp_gm = points_per_game_ppg
  ) %>% 
  rbind(rure_xfp_dk_raw %>%
          select(player, xfp = points_per_game_x_ppg,
                 pff_fp_gm = points_per_game_ppg)) %>%
  right_join(rosters %>% 
               filter(position %in% c("QB", "WR", "RB", "TE")) %>% 
               select(player = full_name, position)) %>%
  filter(!is.na(xfp)) %>%
  rename(pff_xfp = xfp) %>%
  distinct()



xfp_dk_df = pff_dk_xfp %>% #breakout_xfp %>%
  #select(player = name, xfp = l3_x_pts, position = pos, fp_gm = l3_f_pts) %>%
  #full_join(pff_dk_xfp) %>%
  mutate(
    pff_xfp = as.numeric(pff_xfp), 
    pff_fp_gm = as.numeric(pff_fp_gm)) %>%
  rowwise() %>%
  mutate(
    #xfp = if_else(is.na(pff_xfp), xfp, pff_xfp),
    #fp_gm = if_else(is.na(pff_fp_gm), fp_gm, pff_fp_gm),
    xfp = pff_xfp,
    fp_gm = pff_fp_gm
    ) %>%
  ungroup() %>%
  select(-pff_xfp, -pff_fp_gm) %>%
  arrange(-xfp)

### DK Value DF ------
dk_value_df = proj_dk %>%
  select(player, team, opp, opp_rk = a_fpa_rk, position = pos, salary, proj = proj_ff_pts, val = dk_val) %>%
  left_join(xfp_dk_df) %>%
  filter(!is.na(xfp), position != "DEF") %>%
  rename(pos = position) %>%
  mutate(
    pct_budget = salary/50000,
    player = case_when(
      player == "Michael Pittman Jr." ~ "Michael Pittman", TRUE ~ player)
  ) %>%
  group_by(pos) %>%
  mutate(
    pct_pos_max = round(salary/max(salary),2),
    fp_added = proj + max(proj[salary == min(salary)]),
    sal_added = salary - min(salary),
    budget_added = pct_budget - min(pct_budget),
    ) %>%
  ungroup() %>%
  mutate(sal_xfp = (salary/1000)/xfp,
         sal_proj = (salary/1000)/proj,
         sal_fp = (salary/1000)/fp_gm,
         matt_val = fp_added/budget_added) %>%
  arrange(sal_xfp)



#### DK Charting------
dk_value_chart = ggplotly(
  dk_value_df %>%
    mutate(
      pos2 = if_else(pos %in% c('WR','TE','RB'), "FLEX", pos),
      player_label = paste0(player, " (", salary_label, "), ", opp," (#", opp_rk, ")")) %>%
    filter(!player %in% c("Taysom Hilly")) %>%
    ggplot()+
    aes(salary, val, color = pos, label = player_label) %>%
    geom_jitter()+
    geom_smooth(aes(salary, val, color = pos2), method = "glm", se = F)+
    geom_smooth(aes(salary, val), se = F, size = .5, alpha = .3)+
    scale_x_continuous()+
    theme_FE+ 
    labs(x = "salary", y = "value", 
         title = paste0("DK Salary/Value Report, Week ", current_week),
         x = "Value",
         y = "Salary")+
    facet_wrap(~pos2, scales = "free")+
    # theme(legend.position = "none")+
    coord_flip()
)

dk_value_chart

saveWidget(dk_value_chart, file = paste0(week_folder,"/4for4 DK Val Chart.html"))


dk_value_df %>% 
  mutate(
    sal_fp = if_else(is.infinite(sal_fp), NA_real_, sal_fp),
    matt_val = if_else(is.infinite(matt_val), NA_real_, matt_val),
         ) %>%
  googlesheets4::write_sheet(ss = "https://docs.google.com/spreadsheets/d/1wid6hDLU7YMFGuRbwlX6k84SWXt-A7V1U2bm67FuL1c/edit?usp=sharing",
                                           sheet = "dk_week1")


dk_val_chart = dk_value_df %>%
  mutate(player = paste0(player, " (",pos,")"), pos = if_else(pos %in% c("WR","TE","RB"), "FLX", pos)) %>%
  arrange(sal_xfp) %>%
  mutate(player = reorder(player, -sal_xfp),
         player = reorder_within(player, -sal_xfp, pos)
         ) %>%
  group_by(pos) %>%
  top_n(65, -sal_xfp) %>%
  filter(sal_xfp*1000 < 1000) %>% 
  ggplot()+
  aes(sal_xfp*1000, player, label = player, fill = team, width = squish(xfp, range = c(.1, 1)))+
  geom_col(alpha = .6, color = "white")+
  geom_point(aes(sal_proj*1000), fill = "black", size = .5, alpha = .5)+
  scale_fill_nfl()+
  #scale_color_brewer(palette = "Dark2")+
  facet_wrap(. ~ pos, scales = "free") +
  scale_y_reordered()+
  coord_cartesian(xlim = c(0, 1000))+
  theme(legend.position = "none")+
  theme_FE + 
  labs(title = "DraftKings Salary per Expected Fantasy Point",
       x = "Value",
       y = "",
       caption =  "Chart: Matt Savoca @Draftaholic | Data: 4for4 & PFF | Chart Theme: Sam Hoppen @SamHoppen",
       subtitle = "Column: Salary Per XFP/Gm, Dot: Salary per Projected FP/Gm")


dk_val_chart

brand_nfl_plot(orig_plot = dk_val_chart,
               save_name = glue("{week_folder}/dk_val_chart_wk{current_week}.png"),
               logo =  T, logo_4for4_red = T, asp = 24/20, base_size = 9)



# FanDuel Values ------
proj_fd_raw = read_csv(paste0(week_folder,"/4for4_proj.csv")) %>% clean_names()


proj_fd = proj_fd_raw %>%
  rename(salary = fd,
         proj_ff_pts = ff_pts)

qb_xfp_raw = read_csv(glue("{week_folder}/qb_xfp.csv")) %>% clean_names()
rure_xfp_raw = read_csv(glue("{week_folder}/rure_xfp.csv")) %>% clean_names()
breakout_xfp = read_csv(glue("{week_folder}/aybl_raw.csv")) %>% clean_names()
rosters = nflreadr::load_rosters()


## FD XFP  -------
pff_xfp =  qb_xfp_raw %>%
  select(
    player,
    xfp = points_per_game_x_ppg,
    pff_fp_gm = points_per_game_ppg
  ) %>% 
  rbind(rure_xfp_raw %>%
          select(player, xfp = points_per_game_x_ppg,
                 pff_fp_gm = points_per_game_ppg)) %>%
  mutate(
    player = player_name_adjust(player)) %>%
  right_join(rosters %>% 
               filter(position %in% c("QB", "WR", "RB", "TE")) %>% 
               select(player = full_name, position) %>%
               mutate(player = player_name_adjust(player))) %>%
  filter(!is.na(xfp)) %>%
  rename(pff_xfp = xfp) %>%
  distinct()


xfp_df = breakout_xfp %>%
  select(player = name, xfp = l3_x_pts, position = pos, fp_gm = l3_f_pts) %>%
  full_join(pff_xfp) %>%
  rowwise() %>%
  mutate(pff_xfp = as.numeric(pff_xfp), pff_fp_gm = as.numeric(pff_fp_gm)) %>%
  mutate(xfp = if_else(is.na(pff_xfp), xfp, pff_xfp),
         fp_gm = if_else(is.na(pff_fp_gm), fp_gm, pff_fp_gm)) %>%
  ungroup() %>%
  select(-pff_xfp) %>%
  arrange(-xfp)


### FD Value DF -----
value_df = proj_fd %>%
  select(player, team, position = pos, salary, proj = proj_ff_pts, val = fd_val) %>%
  left_join(xfp_df) %>%
  filter(position != "DEF") %>%
  rename(pos = position) %>%
  mutate(
    player = case_when(
      player == "Michael Pittman Jr." ~ "Michael Pittman", 
      TRUE ~ player)
  ) %>%
  mutate(sal_xfp = (salary/1000)/xfp,
         sal_proj = (salary/1000)/proj,
         sal_fp = (salary/1000)/fp_gm,
         adj_val = scale(val) %>% as.numeric()) %>%
  group_by(pos) %>%
  mutate(pos_adj_val = scale(val) %>% as.numeric()) %>%
  ungroup() %>%
  arrange(sal_xfp)


#### FD Charting ------
fd_value_chart = ggplotly(
  value_df %>%
    mutate(pos2 = if_else(pos %in% c('WR','TE','RB'), "FLEX", pos)) %>%
    filter(!player %in%  c("P.J. Walker")) %>%
    ggplot()+
    aes(salary, val, color = pos, label = player) %>%
    geom_jitter()+
    geom_smooth(aes(salary, val, color = pos2),method="lm", formula= (y ~ log10(x)), se = T, alpha = .3)+
    geom_smooth(aes(salary, val), se = F, size = .5, alpha = .3)+
    scale_x_continuous()+
    theme_FE+ 
    labs(x = "salary", y = "value", 
         title = paste0("FD Salary/Value Report, Week ", current_week),
         x = "Value",
         y = "Salary")+
    facet_wrap(~pos2, scales = "free")+
    coord_flip()
  #theme(legend.position = "none")+
  
)

fd_value_chart

saveWidget(fd_value_chart, file = paste0(week_folder,"/4for4 FD Val Chart.html"))


fd_val_chart = value_df %>%
  mutate(
    
    player = paste0(player, " (",pos,")"), pos = if_else(pos %in% c("WR","TE","RB"), "FLX", pos)) %>%
  arrange(sal_xfp) %>%
  rowwise() %>%
  mutate(
    sal_xfp_min = if_else(sal_proj > sal_xfp, sal_proj, sal_xfp),
    sal_xfp_max = if_else(sal_proj > sal_xfp, sal_xfp, sal_proj),
    bar_color = if_else(sal_proj > sal_xfp, "green", "red")
  ) %>%
  ungroup() %>%
  mutate(player = reorder(player, -sal_xfp),
         player = reorder_within(player, -sal_proj, pos)) %>%
  group_by(pos) %>%
  top_n(65, -sal_xfp) %>%
  filter(sal_xfp*1000 < 1500 & sal_proj*1000 < 1500) %>%
  ggplot()+
  #aes(sal_xfp*1000, player, label = player, fill = team, width = squish(xfp, range = c(.1, 1)))+
  #geom_col(alpha = .6, color = "white")+
  geom_point(aes(sal_proj*1000, y = player, fill = team),  size = 1.5, alpha = .81)+
  geom_errorbarh(aes(xmin = sal_xfp_min*1000, xmax = sal_xfp_max*1000, y = player, color = bar_color), height = NA)+
  scale_fill_nfl()+
  #scale_color_brewer(palette = "Dark2")+
  facet_wrap(. ~ pos, scales = "free", nrow = 1) +
  scale_y_reordered()+
  #coord_cartesian(xlim = c(0, 1500))+
  theme(legend.position = "none")+
  theme_FE + 
  labs(title = "FanDuel Salary per Expected Fantasy Point",
       x = "Value",
       y = "",
       caption =  "Chart: Matt Savoca @Draftaholic | Data: 4for4 & PFF | Chart Theme: Sam Hoppen @SamHoppen",
       subtitle = "Column: Salary Per XFP/Gm, Dot: Salary per Projected FP/Gm")+
  scale_color_identity()




fd_val_chart = value_df %>%
  mutate(player = paste0(player, " (",pos,")"), pos = if_else(pos %in% c("WR","TE","RB"), "FLX", pos)) %>%
  arrange(sal_xfp) %>%
  mutate(player = reorder(player, -sal_xfp),
         player = reorder_within(player, -sal_xfp, pos)
  ) %>%
  group_by(pos) %>%
  top_n(65, -sal_xfp) %>%
  filter(sal_xfp*1000 < 1000) %>% 
  ggplot()+
  aes(sal_xfp*1000, player, label = player, fill = team, width = squish(xfp, range = c(.1, 1)))+
  geom_col(alpha = .6, color = "white")+
  geom_point(aes(sal_proj*1000), fill = "black", size = .5, alpha = .5)+
  scale_fill_nfl()+
  #scale_color_brewer(palette = "Dark2")+
  facet_wrap(. ~ pos, scales = "free") +
  scale_y_reordered()+
  coord_cartesian(xlim = c(0, 1000))+
  theme(legend.position = "none")+
  theme_FE + 
  labs(title = "FanDuel Salary per Expected Fantasy Point",
       x = "Value",
       y = "",
       caption =  "Chart: Matt Savoca @Draftaholic | Data: 4for4 & PFF | Chart Theme: Sam Hoppen @SamHoppen",
       subtitle = "Column: Salary Per XFP/Gm, Dot: Salary per Projected FP/Gm")






fd_val_chart


brand_nfl_plot(orig_plot = fd_val_chart,
               save_name = glue("{week_folder}/fd_val_chart_wk{current_week}_mainslate.png"),
               logo =  T, logo_4for4_red = T, asp = 24/20, base_size = 9)



# linear model and charting scratchwork -------

lm_df = value_df %>%
  filter(!is.infinite(sal_fp)) %>%
  recipe(salary ~ .) %>%
  step_rm(c(team, proj, player)) %>%
  step_dummy(pos) %>%
  prep() %>%
  juice()


lm(salary ~ val + pos_TE + pos_RB + pos_WR + pff_fp_gm, lm_df) %>% summary() 

regEq <- function(lmObj, dig) {
  gsub(":", "*", 
       paste0(
         names(lmObj$model)[1]," = ",
         paste0(
           c(round(lmObj$coef[1], dig), round(sign(lmObj$coef[-1])*lmObj$coef[-1], dig)),
           c("", rep("*", length(lmObj$coef)-1)),
           paste0(c("", names(lmObj$coef)[-1]), c(ifelse(sign(lmObj$coef)[-1]==1," + "," - "), "")),
           collapse=""
         )
       )
  )
}

regEq(lm(salary ~ val + pos_TE + pos_RB + pos_WR, data=lm_df), 1)

final_value_df = value_df %>%
  mutate(lin_mod = case_when(pos == 'TE' ~ -1800,
                             pos == 'RB' ~ -1180,
                             pos == 'WR' ~ -1076,
                             TRUE ~ 0),
         x_salary = round((6695.2 + 249.5*val + lin_mod)/100)*100,
         sal_vs_exp = salary - x_salary,
         salary_value = salary/x_salary - 1
         ) %>%
  select(player, team, pos, salary, x_salary, sal_vs_exp, everything()) %>%
  arrange(-x_salary)
  

value_df %>% 
  filter(
    (xfp > 8) | (pos == "TE" & xfp > 5)
    ) %>%
  rowwise() %>%
  mutate(fp_min = min(sal_xfp, sal_fp)*1000,
         fp_max = max(sal_xfp, sal_fp)*1000) %>%
  ungroup() %>%
  mutate(
    player = paste0(player, ", ",pos ," (", team, ")"),
    player = reorder_within(player, proj, pos)) %>%
  top_n(150, proj) %>%
  ggplot()+
  aes(xmin = fp_min, xmax = fp_max, y = player, color = team)+
  geom_errorbarh(height = 0, alpha = .3)+
  geom_point(aes(x = sal_proj*1000))+
  labs(x = "Projected Value", y = "")+
  scale_color_nfl()+
  scale_x_reverse()+
  facet_wrap(pos ~ ., scales = "free")+
  scale_y_reordered()+
  theme_FE
    
#   
#   
# ggplotly(
#   value_df %>% 
#     ggplot()+
#     aes(salary, sal_xfp, label = player, fill = pos)+
#     geom_point(show.legend = F)+
#     scale_y_reverse(breaks = seq(12000, 0, -250))+
#     geom_smooth(aes(salary, sal_xfp, color = pos), se =  F, method = "glm", alpha = .4)+
#     theme_FE+
#     theme(legend.position = "none")
#   
# )
