library(dplyr)
library(tidyverse)
library(nflfastR)
library(ffscrapr)

###Insert ID in the quotes
conn <- ff_connect(platform = "Sleeper", league_id = "", season = 2025)
league_info <- ff_league(conn)

conn_24 <- ff_connect(platform = "Sleeper", league_id = "", season = 2024)
league_info_24 <- ff_league(conn_24)

conn_23 <- ff_connect(platform = "Sleeper", league_id = "", season = 2023)
league_info_23 <- ff_league(conn_24)

conn_22 <- ff_connect(platform = "Sleeper", league_id = "", season = 2022)
league_info_22 <- ff_league(conn_22)

conn_21 <- ff_connect(platform = "Sleeper", league_id = "", season = 2021)
league_info_21 <- ff_league(conn_21)

###Franchises#
mapping <- tribble(
  ~Player,    ~season, ~franchise_id,
  "Name",     2021, 1,
)

teams_25 <- ff_rosters(conn) %>% 
  select(franchise_id) %>%
  mutate(season = 2025, league_id = "")

teams_24 <- ff_rosters(conn_24) %>% 
  select(franchise_id) %>%
  mutate(season = 2024, league_id = "")

teams_23 <- ff_rosters(conn_23) %>% 
  select(franchise_id) %>%
  mutate(season = 2023, league_id = "")

teams_22 <- ff_rosters(conn_22) %>% 
  select(franchise_id) %>%
  mutate(season = 2022, league_id = "")

teams_21 <- ff_rosters(conn_21) %>% 
  select(franchise_id) %>%
  mutate(season = 2021, league_id = "")

teams_all <- bind_rows(teams_25, teams_24, teams_23, teams_22, teams_21)

teams_all_fixed <- teams_all %>%
  mutate(franchise_id = as.character(franchise_id)) %>%
  left_join(
    mapping %>% mutate(franchise_id = as.character(franchise_id)),
    by = c("season", "franchise_id")
  )

standings_25 <- ff_standings(conn) %>% mutate(season = 2025)
standings_24 <- ff_standings(conn_24) %>% mutate(season = 2024)
standings_23 <- ff_standings(conn_23) %>% mutate(season = 2023)
standings_22 <- ff_standings(conn_22) %>% mutate(season = 2022)
standings_21 <- ff_standings(conn_21) %>% mutate(season = 2021)

standings_25 <- ff_standings(conn) %>% mutate(season = 2025)

standings_all <- bind_rows(
  standings_25,
  standings_24,
  standings_23,
  standings_22,
  standings_21
)

standings_all_fixed <- standings_all %>%
  mutate(franchise_id = as.character(franchise_id)) %>%
  left_join(
    mapping %>% mutate(franchise_id = as.character(franchise_id)),
    by = c("season", "franchise_id")
  )

team_stats <- standings_all_fixed %>%
  mutate(ppg = points_for / (h2h_wins + h2h_losses + h2h_ties)) %>%
  select(season, Player, franchise_id, h2h_wins, h2h_losses, h2h_winpct ,points_for, points_against, ppg, allplay_winpct)

lifetime_stats <- team_stats %>%
  group_by(Player) %>%
  summarise(
    seasons_played = n(),
    avg_wins = mean(h2h_wins),
    avg_ppg = mean(ppg, na.rm = TRUE),
    avg_allplay_winpct = mean(allplay_winpct, na.rm = TRUE),
    total_points = sum(points_for, na.rm = TRUE)
  ) %>%
  arrange(desc(avg_ppg))

ff_schedule(conn)
sched_25 <- ff_schedule(conn) %>% mutate(season = 2025)
sched_24 <- ff_schedule(conn_24) %>% mutate(season = 2024)
sched_23 <- ff_schedule(conn_23) %>% mutate(season = 2023)
sched_22 <- ff_schedule(conn_22) %>% mutate(season = 2022)
sched_21 <- ff_schedule(conn_21) %>% mutate(season = 2021)

sched_25 <- ff_schedule(conn) %>% mutate(season = 2025)

schedule_all <- bind_rows(
  sched_25,
  sched_24,
  sched_23,
  sched_22,
  sched_21
)

trans_25 <- ff_transactions(conn) %>% mutate(season = 2025)
trans_24 <- ff_transactions(conn_24) %>% mutate(season = 2024)
trans_23 <- ff_transactions(conn_23) %>% mutate(season = 2023)
trans_22 <- ff_transactions(conn_22) %>% mutate(season = 2022)
trans_21 <- ff_transactions(conn_21) %>% mutate(season = 2021)

transactions_all <- bind_rows(trans_25, trans_24, trans_23, trans_22, trans_21)

transactions_all_fixed <- transactions_all %>%
  mutate(franchise_id = as.character(franchise_id)) %>%
  left_join(mapping %>% mutate(franchise_id = as.character(franchise_id)),
            by = c("season", "franchise_id"))

trades_only <- transactions_all_fixed %>%
  filter(type == "trade") %>%
  select(season, Player, player_name, type, timestamp)

schedule_named <- schedule_all %>%
  mutate(
    franchise_id = as.character(franchise_id),
    opponent_id = as.character(opponent_id)
  ) %>%
  left_join(
    mapping %>% mutate(franchise_id = as.character(franchise_id)),
    by = c("season", "franchise_id")
  ) %>%
  rename(Player = Player) %>%
  left_join(
    mapping %>% mutate(franchise_id = as.character(franchise_id)),
    by = c("season" = "season", "opponent_id" = "franchise_id")
  ) %>%
  rename(Opponent = Player.y) %>%
  rename(Player = Player.x)

mapping <- mapping %>%
  mutate(franchise_id = as.character(franchise_id))

draft_all <- bind_rows(
  ff_draft(conn)     %>% mutate(season = 2025),
  ff_draft(conn_24)  %>% mutate(season = 2024),
  ff_draft(conn_23)  %>% mutate(season = 2023),
  ff_draft(conn_22)  %>% mutate(season = 2022),
  ff_draft(conn_21)  %>% mutate(season = 2021)
) %>%
  mutate(franchise_id = as.character(franchise_id)) %>%
  left_join(mapping, by = c("season", "franchise_id"))

recover_data <- function() {
  env <- .GlobalEnv
    if (!exists("mapping", envir = env)) stop("mapping must exist in the global environment.")
  if (!exists("standings_all", envir = env) && !exists("standings_all_fixed", envir = env)) {
    stop("standings_all or standings_all_fixed must exist. Please load ff_standings() results.")
  }
  if (!exists("schedule_all", envir = env)) stop("schedule_all must exist (combined ff_schedule results).")
  if (!exists("transactions_all", envir = env) && !exists("transactions_all_fixed", envir = env)) {
    stop("transactions_all or transactions_all_fixed must exist (combined ff_transactions results).")
  }
    if (exists("standings_all_fixed", envir = env)) {
    standings <- get("standings_all_fixed", envir = env)
  } else {
    standings <- get("standings_all", envir = env) %>%
      mutate(franchise_id = as.character(franchise_id)) %>%
      left_join(mapping %>% mutate(franchise_id = as.character(franchise_id)),
                by = c("season", "franchise_id"))
  }
    if (!"h2h_wins" %in% names(standings) && "wins" %in% names(standings)) {
    standings <- standings %>% rename(h2h_wins = wins)
  }
  if (!"h2h_losses" %in% names(standings) && "losses" %in% names(standings)) {
    standings <- standings %>% rename(h2h_losses = losses)
  }
  if (!"points_for" %in% names(standings) && "points_for" %in% names(standings)) {
  }
  if (!"allplay_winpct" %in% names(standings) && "all_play_winpct" %in% names(standings)) {
    standings <- standings %>% rename(allplay_winpct = all_play_winpct)
  }
  
  standings <- standings %>%
    mutate(
      games_played = pmax(h2h_wins + h2h_losses + ifelse("h2h_ties" %in% names(.), h2h_ties, 0), 1),
      ppg = ifelse(!is.na(points_for), points_for / games_played, NA_real_)
    )
  
  if (!exists("schedule_named", envir = env)) {
    sched <- get("schedule_all", envir = env)
    sched <- sched %>%
      mutate(franchise_id = as.character(franchise_id),
             opponent_id = as.character(opponent_id),
             season = season)
    sched_named <- sched %>%
      left_join(mapping %>% mutate(franchise_id = as.character(franchise_id)),
                by = c("season", "franchise_id")) %>%
      rename(Player = Player) %>%
      left_join(mapping %>% mutate(franchise_id = as.character(franchise_id)),
                by = c("season" = "season", "opponent_id" = "franchise_id")) %>%
      rename(Opponent = Player.y) %>%
      rename(Player = Player.x)
  } else {
    sched_named <- get("schedule_named", envir = env)
  }
  
  if (exists("transactions_all_fixed", envir = env)) {
    transactions <- get("transactions_all_fixed", envir = env)
  } else {
    transactions <- get("transactions_all", envir = env) %>%
      mutate(franchise_id = as.character(franchise_id)) %>%
      left_join(mapping %>% mutate(franchise_id = as.character(franchise_id)),
                by = c("season", "franchise_id"))
  }
  
  if (!exists("trades_only", envir = env)) {
    trades <- transactions %>% filter(type == "trade")
  } else {
    trades <- get("trades_only", envir = env)
  }
  
  list(
    mapping = mapping,
    standings = standings,
    schedule_all = get("schedule_all", envir = env),
    schedule_named = sched_named,
    transactions = transactions,
    trades = trades
  )
}

write.csv(mapping, "~/Desktop/App_Folder/data/mapping.csv", row.names = FALSE)
write.csv(standings_all_fixed, "~/Desktop/App_Folder/data/standings_all_fixed.csv", row.names = FALSE)
write.csv(schedule_named, "~/Desktop/App_Folder/data/schedule_named.csv", row.names = FALSE)


data_obj <- recover_data()

getwd()
list.files("data")
