source("hoop_math_scrape.R")
# leaderboard <- scrape_hoop_math_leader_tables(years = 2020, type = "OffDefLeader", off_def = "Offense")
# hoop_math_names <- tibble(team = leaderboard$team,
#                           team_no_spaces = str_remove_all(leaderboard$team, " "))
# 
# write_csv(hoop_math_names, "hoop_math_names.csv")

# hoop-math.com team names and table types
hoop_math_names <- read_csv("hoop_math_names.csv")
types <- c("OffTable", "RebTable", "TransOTable", "DefTable", "TransDTable")

# function to update yearly leaderboards (off, def, trans o, trans d)
update_leaderboard <- function(years, new_games = F) {
  for (i in 1:length(years)) {
    # get year
    year <- years[i]
    year_lab <- paste0(as.numeric(year) - 1, "-", str_extract(year, "[012][0-9]$"))
    message(paste0("updating leaderboard for year: ", year_lab))
    if (!dir.exists(paste0("data/", year_lab))) {
      dir.create(paste0("data/", year_lab))
    }
    leaderboard <- try(read_csv(paste0("data/", year_lab,"/leaderboard.csv")))
    if (class(leaderboard)[1] == "try-error" | new_games == T) {
      leaderboard <- combine_hoop_math_leaders(years = year)
    }
    write_csv(leaderboard, paste0("data/", year_lab, "/leaderboard.csv"))
  }
}
# function to update team tables
update_team_tables <- function(years, new_games = F) {
  for (i in 1:length(years)) {
    # get year
    year <- years[i]
    year_lab <- paste0(as.numeric(year) - 1, "-", str_extract(year, "[012][0-9]$"))
    message(paste0("updating offensive tables for year: ", year_lab))
    for (j in 1:length(hoop_math_names$team)) {
      # get team
      team <- hoop_math_names$team_no_spaces[j]
      for (k in 1:length(types)) {
        # get the type
        type <- types[k]
        message(paste0("updating ", type, " tables for team: ", team))
        if (!dir.exists(paste0("data/", year_lab, "/", team))) {
          dir.create(paste0("data/", year_lab, "/", team))
        }
        table <- try(read_csv(paste0("data/", year_lab, "/", team, "/", type, ".csv")), silent = T)
        if (class(table)[1] == "try-error" | new_games == T) {
          table <- scrape_hoop_math_team_tables(team = team,
                                                type = type,
                                                years = year)
        } else {
          next
        }
        if (!is.null(table)) {
          write_csv(table, paste0("data/", year_lab, "/", team, "/", type, ".csv"))
        } else {
          next
        }
      }
    }
  }
}
# function to find most recent update to hoop-math.com for given year
hoop_math_recent_update <- function(year) {
  # this is date of most recent update to hoop-math.com
  last_update <- read_html(paste0("https://hoop-math.com/leader_o", year, ".php")) %>% 
    html_node("#bodytext+ p") %>% 
    html_text() %>% 
    str_sub(., -20) %>% 
    str_remove(., ".{2}:\\d+:\\d+ ") %>% 
    lubridate::mdy()
  return(last_update)
}

# update this as date of most recent scrape
date <- as.Date("2020-08-10")
# this is date of most recent update to hoop-math.com
last_update <- hoop_math_recent_update(year = 2020)

# if hoop-math.com has been updated more recently than the scrape, update
if (last_update > date) {
  # run this to update for 2021 and rewrite for new games
  update_leaderboard(years = 2021, new_games = T)
}
if (last_update > date) {
  # run this to update for 2021 and rewrite for new games
  update_team_tables(years = 2021, new_games = T)
}

update_team_tables(years = 2012:2020, new_games = T)

