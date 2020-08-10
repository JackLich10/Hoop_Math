source("hoop_math_scrape.R")
# leaderboard <- scrape_hoop_math_leader_tables(years = 2020, type = "OffDefLeader", off_def = "Offense")
# hoop_math_names <- tibble(team = leaderboard$team,
#                           team_no_spaces = str_remove_all(leaderboard$team, " "))
# 
# write_csv(hoop_math_names, "hoop_math_names.csv")

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

update_leaderboard(years = 2021, new_games = T)

hoop_math_names <- read_csv("hoop_math_names.csv")
types <- c("OffTable", "RebTable", "TransOTable", "DefTable", "TransDTable")
years <- 2012:2020
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

# exploring team datasets from hoop-math.com
off_tables <- scrape_hoop_math_team_tables(team = "Duke", 
                                           type = "OffTable", 
                                           years = 2012)
scrape_hoop_math_team_tables()
off_tables %>% 
  filter(name == "Total") %>% view()

off_trans_tables <- scrape_hoop_math_team_tables(team = "Duke", 
                                                 type = "TransOTable", 
                                                 years = 2012:2020)

n <- nrow(ids)
### Pull Games
date <- as.Date("2020-03-11")
while(date <= Sys.Date()) {
  schedule <- get_master_schedule(date)
  if(!is.null(schedule)) {
    if(!dir.exists(paste("2019-20/pbp_logs", date, sep = "/"))) {
      dir.create(paste("2019-20/pbp_logs", date, sep = "/")) 
    }
    write_csv(schedule, paste("2019-20/pbp_logs", date, "schedule.csv", sep = "/"))
    
    n <- nrow(schedule)
    for(i in 1:n) {
      print(paste("Getting Game", i, "of", n, "on", date))
      x <- try(get_pbp_game(schedule$game_id[i]))
      if(!is.null(x) & class(x) != "try-error") {
        write_csv(x, paste("2019-20/pbp_logs", date, paste0(schedule$game_id[i], ".csv"), sep = "/"))
      }
    }
  }
  date <- date + 1
}

### Update Master Schedule
date <- as.Date("2019-11-05")
master_schedule <- NULL
while(date <= Sys.Date()) {
  schedule <- try(read_csv(paste("2019-20/pbp_logs", date, "schedule.csv", sep = "/")) %>%
                    mutate("date" = date))
  if(class(schedule)[1] != "try-error") {
    write_csv(schedule, paste("2019-20/pbp_logs", date, "schedule.csv", sep = "/"))
    master_schedule <- bind_rows(master_schedule, schedule)
  }
  
  date <- date + 1
}
write_csv(master_schedule, "2019-20/pbp_logs/master_schedule.csv")


### Box Scores
schedules <- dir(paste("2019-20/schedules", sep = "/"), full.names = T)
schedules_clean <- dir(paste("2019-20/schedules", sep = "/"), full.names = F)
n <- length(schedules)
for(i in 1:n) {
  ### Read in Schedule
  s <- read_csv(schedules[i])
  n1 <- nrow(s)
  box### Try to Scrape PBP
  for(k in 1:n1) {
    cat("Scraping Game", k, "of", n1, "for Team", i, "of", n, "\n")
    team <- gsub("_", " ", gsub("_schedule.csv", "", schedules_clean[i]))
    file <- paste("2019-20/box_scores", gsub(" ", "_", team), paste0(s$game_id[k], ".csv"), sep = "/")
    if(!file.exists(file)) {
      box <- try(get_boxscore(s$game_id[k]))
      box_team <- ifelse(team == "UConn", team, dict$ESPN_PBP[dict$ESPN == team])
      box[box_team]
      
      if(class(box) != "try-error" & box_team %in% names(box) & !is.na(box_team)) {
        ### Create Date Directory if Doesn't Exist
        if(!dir.exists(paste("2019-20/box_scores", sep = "/"))) {
          dir.create(paste("2019-20/box_scores", sep = "/")) 
        }
        if(!dir.exists(paste("2019-20/box_scores", gsub(" ", "_", team), sep = "/"))) {
          dir.create(paste("2019-20/box_scores", gsub(" ", "_", team), sep = "/"))
        }
        df <- as.data.frame(box[[box_team]])
        df$date <- s$date[k]
        df$opponent <- s$opponent[k]
        df$location <- s$location[k]
        write_csv(df, file)
      }
    }
  }
}