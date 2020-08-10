# load libraries
library(tidyverse)
library(rvest)
library(janitor)

# function to scrape hoop-math.com team tables
scrape_hoop_math_team_tables <- function(team, years, type = c("OffTable", "RebTable", "TransOTable", "DefTable", "TransDTable")) {
  if (years[1] < 2012) {
    stop("hoop-math.com only has years from 2012 onward")
  }
  nodes <- paste0("#", type, "1 p")
  for (i in 1:length(years)) {
    # get year
    year <- years[i]
    message(paste0("scraping year: ", year))
    # navigate to page
    session <- try(read_html(paste0("https://hoop-math.com/", team, year, ".php")))
    #session <- try(read_html("https://hoop-math.com/Louisiana2013.php"))
    if (class(session)[1] == "try-error") {
      message(paste0("no data for: ", team, " during ", year))
      return(NULL)
    }
    # scrape table
    table_list <- session %>% 
      html_nodes(nodes) %>% 
      html_text() %>% 
      str_split(., "word")
    # reshape table
    df <- reshape_table(list_table = table_list, type = type, year = year) %>% 
      mutate(team = team)
    
    if (!exists("total_df")) {
      total_df <- df
    } else {
      total_df <- bind_rows(total_df, df)
    }
  }
  return(total_df)
}
# function to scrape hoop-math.com leaderboard tables
scrape_hoop_math_leader_tables <- function(years, type = c("OffDefLeader", "TransLeader"), off_def = c("Offense", "Defense")) {
  if (years[1] < 2012) {
    stop("hoop-math.com only has years from 2012 onward")
  }
  # navigate to correct page
  if (type == "TransLeader" & off_def == "Offense") {
    page <- "to"
  } else if (type == "TransLeader" & off_def == "Defense") {
    page <- "td"
  } else if (type == "OffDefLeader" & off_def == "Offense") {
    page <- "o"
  } else if (type == "OffDefLeader" & off_def == "Defense") {
    page <- "d"
  }
  for (i in 1:length(years)) {
    # get year
    year <- years[i]
    message(paste0("scraping year: ", year))
    # navigate to page
    session <- read_html(paste0("https://hoop-math.com/leader_", page, year, ".php"))
    # scrape table
    list_table <- session %>% 
      html_nodes(".sortable p") %>% 
      html_text() %>% 
      str_split(., "word")
    # reshape table
    df <- reshape_table(list_table = list_table, type = type, year = year) %>% 
      mutate(type = off_def)
    
    if (!exists("total_df")) {
      total_df <- df
    } else {
      total_df <- bind_rows(total_df, df)
    }
  }
  return(total_df)
}
# helper function to reshape lists of words into dataframes
reshape_table <- function(list_table, type, year) {
  extra <- F
  if (type == "OffTable") {
    variables <- 15L
    first_stat <- "fga"
    last_stat <- "ft_percent"
    extra = T
  } else if (type == "RebTable") {
    variables <- 8L
    first_stat <- "percent_of_total_at_rim_shots_coming_on_putbacks"
    last_stat <- "putback_fg_percent_2pt_jumpers"
    extra = T
  } else if (type %in% c("TransOTable", "TransDTable")) {
    variables <- 9L
    first_stat <- "percent_of_initial_attempts"
    last_stat <- "fg_percent_3pt"
  } else if (type == "DefTable") {
    variables <- 5L
    first_stat <- "percent_of_shots"
    last_stat <- "unblocked_fg_percent"
  } else if (type == "TransLeader") {
    variables <- 17L
    first_stat <- "percent_of_initial_fga_in_transition"
    last_stat <- "e_fg_percent_steal_11_30_s"
  } else if (type == "OffDefLeader") {
    variables <- 14L
    first_stat <- "e_fg_percent"
    last_stat <- "non_transtion_e_fg_percent"
  }
  
  df <- reshape2::melt(list_table) %>% 
    as_tibble() %>% 
    select(-L1) %>% 
    mutate(value = ifelse(value == "eFG% -- Score, 11-35", "eFG% -- Score, 11-35 s", value),
           value = ifelse(value == "eFG% -- Score, 11-30", "eFG% -- Score, 11-30 s", value),
           value = str_replace_all(value, "11-35 s", "11-30 s"),
           value = ifelse(value == "North Carolina St.", "NC State", value),
           group = (row_number() - 1) %/% variables) %>%
    group_by(group) %>% 
    mutate(row = row_number()) %>% 
    ungroup() %>% 
    arrange(row, group) %>%
    pivot_wider(names_from = "row",
                values_from = "value") %>% 
    select(-1) %>% 
    row_to_names(row_number = 1) %>% 
    clean_names() %>% 
    mutate(across(c(all_of(first_stat):all_of(last_stat)), ~ parse_number(., na = c("", "NA", "-", "---"))),
           across(c(all_of(first_stat):all_of(last_stat)), ~ ./100),
           season = paste0(as.numeric(year) - 1, "-", str_extract(year, "[012][0-9]$"))) 
  
  if (extra == T) {
    df <- df %>% 
      mutate(name = str_remove(name, " Jr.,")) %>% 
      separate(name, into = c("last", "first"), sep = ", ", fill = "left") %>% 
      mutate(name = ifelse(first == "Total", first, paste0(first, " ", last)),
             name = str_remove(name, " NA")) %>%
      select(name, everything(), -c(first, last))
  }
  if (type == "OffTable") {
    df <- df %>% 
      mutate(fga = 100 * fga,
             ts_percent = 100 * ts_percent)
  }
  if (type == "RebTable") {
    df <- df %>% 
      mutate(putbacks = as.numeric(putbacks)) %>% 
      filter(putbacks > 0)
  }
  return(df)
}
# function to combine hoop-math leaderboards for given years
combine_hoop_math_leaders <- function(years) {
  # scrape offensive leaderboard
  off <- scrape_hoop_math_leader_tables(years = years, 
                                        type = "OffDefLeader", 
                                        off_def = "Offense")
  # scrape defensive leaderboard
  def <- scrape_hoop_math_leader_tables(years = years, 
                                        type = "OffDefLeader", 
                                        off_def = "Defense")
  # scrape offensive transition leaderboard
  trans_o <- scrape_hoop_math_leader_tables(years = years, 
                                            type = "TransLeader", 
                                            off_def = "Offense") %>% 
    rename_with(cols = contains("_opp_score_"), ~ str_replace(., "_opp_score_", "_score_"))
  
  # scrape defensive transition leaderboard
  trans_d <- scrape_hoop_math_leader_tables(years = years, 
                                            type = "TransLeader", 
                                            off_def = "Defense")
  # combine leaderboards
  hoop_math <- off %>% 
    select(-type) %>% 
    left_join(def %>% select(-type), 
              by = c("team", "season"),
              suffix = c("_off", "_def")) %>% 
    select(team, season, everything()) %>% 
    left_join(trans_o %>% 
                select(-type) %>% 
                left_join(trans_d %>% select(-type), 
                          by = c("team", "season"),
                          suffix = c("_off", "_def")) %>% 
                select(team, season, everything()),
              by = c("team", "season")) %>% 
    select(-c(percent_of_total_fga_in_transition_off, percent_of_total_fga_in_transition_def,
              e_fg_percent_transition_off, e_fg_percent_transition_def,
              e_fg_percent_non_transition_off, e_fg_percent_non_transition_def))
  return(hoop_math)
}
