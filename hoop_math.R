source("hoop_math_scrape.R")

# exploring leaderboard data
off <- scrape_hoop_math_leader_tables(years = 2012:2020, 
                                      type = "OffDefLeader", 
                                      off_def = "Offense")
def <- scrape_hoop_math_leader_tables(years = 2012:2020, 
                                      type = "OffDefLeader", 
                                      off_def = "Defense")

trans_o <- scrape_hoop_math_leader_tables(years = 2012:2020, 
                                          type = "TransLeader", 
                                          off_def = "Offense") %>% 
  rename_with(cols = contains("_opp_score_"), ~ str_replace(., "_opp_score_", "_score_"))

trans_d <- scrape_hoop_math_leader_tables(years = 2012:2020, 
                                          type = "TransLeader", 
                                          off_def = "Defense") 

team_style <- c("percent_shots_at_rim_off", "percent_assisted_at_rim",
                "percent_assisted_2pt_jumpers",
                "percent_of_shots_3pt_off", "percent_assisted_3pt",
                "percent_shots_in_transition_off",
                "percent_shots_at_rim_def", "percent_blocked_at_rim", 
                "percent_blocked_2pt_jumpers", 
                "percent_of_shots_3pt_def", "percent_blocked_3pt",
                "percent_shots_in_transition_def",
                "percent_of_initial_fga_rebound_0_10_s_off",
                "percent_of_initial_fga_rebound_11_30_s_off",
                "percent_of_initial_fga_steal_0_10_s_off",
                "percent_of_initial_fga_steal_11_30_s_off",
                "percent_of_initial_fga_rebound_0_10_s_def",
                "percent_of_initial_fga_rebound_11_30_s_def",
                "percent_of_initial_fga_steal_0_10_s_def",
                "percent_of_initial_fga_steal_11_30_s_def")

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
%>% 
  select(team, season, all_of(team_style), percent_shots_2pt_j_off, percent_shots_2pt_j_def)

hoop_math %>% 
  select(team, season, all_of(team_style))

# Determine number of clusters
wss <- (nrow(hoop_math)-1)*sum(apply(hoop_math[-c(1:2)],2,var))
for (i in 2:15) wss[i] <- sum(kmeans(hoop_math[-c(1:2)],
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")

# K-Means Cluster Analysis
fit <- kmeans(hoop_math[-c(1:2)], 5) # 5 cluster solution
# get cluster means
aggregate(hoop_math[-c(1:2)],by=list(fit$cluster),FUN=mean) %>% view()
# append cluster assignment
mydata <- data.frame(hoop_math, fit$cluster)

# More complex
cluster::clusplot(hoop_math[-c(1:2)], fit$cluster, color=TRUE, shade=TRUE, 
                  labels=2, lines=0)

with(hoop_math, pairs(hoop_math[-c(1:8)], col=c(1:5)[fit$cluster])) 

# PCA
pca_rec <- recipe( ~ ., data = hoop_math) %>%
  update_role(team, season, new_role = "id") %>%
  step_zv(all_predictors()) %>% 
  step_normalize(all_predictors()) %>%
  step_pca(all_predictors())

pca_prep <- prep(pca_rec)

tidied_pca <- tidy(pca_prep, 3)

sdev <- pca_prep$steps[[3]]$res$sdev

percent_variation <- sdev^2 / sum(sdev^2)

pca_variation <- tibble(component = unique(tidied_pca$component),
                        percent_var = percent_variation,
                        cum_percent_var = cumsum(percent_variation)) %>%
  mutate(component = fct_inorder(component))

pca_variation %>%
  head(10) %>% 
  ggplot(aes(x = component, y = percent_var)) +
  geom_col() +
  geom_line(aes(x = component, y = cum_percent_var, group = 1))+
  scale_y_continuous(labels = scales::percent_format()) +
  theme_bw() +
  theme(plot.title = element_text(face = "bold", hjust = 0),
        plot.subtitle = element_text(face = "italic", hjust = 0),
        plot.caption = element_text(face = "italic")) +
  labs(title = "Explaining variance by principal components",
       subtitle = "Bars show variance explained by each PC | Line shows cumulative variance explained",
       x = NULL, 
       y = "Variance Explained")

tidied_pca %>%
  filter(component %in% paste0("PC", 1:4)) %>%
  mutate(component = fct_inorder(component)) %>%
  ggplot(aes(value, terms, fill = terms)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~component, nrow = 1) +
  theme_bw() +
  theme(plot.title = element_text(face = "bold", hjust = 0),
        plot.subtitle = element_text(face = "italic", hjust = 0),
        plot.caption = element_text(face = "italic")) +
  labs(y = NULL)

tidied_pca %>%
  filter(component %in% paste0("PC", 1:4)) %>%
  group_by(component) %>%
  top_n(10, abs(value)) %>%
  ungroup() %>%
  mutate(terms = str_replace_all(terms, "_", " "),
         terms = str_to_title(terms),
         terms = str_replace_all(terms, "Percent", "%"),
         terms = reorder_within(terms, value, component)) %>%
  ggplot(aes(value, terms, fill = value > 0)) +
  geom_col() +
  scale_y_reordered() +
  scale_fill_manual(values = c("#93d3ab", "#35b0ab"),
                    guide = guide_legend(reverse = T)) +
  facet_wrap(~component, scales = "free_y") +
  theme_bw() +
  theme(plot.title = element_text(face = "bold", hjust = 0),
        plot.subtitle = element_text(face = "italic", hjust = 0),
        plot.caption = element_text(face = "italic")) +
  labs(title = "What contributes to each principal component?",
       subtitle = "PCA of NCAA team styles",
       x = "Absolute Value of Contribution",
       y = NULL, 
       fill = "Positive?")

# pull ACC team names
ACC_teams <- ncaahoopR::ncaa_colors %>% 
  filter(conference == "ACC") %>% 
  mutate(espn_name = case_when(
    espn_name == "UNC" ~ "North Carolina",
    espn_name == "UVA" ~ "Virginia",
    espn_name == "Pitt" ~ "Pittsburgh",
    espn_name == "Florida State" ~ "Florida St.",
    espn_name == "Miami" ~ "Miami (FL)",
    TRUE ~ espn_name)) %>% 
  select(espn_name) %>% 
  pull()

# function to find ACC win totals
get_acc_standings <- function(years) {
  if (years[1] < 2012) {
    stop("hoop-math.com only has years from 2012 onward")
  }
  for (i in 1:length(years)) {
    # get year
    year <- years[i]
    # navigate to page
    session <- read_html(paste0("https://www.sports-reference.com/cbb/conferences/acc/", year, ".html"))
    # scrape table
    df <- session %>% 
      html_node("#standings") %>% 
      html_table() %>% 
      row_to_names(row_number = 1) %>%
      clean_names() %>% 
      as_tibble() %>% 
      transmute(team = school, 
                wins = as.numeric(w), 
                losses = as.numeric(l), 
                pct_win = as.numeric(w_l_percent),
                season = paste0(as.numeric(year) - 1, "-", str_extract(year, "[012][0-9]$")))
    
    if (!exists("total_df")) {
      total_df <- df
    } else {
      total_df <- bind_rows(total_df, df)
    }
  }
  return(total_df)
}

ACC_wins <- get_acc_standings(years = 2012:2020) %>% 
  mutate(team = case_when(
    team == "Florida State" ~ "Florida St.",
    team == "North Carolina State" ~ "NC State",
    TRUE ~ team))

juice(pca_prep) %>%
  filter(team %in% ACC_teams) %>%
  left_join(as_tibble(mydata) %>% 
              transmute(team, season, cluster = factor(fit.cluster)),
            by = c("team", "season")) %>% 
  mutate(season = str_remove(season, "20")) %>% 
  ggplot(aes(PC1, PC2, color = cluster)) +
  geom_point() +
  ggrepel::geom_text_repel(aes(label = season), size = 2.5) +
  facet_wrap(~ team) +
  theme_bw() +
  theme(plot.title = element_text(face = "bold", hjust = 0),
        plot.subtitle = element_text(face = "italic", hjust = 0),
        plot.caption = element_text(face = "italic")) +
  labs(color = "Cluster")

juice(pca_prep) %>%
  filter(team %in% ACC_teams) %>%
  left_join(as_tibble(mydata) %>% 
              transmute(team, season, cluster = factor(fit.cluster)),
            by = c("team", "season")) %>% 
  left_join(ACC_wins, by = c("team", "season")) %>% 
  mutate(season = str_remove(season, "20")) %>% 
  ggplot(aes(PC1, PC2, color = pct_win)) +
  geom_point() +
  ggrepel::geom_text_repel(aes(label = paste0(team, ": ", season)),
                           size = 2.5) +
  scale_color_viridis_c() +
  #scale_color_stepsn(colors = scales::viridis_pal(begin = .1, end = .9)(10)) +
  facet_wrap(~ cluster) +
  theme_bw() +
  theme(plot.title = element_text(face = "bold", hjust = 0),
        plot.subtitle = element_text(face = "italic", hjust = 0),
        plot.caption = element_text(face = "italic")) +
  labs(color = "Cluster")

juice(pca_prep) %>%
  filter(team %in% ACC_teams) %>%
  left_join(as_tibble(mydata) %>% 
              transmute(team, season, cluster = factor(fit.cluster)),
            by = c("team", "season")) %>% 
  left_join(ACC_wins, by = c("team", "season")) %>% 
  group_by(cluster) %>% 
  summarise(wins = mean(wins, na.rm = T),
            pct_win = mean(pct_win, na.rm = T),
            teams = n())

hoop_math %>% 
  filter(season == "2019-20") %>%
  mutate(across(c(percent_shots_at_rim_off, percent_of_shots_3pt_off,
                  percent_shots_2pt_j_off, percent_shots_in_transition_off), 
                ~ paste0(., ": ", percent_rank(.))),
         across(c(percent_shots_at_rim_def, percent_of_shots_3pt_def,
                  percent_shots_2pt_j_def, percent_shots_in_transition_def), 
                ~ paste0(., ": ", percent_rank(.)))) %>% 
  pivot_longer(cols = c(percent_shots_at_rim_off, percent_of_shots_3pt_off,
                        percent_shots_2pt_j_off, percent_shots_in_transition_off,
                        percent_shots_at_rim_def, percent_of_shots_3pt_def,
                        percent_shots_2pt_j_def, percent_shots_in_transition_def)) %>%
  separate(value, into = c("value", "rank"), sep = ": ", convert = T) %>%
  mutate(type = ifelse(str_detect(name, "_off"), "Offense", "Defense"),
         name = str_remove(str_replace_all(name, "_", " "), " off| def"),
         name = str_remove(str_to_title(str_replace(name, "percent", "%")), " Of| In"),
         name = fct_relevel(name, "% Shots Transition", "% Shots At Rim", "% Shots 3pt"),
         rank = case_when(
           round(100*rank) %% 10 == 1 ~ paste0(format(100*round(rank, 2), nsmall = 0), "st%"),
           round(100*rank) %% 10 == 2 ~ paste0(format(100*round(rank, 2), nsmall = 0), "nd%"),
           round(100*rank) %% 10 == 3 ~ paste0(format(100*round(rank, 2), nsmall = 0), "rd%"),
           TRUE ~ paste0(format(100*round(rank, 2), nsmall = 0), "th%")),
         name = fct_rev(name),
         type = fct_rev(type),
         team = fct_relevel(team, "Duke", "NC State")) %>%
  arrange(desc(team)) %>% 
  ggplot(aes(value, name)) +
  geom_jitter(aes(color = ifelse(team %in% c("Duke", "NC State"), team, ""), 
                  alpha = ifelse(team %in% c("Duke", "NC State"), team, ""),
                  size = ifelse(team %in% c("Duke", "NC State"), team, "")), height = 0.05) +
  ggrepel::geom_text_repel(aes(label = ifelse(team %in% c("Duke", "NC State"), 
                                              paste0(team, ": ", rank), 
                                              "")),
                           fontface = "bold", size = 3) +
  facet_wrap(~ type) +
  scale_x_continuous(labels = scales::percent_format()) +
  scale_color_manual(values = c("black", "blue", "red")) +
  scale_alpha_manual(values = c(0.2, 1, 1)) +
  scale_size_manual(values = c(1, 3, 3)) +
  guides(color = F, alpha = F, size = F) +
  labs(title = "Stylistic Breakdown",
       subtitle = "Duke vs. NC State | 2019-20 Season",
       x = NULL,
       y = NULL)

library(widyr)

hoop_math %>% 
  unite("team", c(team, season), sep = ": ") %>% 
  pivot_longer(cols = c(everything(), -team)) %>% 
  pairwise_cor(name, team, value) %>% view()

# Looking at year to year correlations between stats
by_season_stats <- hoop_math %>% 
  pivot_longer(cols = c(e_fg_percent_off:e_fg_percent_steal_11_30_s_def),
               names_to = "stat") %>% 
  group_by(team, stat) %>% 
  arrange(team, season) %>%
  mutate(prior_value = lag(value, default = NA)) %>% 
  ungroup() %>% 
  filter(!is.na(prior_value)) %>% 
  mutate(type = ifelse(str_detect(stat, "_off|_assisted"), "Offense", "Defense"),
         stat = str_remove(str_replace(str_replace_all(stat, "_", " "), "percent", "%"), " off| def"),
         stat = str_to_title(str_replace(stat, "jumpers", "j")),
         stat = str_remove_all(stat, "Of |At |In "),
         stat = str_replace(stat, "Fga", "FGA"),
         stat = str_replace(stat, "E Fg %", "eFG%"),
         stat = str_replace(stat, "Fg %", "FG%"),
         stat = str_replace(stat, "X3fg %", "3FG%"),
         stat = str_replace(stat, "0 10 S", "0-10s"),
         stat = str_replace(stat, "11 30 S", "11-30s"),
         stat = case_when(
           stat == "% Initial FGA Score 0-10s" ~ "% Initial FGA After Score 0-10s",
           stat == "% Initial FGA Score 11-30s" ~ "% Initial FGA After Score 11-30s",
           stat == "% Initial FGA Rebound 0-10s" ~ "% Initial FGA After REB 0-10s",
           stat == "% Initial FGA Rebound 11-30s" ~ "% Initial FGA After REB 11-30s",
           stat == "% Initial FGA Steal 0-10s" ~ "% Initial FGA After Steal 0-10s",
           stat == "% Initial FGA Steal 11-30s" ~ "% Initial FGA After Steal 11-30s",
           stat == "eFG% Score 0-10s" ~ "eFG% After Score 0-10s",
           stat == "eFG% Score 11-30s" ~ "eFG% After Score 11-30s",
           stat == "eFG% Rebound 0-10s" ~ "eFG% After REB 0-10s",
           stat == "eFG% Rebound 11-30s" ~ "eFG% After REB 11-30s",
           stat == "eFG% Steal 0-10s" ~ "eFG% After Steal 0-10s",
           stat == "eFG% Steal 11-30s" ~ "eFG% After Steal 11-30s",
           TRUE ~ stat),
         type = fct_relevel(type, "Offense", "Defense"))

year_to_year_corrs <- by_season_stats %>% 
  group_by(type, stat) %>% 
  nest(data = c(team, season, value, prior_value)) %>% 
  mutate(model = map(data, ~ lm(value ~ prior_value, .)),
         glanced = map(model, glance)) %>% 
  unnest(glanced) %>% 
  ungroup() %>% 
  arrange(desc(r.squared)) %>% 
  mutate(stat = reorder_within(stat, r.squared, type)) %>% 
  select(type, stat, r.squared) 

year_to_year_corrs %>% 
  ggplot(aes(r.squared, stat)) +
  geom_col(aes(fill = type, color = type), show.legend = F, alpha = 0.55) +
  geom_text(aes(label = format(round(r.squared, 2), nsmall = 2)), 
            size = 3.5, fontface = "bold", hjust = -0.25) +
  facet_wrap(~ type, scales = "free_y") +
  scale_y_reordered() +
  scale_x_continuous(expand = expansion(mult = c(0, 0.15))) +
  scale_color_manual(values = c("#235F9C", "#001A57")) +
  scale_fill_manual(values = c("#235F9C", "#001A57")) +
  labs(title = "How stable are NCAA basketball metrics year to year?",
       subtitle = "2011-12 through 2019-20 seasons",
       x = "Year-to-Year R-Squared",
       y = NULL,
       caption = "Data from hoop-math.com")

# scatter plot of year to year correlations
by_season_stats %>% 
  filter(type == "Offense") %>% 
  ggplot(aes(prior_value, value)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~ stat, scales = "free") +
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Value in Year N",
       y = "Value in Year N+1")
 
# exploring team datasets from hoop-math.com
off_tables <- scrape_hoop_math_team_tables(team = "Duke", 
                                           type = "OffTable", 
                                           years = 2012:2020)

off_tables %>% 
  filter(name == "Total") %>% view()

off_trans_tables <- scrape_hoop_math_team_tables(team = "Duke", 
                                                 type = "TransOTable", 
                                                 years = 2012:2020)

off_trans_tables %>% 
  separate(start_of_poss, into = c("start", "shot_clock"), sep = ", ", fill = "right") %>% 
  filter(!is.na(shot_clock)) %>% 
  ggplot(aes(season, e_fg_percent, color = shot_clock, group = shot_clock)) +
  geom_line() +
  geom_point() +
  expand_limits(y = 0) +
  facet_wrap(~ start) +
  scale_y_continuous(labels = scales::percent_format()) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = NULL,
       y = "eFG%")


# offensive rebounding
get_off_reb_conference <- function(teams, years = 2012:2020) {
  for (i in 1:length(teams)) {
    team <- teams[i]
    message("scraping team ", team)
    if (team == "NC State") {
      years <- 2018:2020
    } else {
      years <- 2012:2020
    }
    
    df <- scrape_hoop_math_team_tables(team = str_remove_all(team, " "),
                                       type = "RebTable", 
                                       years = years) %>% 
      mutate(team = team)
    if (!exists("total_df")) {
      total_df <- df
    } else {
      total_df <- bind_rows(total_df, df)
    }
  }
  return(total_df)
}
ACC_off_reb <- get_off_reb_conference(teams = ACC_teams) %>% 
  left_join(ncaahoopR::ncaa_colors %>% 
              filter(conference == "ACC") %>% 
              mutate(espn_name = case_when(
                espn_name == "UNC" ~ "North Carolina",
                espn_name == "UVA" ~ "Virginia",
                espn_name == "Pitt" ~ "Pittsburgh",
                espn_name == "Florida State" ~ "Florida St.",
                espn_name == "Miami" ~ "Miami (FL)",
                TRUE ~ espn_name)) %>% 
              select(espn_name, logo_url, primary_color, secondary_color),
            by = c("team" = "espn_name")) 

fill <- ACC_off_reb %>% 
  distinct(team, .keep_all = T) %>% 
  arrange(team) %>%
  select(primary_color) %>% 
  pull()

color <- ACC_off_reb %>% 
  distinct(team, .keep_all = T) %>% 
  arrange(team) %>% 
  select(secondary_color) %>% 
  pull()

ACC_off_reb %>% 
  filter(name != "Total",
         putbacks >= 15) %>% 
  group_by(team) %>% 
  top_n(10, putback_fg_percent_at_rim) %>% 
  mutate(name = paste0(name, " (", str_remove(season, "20"),")"),
         name = fct_reorder(name, putback_fg_percent_at_rim)) %>% 
  ggplot(aes(putback_fg_percent_at_rim, name, fill = team, color = team)) +
  geom_col() +
  geom_text(aes(label = name),
            hjust = 1, size = 3, color = "white", fontface = "bold") +
  ggimage::geom_image(aes(x = 1, y = 1.5, image = logo_url),
                      size = 0.12, asp = 1.5, inherit.aes = F) +
  scale_x_continuous(labels = scales::percent, expand = expansion(c(0, 0.1))) +
  scale_fill_manual(values = fill) +
  scale_color_manual(values = color) +
  facet_wrap(~ team, scales = "free_y", nrow = 3) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none") +
  labs(title = "Top 10 most proficient offensive rebounding seasons in the ACC by team",
       subtitle = "2011-12 through 2019-20 seasons | Minimum 15 putback attempts",
       x = "Putback FG%",
       y = NULL,
       size = "Putback FGA",
       caption = "Data from hoop-math.com")
  



