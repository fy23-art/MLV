library(rvolleydata)
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(ggplot2)
library(gt)

#Load data
pbp <- load_pbp(league = "mlv", seasons = 2026)
current_season <- max(pbp$season, na.rm = TRUE)
pbp <- pbp %>% filter(season == current_season)
players <- load_player_info(league = "mlv", seasons = 2026)
head(pbp)
colnames(pbp)
# Expected columns:match_id, season, match_datetime, home_team_name, away_team_name, team_involved, jersey_number, action, outcome, set, point_number, point_winner, home_score, away_score, league

#Touch level features
# Organize data chronologically within each match, set, and point
pbp <- pbp %>%
  arrange(match_id, set, point_number, row_number()) %>%
  group_by(match_id, set, point_number) %>%
  mutate(
    touch_index   = row_number(),          # 1, 2, 3, … within a point
    n_touches     = n(),                   # total touches in this point
    # Identify the serving team based on the first action of the point
    serving_team  = first(team_involved),
    # Categorize whether the current team acting is the one that served or is receiving
    touch_side    = if_else(team_involved == serving_team,
                            "serving", "receiving"),
    # Count consecutive touches by the same team (resets when team changes)
    team_run      = {
      sides <- touch_side
      run   <- integer(length(sides))
      k     <- 1L
      for (i in seq_along(sides)) {
        if (i == 1L || sides[i] != sides[i - 1L]) k <- 1L else k <- k + 1L
        run[i] <- k
      }
      run
    },
    # Calculate how many touches the team has left
    touches_remaining = pmax(0L, 3L - team_run + 1L),
    # Define a unique "State" string combining side, touch count, action, and outcome
    state = paste(touch_side, touches_remaining, action, outcome, sep = "|")
  ) %>%
  ungroup()

# Terminal states represent the end of a point (who actually won)
TERM_SERVE_WIN <- "TERMINAL|serving_wins"
TERM_RECV_WIN  <- "TERMINAL|receiving_wins"

# Create a dataframe of the final outcomes for every point in the dataset
terminals <- pbp %>%
  group_by(match_id, set, point_number) %>%
  summarise(
    serving_team = first(serving_team),
    point_winner = first(point_winner),
    touch_index  = max(touch_index) + 1L,
    .groups = "drop"
  ) %>%
  mutate(
    state = if_else(point_winner == serving_team,
                    TERM_SERVE_WIN, TERM_RECV_WIN)
  )

# Build second-order Markov transition counts
# Combine live play states with terminal outcomes to create a complete sequence
state_seq <- pbp %>%
  select(match_id, set, point_number, touch_index, state) %>%
  bind_rows(
    terminals %>%
      select(match_id, set, point_number, touch_index, state)
  ) %>%
  arrange(match_id, set, point_number, touch_index)

# Create a second-order Markov structure (s0 -> s1 -> s2)
transitions <- state_seq %>%
  group_by(match_id, set, point_number) %>%
  mutate(
    s0 = lag(state, 2),   # two steps ago
    s1 = lag(state, 1),   # one step ago
    s2 = state            # current
  ) %>%
  filter(!is.na(s0), !is.na(s1)) %>%
  ungroup() %>%
  select(s0, s1, s2)

# Count how many times each specific sequence of three states occurs
trans_counts <- transitions %>%
  count(s0, s1, s2, name = "n")

# Compute Transition Probabilities P(s2 | s0, s1)
trans_probs <- trans_counts %>%
  group_by(s0, s1) %>%
  mutate(
    total   = sum(n),
    n_next  = n_distinct(s2),
    prob    = (n + 1) / (total + n_next)   
  ) %>%
  ungroup()

# Compute sideout probability via value iteration until convergence
contexts <- trans_probs %>%
  distinct(s0, s1)

# Initialize sideout probability for every context (pair)
# Seed: terminal states get their known values; others start at 0.5
sideout_init <- 0.5

V <- contexts %>%
  mutate(sideout_prob = sideout_init)

# For terminal "contexts" override now
V <- V %>%
  mutate(
    sideout_prob = case_when(
      s1 == TERM_RECV_WIN  ~ 1,
      s1 == TERM_SERVE_WIN ~ 0,
      TRUE                 ~ sideout_prob
    )
  )

# Value iteration (Bellman updates)
max_iter  <- 200
tol       <- 1e-6

for (iter in seq_len(max_iter)) {
  V_old <- V
  V_new <- trans_probs %>%
    left_join(V %>% rename(s2_val = sideout_prob),
              by = c("s1" = "s0", "s2" = "s1")) %>%
    mutate(
      s2_val = case_when(
        s2 == TERM_RECV_WIN  ~ 1,
        s2 == TERM_SERVE_WIN ~ 0,
        is.na(s2_val)         ~ sideout_init,   # unseen context → prior
        TRUE                  ~ s2_val
      )
    ) %>%
    group_by(s0, s1) %>%
    summarise(
      sideout_prob = sum(prob * s2_val),
      .groups = "drop"
    )
  
  # Ensure terminals remain fixed during iteration
  V_new <- V_new %>%
    mutate(
      sideout_prob = case_when(
        s1 == TERM_RECV_WIN  ~ 1,
        s1 == TERM_SERVE_WIN ~ 0,
        TRUE                 ~ sideout_prob
      )
    )
  
  # Check convergence
  joined <- V_new %>%
    left_join(V_old %>% rename(old_prob = sideout_prob), by = c("s0", "s1"))
  delta <- max(abs(joined$sideout_prob - joined$old_prob), na.rm = TRUE)
  
  V <- V_new
  if (delta < tol) {
    message(sprintf("Value iteration converged after %d iterations (delta = %.2e)",
                    iter, delta))
    break
  }
}

# Attach sideout probabilities back to play-by-play
pbp_win <- pbp %>%
  # We need the (prev_state, curr_state) pair for each touch
  group_by(match_id, set, point_number) %>%
  mutate(
    prev_state = lag(state),
    curr_state = state
  ) %>%
  ungroup() %>%
  # For the first touch in a point there is no prev_state;
  # use a special "START" sentinel
  mutate(prev_state = if_else(is.na(prev_state), "START", prev_state)) %>%
  left_join(V %>% rename(prev_state = s0, curr_state = s1,
                         sideout_prob = sideout_prob),
            by = c("prev_state", "curr_state"))

# For the receiving team, win prob = sideout_prob
# For the serving  team, win prob = 1 - sideout_prob
pbp_win <- pbp_win %>%
  mutate(
    team_win_prob = if_else(touch_side == "receiving",
                            sideout_prob,
                            1 - sideout_prob)
  )

# Figure 1: Sideout probability vizualization
longest_point <- pbp_win %>%
  group_by(match_id, set, point_number) %>%
  summarise(n_touches = n(), .groups = "drop") %>%
  slice_max(n_touches, n = 1, with_ties = FALSE)

lp_data <- pbp_win %>%
  semi_join(longest_point, by = c("match_id", "set", "point_number")) %>%
  arrange(touch_index) %>%
  mutate(label = paste0(action, outcome))

fig1 <- ggplot(lp_data, aes(x = touch_index, y = sideout_prob)) +
  geom_hline(yintercept = 0.5, linetype = "dashed", colour = "grey60") +
  geom_line(colour = "#0072B2", linewidth = 1.1) +
  geom_point(aes(colour = touch_side), size = 3) +
  geom_text(aes(label = label), vjust = -0.9, size = 2.8) +
  scale_colour_manual(values = c(serving   = "#E69F00",
                                 receiving = "#0072B2"),
                      name = "Team") +
  scale_y_continuous(limits = c(0, 1),
                     labels = scales::percent_format(accuracy = 1)) +
  scale_x_continuous(breaks = seq_len(max(lp_data$touch_index))) +
  labs(
    title    = "Sideout Probability – Longest Point in Dataset",
    subtitle = sprintf("Match %s | Set %s | Point %s  (%d touches)",
                       longest_point$match_id,
                       longest_point$set,
                       longest_point$point_number,
                       longest_point$n_touches),
    x        = "Touch Number",
    y        = "Sideout (Receiving-Team Win) Probability",
    caption  = "Touch labels: action + outcome code"
  ) +
  theme_bw(base_size = 12) +
  theme(
    plot.title    = element_text(face = "bold"),
    legend.position = "bottom"
  )

#ggsave("C:/Users/x/Downloads/SMGT430/fig1_sideout_probability.pdf", plot = fig1, width = 11, height = 8.5, units = "in")


#Figure 2: Top 10 table
# Calculate the change in Point Win Probability
pbp_win <- pbp_win %>%
  group_by(match_id, set, point_number) %>%
  mutate(
    prev_sideout_prob = lag(sideout_prob),
    prev_sideout_prob = if_else(is.na(prev_sideout_prob),
                                if_else(point_winner == serving_team, 0, 1),
                                prev_sideout_prob),
    delta_wp = if_else(touch_side == "receiving",
                       sideout_prob - prev_sideout_prob,
                       prev_sideout_prob - sideout_prob),
  ) %>%
  ungroup()

# Join Player Names
players_dedup <- players %>%
  select(match_id, player_name, team_name, jersey_number) %>%
  distinct(match_id, team_name, jersey_number, .keep_all = TRUE)

pbp_players <- pbp_win %>%
  mutate(player_team_name = if_else(team_involved == "home", home_team_name, away_team_name)) %>%
  left_join(players_dedup,
            by = c("match_id", "player_team_name" = "team_name", "jersey_number"))

# Analyze Attacks
attacker_stats <- pbp_players %>%
  filter(action == "A") %>%
  group_by(player_name) %>%
  summarise(
    total_delta_wp = sum(delta_wp, na.rm = TRUE),
    attacks        = n(),
    kills          = sum(outcome == "#", na.rm = TRUE),
    errors         = sum(outcome %in% c("=", "/"), na.rm = TRUE),
    hitting_pct    = (kills - errors) / attacks,
    .groups = "drop"
  ) %>%
  filter(!is.na(player_name)) %>%
  slice_max(total_delta_wp, n = 10) %>%
  arrange(desc(total_delta_wp))

# Create and Save the Table
top_attackers_table <- attacker_stats %>%
  gt() %>%
  tab_header(
    title = "Top 10 MLV Attackers by Added Win Probability",
    subtitle = "2026 Season | Evaluated by Second-Order Markov Model"
  ) %>%
  cols_label(
    player_name    = "Player",
    total_delta_wp = "Total WP Added",
    attacks        = "Attacks",
    kills          = "Kills",
    errors         = "Errors",
    hitting_pct    = "Hitting %"
  ) %>%
  fmt_number(columns = total_delta_wp, decimals = 3) %>%
  fmt_percent(columns = hitting_pct, decimals = 3) %>%
  tab_options(table.width = pct(100))

print(top_attackers_table)
#gtsave(top_attackers_table, "C:/Users/x/Downloads/SMGT430/table_top_attackers.pdf")
