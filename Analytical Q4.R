# Load necessary libraries
library(dplyr)
library(ggplot2)
library(stringr)
library(tidyr)
library(tidyverse)
library(scales) # For percentage formatting in plots
library(forcats) # For reordering factors in plots
library(segmented)

dataset <- read.csv("cleaned_chess_data.csv")

df <- dataset

str(df)

### Exploratory Analysis
#### Q1. Is there a relationship between the ratings of black and white players in a match? 
ggplot(df, aes(white_rating, black_rating)) +
  geom_point(alpha=0.3) +
  geom_smooth(method = "loess", span=0.5, se = FALSE)


lm_model <- lm(black_rating ~ white_rating, data = df)
segmented_model <- segmented(lm_model, seg.Z = ~white_rating, psi = 2200)
summary(segmented_model)  # Check breakpoint(s)

## Since we get an oval shape, we assume we have linear relationship between white rating and black rating, which means professionals play with professional most of the time and amatuers play with amatuers most the time, howvere because we have drawn a doess line line we can see clearly the players above around 2100 rating from either side tend to play more with intermediate players (1500-2000) in overall games that we see a decrease or non linearity in the last part of the line.

#1. Oval Shape & Linear Core Relationship

#✅ Your observation is correct:

#  The oval cloud in the scatter plot suggests a positive linear trend between white_rating and black_rating.
#This reflects skill-based matching: Professionals (e.g., 2500+ ratings) tend to play against other professionals, while amateurs (e.g., <1500) play against similarly rated opponents.
#2. Nonlinearity at Higher Ratings (2200+)

#✅ Key Insight from loess:

#  The curve bending downward at higher ratings (>2200) indicates:
#  Top players (e.g., GMs, IMs) often play against intermediate players (1500-2000).
#Possible reasons:
#  Tournament structures: Strong players face weaker opponents in early rounds.
#Rating inflation: Fewer elite players available, forcing mismatches.
#Training/practice games: Pros may play lower-rated players for coaching or experimentation.

# Q2. Is there an effect of rated games on the victory status of matches played?

ggplot(df, aes(x=rated)) +
  geom_bar()

ggplot(df, aes(x=victory_status)) +
  geom_bar() + 
  facet_wrap(~ rated)

ggplot(df, aes(x = victory_status)) +
  geom_bar(aes(y = ..prop.., group = rated)) +
  geom_text(
    aes(y = ..prop.., label = scales::percent(..prop..)),
    stat = "count", vjust = -0.5
  ) +
  facet_wrap(~rated)

# as shown in the below faceted bar chart diagram faceted by either rated games or not, no real difference between these two cases as they're relatively similar since its unbalanced in this regard, the rated games are more in quantity than unrated games, the ratio between draw, mate, resign and out-of-time statuses are similar in these two bar charts.

#✅ Your interpretation is correct:

#  The ratios of victory statuses (draw, mate, resign, outoftime) are similar between rated and unrated games.
#The counts differ (rated games dominate in volume), but the proportions of outcomes are consistent.
#✅ Evidence of Similarity:

#  The relative heights of bars (e.g., mate vs. resign) are nearly identical in both facets.
#No drastic shifts in outcome preferences (e.g., rated games don’t favor mate over resign disproportionately).

#. Why This Matters

#Practical Implication: Chess outcome dynamics (how games end) are independent of rating enforcement.
#Players resign or get checkmated at similar rates, whether rated or not.
#Data Bias Alert: The imbalance (more rated games) suggests:
#  Unrated games might be underrepresented (e.g., casual play vs. tournaments).
#But the similar proportions imply no systemic bias in outcomes.

df %>%
  group_by(rated, victory_status) %>%
  summarize(count = n()) %>%
  mutate(prop = count / sum(count))

chisq.test(table(df$rated, df$victory_status))

# Q3. What is the frequency of turns in chess games in respect to victory status?

ggplot(df, aes(turns)) +
  geom_histogram(bins=30) + 
  theme_bw()

ggplot(df, aes(turns)) +
  geom_boxplot() + 
  facet_wrap(~ victory_status)

summary(df$turns)

#### Notably, we infer from the box plots for each victory status and the histogram for all matches number of turns that number of turns in both diagrams is skewed to the right with median around 55 turns with extreme values in some cases with maximum value 300 turns

# 1. Key Observations from the Diagrams

# A. Box Plots (Victory Status vs. Turns)

# Right-Skewed Distributions: All victory statuses (draw, mate, resign, outoftime) show longer tails to the right, indicating most games end before ~100 turns, but some extend much further.
#Median (~55 turns): Consistent across all victory types, suggesting the "typical" game length is independent of how it ends.
#Outliers: Extreme values (up to 300 turns) occur in all categories but are most visible in draw and mate.
#B. Histogram (Overall Turns Distribution)

#Peak Near Lower Turns: The highest frequency occurs at shorter game lengths (<100 turns).
#Long Tail: Few games reach 200+ turns, aligning with the boxplot outliers.

#✅ Your inference is correct:

#  Turn counts are right-skewed with a median of ~55.
#draw has the widest spread, while resign/outoftime are more concentrated at lower turns.
#Extreme values (300 turns) are rare but real.


# Q4. Is there a relationship between number of turns and rating of a match's chess players?
# Create bins and summarize for SKILL DISPARITY (rating difference)
## Normality check
ggplot(df, aes(abs(white_rating - black_rating), turns)) +
  geom_point(alpha=0.3) +
  geom_smooth(method = "loess", span=0.5, se = FALSE)

cor(abs(df$white_rating - df$black_rating), df$turns, method = "spearman")

heatmap_data <- df %>%
  mutate(
    skill_disparity_bin = cut(abs(white_rating - black_rating), 
                              breaks = 10,
                              labels = c("0-100", "101-200", "201-300", "301-400", 
                                         "401-500", "501-600", "601-700", "701-800", 
                                         "801-900", "900+")),
    turns_bin = cut(turns, 
                    breaks = 10,
                    labels = paste(seq(0, 90, 10), seq(10, 100, 10), sep = "-"))) %>%  # Fixed: Added sep="-" and closed parenthesis
  group_by(skill_disparity_bin, turns_bin) %>%
  summarise(
    correlation = cor(abs(white_rating - black_rating), turns, method = "spearman", use = "complete.obs"),
    count = n()
  ) %>%
  ungroup()

# Plot (unchanged)
ggplot(heatmap_data, aes(x = skill_disparity_bin, y = turns_bin, fill = correlation)) +
  geom_tile(color = "white", linewidth = 0.2) +
  scale_fill_gradient2(
    low = "blue", mid = "white", high = "red",
    midpoint = 0, 
    limits = c(-1, 1),
    name = "Correlation\nStrength"
  ) +
  labs(
    title = "Game Length vs. Skill Disparity",
    subtitle = "How rating differences correlate with number of turns",
    x = "Absolute Rating Difference (|White - Black| Elo)",
    y = "Number of Turns",
    caption = "Negative correlation (blue) = Larger skill gaps lead to shorter games"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
    axis.text.y = element_text(size = 9),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, color = "gray40"),
    legend.position = "right"
  ) +
  geom_text(aes(label = ifelse(count > 30, round(correlation, 2), "")), 
            color = "black", size = 3)

## as the correlation heatmap diagram illustrates that the skill gap between players has really negligible to zero effect on the number of turns played, with overall correlation of -0.1265309 as we can see from the LOESS line in scatter plot, however when we calculated the correlation matrix for each slice of data, we can infer that there's a strong negative correlation between shorter games and skill gap especially very early in the game as illustrated in blue line with huge rating difference despite the overall small negative correlation

#    1. Key Findings

#    Overall Weak Correlation (-0.127)
#    The global Pearson/Spearman correlation suggests almost no linear relationship between skill gap and game length.
#    Interpretation: On aggregate, skill disparity explains very little variance in turn count.
#    Stratified Analysis (Sliced Data)
#    Short games (0–20 turns): Strong negative correlation (blue in heatmap).
#    Meaning: Large skill gaps lead to early resignations/blunders, ending games quickly.
#    Longer games (50+ turns): Correlation ≈ 0 (white/red in heatmap).
#    Meaning: Skill disparity has no effect on game length in endgames.
#    LOESS Curve in Scatter Plot
#    The curve’s downward tilt at low turn counts aligns with your heatmap’s blue zones, confirming that skill gaps matter most early in games.
#    2. Why the Discrepancy? Global vs. Stratified Correlation

#    Global Correlation (-0.127):
#      Averages all games, masking subset dynamics (e.g., short vs. long games).
#    Dominated by mid-length games where skill gaps have little impact.
#    Stratified Correlation:
#      Exposes phase-dependent effects:
#      Opening/Middlegame: Skill gaps accelerate wins/resignations.
#    Endgame: Skill differences are neutralized (both players play accurately).

##total number of openings

# Analytical Questions
#  Q2:What are the five most common opening moves by colour?
#      Identify the most frequently used opening moves and analyse differences between white and black.

df_agg <- df %>%
  mutate(move_list = str_split(moves, " "), critical_moves = sapply(move_list, function(x) paste(x[1:min(opening_ply, length(x))], collapse = " ")))

black_driven_openings <- list(
  # ECO codes where Black dictates the opening
  eco_codes = c("B00-B99", "A40-A99", "E00-E99", "C00-C19", "C41"),  # Added C41 for Philidor
  # Move sequences where Black's response defines the opening
  move_patterns = c(
    "^e4 c5",       # Sicilian
    "^e4 e6",       # French
    "^e4 c6",       # Caro-Kann
    "^e4 d6",       # Philidor (explicitly added)
    "^d4 Nf6",      # Indian Defenses
    "^d4 f5",       # Dutch
    "^e4 d5",       # Scandinavian
    "^d4 g6"        # Modern/KID
  )
)

classify_opening <- function(opening_eco, critical_moves) {
  # Check against ECO codes (including explicit C41)
  eco_first_char <- substr(opening_eco, 1, 1)
  eco_num <- as.numeric(substr(opening_eco, 2, 3))
  
  # Check if ECO is in Black-driven ranges or is C41
  if (eco_first_char %in% c("B", "E") || 
      (eco_first_char == "A" && eco_num >= 40) || 
      (eco_first_char == "C" && (eco_num < 20 || opening_eco == "C41"))) {  # Modified for C41
    return("Black")
  }
  
  # Check move patterns (Black-driven, now includes Philidor's ^e4 d6)
  if (any(str_detect(critical_moves, black_driven_openings$move_patterns))) {
    return("Black")
  }
  
  # Default to White-driven
  return("White")
}


# Apply classification
df_agg <- df_agg %>%
  mutate(
    strategy_color = sapply(1:nrow(df), function(i) {
      classify_opening(opening_eco[i], critical_moves[i])
    })
  )

# Example: Queen's Gambit Declined (D30-D69) is White-driven, but Black can influence structure
df_agg <- df_agg %>%
  mutate(
    strategy_color = case_when(
      # Override ECO-based classification for specific cases
      str_detect(opening_name, "Queen's Gambit Declined") ~ "White",
      str_detect(opening_name, "King's Indian") ~ "Black",
      TRUE ~ strategy_color
    )
  )

df_agg <- df_agg %>%
  group_by(strategy_color, opening_eco, opening_name) %>%
  summarise(count = n(), .groups = "drop")

ggplot(df_agg, aes(x = strategy_color, y = count, fill = opening_eco)) +
  geom_col() +
  geom_text(
    aes(label = opening_name),
    position = position_stack(vjust = 0.5),
    size = 3,
    color = "white"
  ) +
  labs(
    title = "Chess Openings by Strategy Color (Definitive Classification)",
    x = "Strategy Dictated By",
    y = "Frequency",
    fill = "ECO Code"
  ) +
  theme_minimal()

# Step 1: Get top 5 openings per color
df_top5 <- df_agg %>%
  group_by(strategy_color) %>%
  arrange(desc(count), .by_group = TRUE) %>%  # Sort by frequency within each color
  slice_head(n = 7) %>%                      # Keep top 5 per group
  ungroup()

# Step 2: Plot with contrast colors and small labels
ggplot(df_top5, aes(x = strategy_color, y = count, fill = opening_eco)) +
  geom_col(width = 0.85) +  # Adjust bar width for better label spacing
  geom_text(
    aes(label = sprintf("%s\n(%d)", str_trunc(opening_name, width = 40), count)),  # Truncate long names + newline
    position = position_stack(vjust = 0.5),
    size = 2.5,           # Smaller font size
    color = "black",      # High-contrast text
    lineheight = 0.8      # Tighter line spacing
  ) +
  labs(
    title = "Top 5 Chess Openings by Strategy Color",
    x = "Strategy Dictated By",
    y = "Frequency",
    fill = "ECO Code"
  ) +
  theme_minimal() +
  scale_fill_manual(
    values = c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd",  # High-contrast palette
               "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22", "#17becf")
  ) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "bottom",
    axis.text.x = element_text(size = 10)
  )


# overall Scandinavian Defense is the most common stratgey used in the dataset and by the black players, the generic one and Mises-Krotoc vairety in total are 464 times used, howver from white players side the Queen' Pawn Game is the most common with total count of 361, there's a pattern of different vaireties of strategy used is common across the data we have, for example Philidor Defense is the most commonly used by black players with 362, however that number is nearly half divided between varitey #2 and #3, so we took that into account while showing the Top 5 by the most common opening ECO code, as we can see from the legends that we group fill stacks of the same ECO code with same colour, regardless if their label but we specify them on the stack with each one's count for understandability,
# Secondly there's a a quite difference between the strategies used by black and white players, strategies used by white are commonly attack stratgies or pawn openings since the white is the first player to start, he has the advantage of shaping the opening of the game, however the black players usually set counter-attack or defense stratgies against white players as all the top 5 openings by black have Defense in their opening name

#unique number of openings
nlevels(factor(df$opening_name))
nlevels(factor(df$opening_eco))
openings <- factor(df$opening_eco)
openings
nlevels(factor(df$victory_status))
df$moves

fct_infreq(df$opening_eco)

### Balanced Dataset

# Generate a bar chart
ggplot(df, aes(x = winner)) +
  geom_bar(fill = "skyblue") +  # Automatically counts occurrences
  labs(title = "Bar Chart with Counts",  # Title of the chart
       x = "Categories",             # X-axis label
       y = "Count") +                # Y-axis label
  theme_minimal()                    # Use a minimal theme

# Count unique items in the "Category" column for each group
unique_counts <- df %>%
  group_by(winner) %>%
  summarise(unique_count = n_distinct(Category))

# Print the result
print(unique_counts)


#Q4 <- Osama.

chess_data_raw <- dataset

# Function to extract the first N half-moves from a move string
extract_n_half_moves <- function(move_string, n_half_moves) {
  if (is.na(move_string) || move_string == "") return(NA_character_)
  moves_list <- str_split(move_string, "\\s+")[[1]]
  moves_list <- moves_list[moves_list != ""]
  if (length(moves_list) >= n_half_moves) {
    return(paste(moves_list[1:n_half_moves], collapse = " "))
  } else {
    return(paste(moves_list, collapse = " "))
  }
}

# Function to define rating groups
rating_group <- function(rating) {
  cut(rating,
      breaks = c(-Inf, 1250, 1750, Inf),
      labels = c("Low", "Medium", "High"),
      right = FALSE)
}

# Clean data and add new features in a single pipeline
chess_data <- chess_data_raw %>%
  mutate(
    winner = tolower(trimws(winner)),
    outcome = case_when(
      winner == "white" ~ "white_win",
      winner == "black" ~ "black_win",
      TRUE ~ "draw"
    )
  ) %>%
  rowwise() %>%
  mutate(
    first_10_half_moves = extract_n_half_moves(moves, 10)
  ) %>%
  ungroup() %>%
  mutate(
    white_rating_group = rating_group(white_rating),
    black_rating_group = rating_group(black_rating),
    game_duration_group = ifelse(turns <= median(turns, na.rm = TRUE), "Short", "Normal")
  )


# Define analysis parameters
min_games_threshold <- 10
top_n_display <- 10

# Find the most common opening sequences to focus our analysis on
top_moves <- chess_data %>%
  filter(!is.na(first_10_half_moves)) %>%
  count(first_10_half_moves, sort = TRUE) %>%
  top_n(top_n_display, wt = n) %>%
  pull(first_10_half_moves)

# Calculate stats for White's rating group
white_rating_patterns <- chess_data %>%
  filter(first_10_half_moves %in% top_moves) %>%
  group_by(first_10_half_moves, white_rating_group) %>%
  summarise(white_wins = sum(outcome == "white_win"), count = n(), .groups = "drop") %>%
  mutate(
    analysis_type = "White Player Rating",
    grouping_level = white_rating_group,
    white_win_rate = white_wins / count
  ) %>%
  dplyr::select(first_10_half_moves, analysis_type, grouping_level, white_win_rate)

# Calculate stats for Black's rating group
black_rating_patterns <- chess_data %>%
  filter(first_10_half_moves %in% top_moves) %>%
  group_by(first_10_half_moves, black_rating_group) %>%
  summarise(white_wins = sum(outcome == "white_win"), count = n(), .groups = "drop") %>%
  mutate(
    analysis_type = "Black Player Rating",
    grouping_level = black_rating_group,
    white_win_rate = white_wins / count
  ) %>%
  dplyr::select(first_10_half_moves, analysis_type, grouping_level, white_win_rate)

# Calculate stats for game duration
duration_patterns <- chess_data %>%
  filter(first_10_half_moves %in% top_moves) %>%
  group_by(first_10_half_moves, game_duration_group) %>%
  summarise(white_wins = sum(outcome == "white_win"), count = n(), .groups = "drop") %>%
  mutate(
    analysis_type = "Game Duration",
    grouping_level = game_duration_group,
    white_win_rate = white_wins / count
  ) %>%
  dplyr::select(first_10_half_moves, analysis_type, grouping_level, white_win_rate)

# Combine all three dataframes into one for faceting
combined_patterns_data <- bind_rows(
  white_rating_patterns,
  black_rating_patterns,
  duration_patterns
)

# --- 4. Create the Combined, Faceted Visualization ---

p10 <- ggplot(plot_data_10, aes(x = reorder(first_10_half_moves, win_rate), y = win_rate, fill = player_perspective)) +
  
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  
  coord_flip() +
  
  scale_y_continuous(labels = percent_format()) +
  
  labs(
    
    title = paste("Win Rates for Top", top_n_display, "Opening Sequences"),
    
    subtitle = "(First 10 Half-Moves, min", min_games_threshold_10, "games per sequence)",
    
    x = "Opening Sequence (First 10 Half-Moves)",
    
    y = "Win Rate",
    
    fill = "Player"
    
  ) +
  
  theme_minimal(base_size = 10) +
  
  theme(
    
    plot.title = element_text(hjust = 0.5, face = "bold"),
    
    plot.subtitle = element_text(hjust = 0.5),
    
    axis.text.y = element_text(size = 8),
    
    legend.position = "top"
  )

print(p10)



# To ensure the moves are ordered nicely, we reorder them based on their average win rate
# across all analyses.
faceted_plot <- combined_patterns_data %>%
  mutate(
    first_10_half_moves = fct_reorder2(first_10_half_moves, grouping_level, white_win_rate)
  ) %>%
  ggplot(aes(x = first_10_half_moves, y = white_win_rate, fill = grouping_level)) +
  geom_col(position = position_dodge(width = 0.9)) +
  coord_flip() +
  # Create a separate subplot for each analysis type, stacked vertically
  facet_wrap(~ analysis_type, ncol = 1, scales = "free_y") +
  scale_y_continuous(labels = percent_format()) +
  labs(
    title = "White's Win Rate for Top Opening Sequences by Grouping",
    subtitle = paste("Comparing the influence of player ratings and game duration on the top", top_n_display, "openings"),
    x = "Opening Sequence (First 10 Half-Moves)",
    y = "White Win Rate",
    fill = "Group" # The legend title is now generic
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 10),
    legend.position = "top",
    # Make facet titles stand out
    strip.text = element_text(face = "bold", size = 11, hjust = 0.5),
    panel.spacing = unit(1.5, "lines") # Add some space between facets
  )

print(faceted_plot)
