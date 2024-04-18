# Celine
# Project Kaizo Ironmon

setwd('C:/Users/mc_ry/Documents/R Code')
install.packages("ggplot2")
install.packages("plyr")
install.packages("dplyr")
list.files()
ironmon <- read.csv("FRLG_kaizo_ironmon.csv")

View(ironmon)

library(ggplot2)
library(plyr)
library(dplyr)


# Bar plot of Win status by Pokemon
ggplot(ironmon, aes(x = Pokemon, fill = Win)) +
  geom_bar() +
  labs(title = "Win Status by Pokemon", x = "Pokemon", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Violin plot for Special Attack vs Win
ggplot(ironmon, aes(x = Win, y = SpecialAttack, fill = Win)) +
  geom_violin(trim = FALSE) +
  labs(title = "Violin Plot: Special Attack vs Win",
       x = "Win",
       y = "Special Attack") +
  theme_minimal()

# Facet grid for Type1 vs Special Attack with Win Status
ggplot(ironmon, aes(x = Type1, y = SpecialAttack, fill = Win)) +
  geom_boxplot() +
  labs(title = "Facet Grid: Type1 vs Special Attack with Win Status",
       x = "Type1",
       y = "Special Attack",
       fill = "Win") +
  facet_wrap(~ Win) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Box plot of BST by Win status
ggplot(ironmon, aes(x = Win, y = BST, fill = Win)) +
  geom_boxplot() +
  labs(title = "BST by Win Status", x = "Win", y = "BST") +
  theme_minimal()

# Scatter plot with smoothed line
ggplot(ironmon, aes(x = SpecialAttack, y = Defense)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Scatter Plot: Special Attack vs Defense",
       x = "Special Attack",
       y = "Defense") +
  theme_minimal()

# Shapiro-Wilk test for IV
shapiro_test_IV <- shapiro.test(ironmon$IV)
shapiro_test_IV

# Shapiro-Wilk test for Speed
shapiro_test_speed <- shapiro.test(ironmon$Speed)
shapiro_test_speed

# Shapiro-Wilk test for Attack
shapiro_test_atk <- shapiro.test(ironmon$Attack)
shapiro_test_atk

# Shapiro-Wilk test for Defense
shapiro_test_def <- shapiro.test(ironmon$Defense)
shapiro_test_def

# Shapiro-Wilk test for HP
shapiro_test_HP <- shapiro.test(ironmon$HP)
shapiro_test_HP

# Shapiro-Wilk test for Special Defense
shapiro_test_spD <- shapiro.test(ironmon$SpecialDefense)
shapiro_test_spD

# Shapiro-Wilk test for Special Attack
shapiro_test_spA <- shapiro.test(ironmon$SpecialAttack)
shapiro_test_spA

# Shapiro-Wilk test for Special Attack
shapiro_test_lvl <- shapiro.test(ironmon$PokemonLvl)
shapiro_test_lvl

# Shapiro-Wilk test for Base Stat
shapiro_test_bst <- shapiro.test(ironmon$BST)
shapiro_test_bst

# Filter data for Pokemon with BST under 500
pokemon_bst_under_500 <- ironmon %>% 
  filter(BST < 500)

# Bar plot of Wins for Pokemon with BST under 500
ggplot(pokemon_bst_under_500, aes(x = factor(Win), fill = factor(Win))) +
  geom_bar() +
  labs(title = "Wins for Pokemon with BST under 500",
       x = "Win",
       y = "Count") +
  scale_fill_manual(values = c("red", "blue")) +  # Colors for wins
  theme_minimal()

# Assuming ironmon is your data frame
pokemon_bst_under_450 <- ironmon %>% 
  filter(BST < 450)

# View the filtered data
head(pokemon_bst_under_450)

# Bar plot of BST under 450 vs. Wins
ggplot(pokemon_bst_under_450, aes(x = Pokemon, fill = Win)) +
  geom_bar() +
  labs(title = "Wins vs. BST under 450 for All Types",
       x = "Pokemon",
       y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Bar plot of Wins by Type1
ggplot(ironmon, aes(x = Type1, fill = Win)) +
  geom_bar() +
  labs(title = "Wins by Type1",
       x = "Type1",
       y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Stacked bar plot of Wins by Type1 and Type2
ggplot(ironmon, aes(x = Type1, fill = Type2)) +
  geom_bar(position = "stack") +
  labs(title = "Wins by Type1 and Type2",
       x = "Type1",
       y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  guides(fill = guide_legend(title = "Type2"))

library(dplyr)

# Calculate total wins and losses by Type1 and Type2
wins_losses <- ironmon %>%
  group_by(Type1, Type2) %>%
  summarize(TotalWins = sum(Win == "TRUE"),
            TotalLosses = sum(Win == "FALSE"))

# View the wins_losses table
print(wins_losses)

# Type1 and Type2 with most wins
most_wins <- wins_losses %>%
  filter(TotalWins == max(TotalWins))

# Type1 and Type2 with most losses
most_losses <- wins_losses %>%
  filter(TotalLosses == max(TotalLosses))

# Print the results
cat("Type1 and Type2 with the most wins:\n")
print(most_wins)

cat("\nType1 and Type2 with the most losses:\n")
print(most_losses)

# Calculate counts of wins and losses
win_counts <- sum(ironmon$Win == TRUE)
loss_counts <- sum(ironmon$Win == FALSE)

# Create a data frame for the pie chart
pie_data <- data.frame(Win = c("Wins", "Losses"),
                       Count = c(win_counts, loss_counts))

# Create the pie chart
ggplot(pie_data, aes(x = "", y = Count, fill = Win)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  labs(title = "Pie Chart of Win Status",
       fill = "Win") +
  theme_void() +
  theme(legend.position = "right") +
  geom_text(aes(label = paste0(round(Count/sum(Count) * 100), "%")),
            position = position_stack(vjust = 0.5))

# Filter data for Pokemon that won
winning_pokemon <- ironmon %>%
  filter(Win == TRUE)

# Count occurrences of each Type1 for winners
win_type1_counts <- count(winning_pokemon, Type1)

# Create color palette for types that win
win_color_palette <- c("#1F77B4", "#FF7F0E", "#2CA02C", "#D62728", "#9467BD", "#8C564B", "#E377C2", "#7F7F7F", "#BCBD22", "#17BECF")

# Create pie chart for types that win
ggplot(win_type1_counts, aes(x = "", y = n, fill = Type1)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  labs(title = "Pie Chart of Types that Win",
       fill = "Type1") +
  scale_fill_manual(values = win_color_palette) +  # Use color palette
  theme_void() +
  theme(legend.position = "right") +
  geom_text(aes(label = paste0(round(n/sum(n) * 100), "%")),
            position = position_stack(vjust = 0.5))

# Calculate total wins by Type1
total_wins_type1 <- ironmon %>%
  filter(Win == TRUE) %>%
  count(Type1, sort = TRUE)

# View the total wins by Type1
print(total_wins_type1)

# Plot the bar chart
ggplot(total_wins_type1, aes(x = reorder(Type1, -n), y = n, fill = Type1)) +
  geom_bar(stat = "identity") +
  labs(title = "Total Wins by Type1",
       x = "Type1",
       y = "Total Wins") +
  scale_fill_manual(values = win_color_palette) +  # Use color palette
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Calculate total wins by Type2
total_wins_type2 <- ironmon %>%
  filter(Win == TRUE) %>%
  count(Type2, sort = TRUE)

# View the total wins by Type2
print(total_wins_type2)

# Plot the bar chart
ggplot(total_wins_type2, aes(x = reorder(Type2, -n), y = n, fill = Type2)) +
  geom_bar(stat = "identity") +
  labs(title = "Total Wins by Type2",
       x = "Type2",
       y = "Total Wins") +
  scale_fill_manual(values = win_color_palette) +  # Use color palette
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Descriptive statistics for Attack
attack_stats <- summarise(ironmon, 
                          Mean_Attack = mean(Attack),
                          Median_Attack = median(Attack),
                          Min_Attack = min(Attack),
                          Max_Attack = max(Attack),
                          SD_Attack = sd(Attack))

# Print the results
print(attack_stats)

# Correlation matrix for numerical columns
correlation_matrix <- cor(ironmon[, c("HP", "Attack", "Defense", "SpecialAttack", "SpecialDefense", "Speed", "PokemonLvl", "BST", "IV")])

# Print the correlation matrix332x2-p8u
print(correlation_matrix)

# ANOVA for Attack across different Types
attack_anova <- aov(Attack ~ Type1, data = ironmon)

# Summary of ANOVA results
summary(attack_anova)

# Linear regression for Attack based on Speed and SpecialAttack
attack_lm <- lm(Attack ~ Speed + SpecialAttack, data = ironmon)

# Summary of linear regression model
summary(attack_lm)

# Linear regression for Wins vs. Speed
win_speed_lm <- lm(Win ~ Speed, data = ironmon)

# Get summary of the linear model
summary(win_speed_lm)
