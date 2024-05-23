Title
================

## GitHub Documents

This is an R Markdown format used for publishing markdown documents to
GitHub. When you click the **Knit** button all R code chunks are run and
a markdown file (.md) suitable for publishing to GitHub is generated.

``` r
library(ggplot2)

library(caret) # for data preprocessing and model training
```

    ## Loading required package: lattice

``` r
library(randomForest) 
```

    ## randomForest 4.7-1.1

    ## Type rfNews() to see new features/changes/bug fixes.

    ## 
    ## Attaching package: 'randomForest'

    ## The following object is masked from 'package:ggplot2':
    ## 
    ##     margin

``` r
# For Logistic Regression
library(glmnet)
```

    ## Loading required package: Matrix

    ## Loaded glmnet 4.1-8

``` r
library(nnet)
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following object is masked from 'package:randomForest':
    ## 
    ##     combine

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
# For Support Vector Machines
library(e1071)
```

``` r
data<- read.csv("Fifa_world_cup_matches.csv")
```

``` r
data$total.attempts.team1 <- as.numeric(data$total.attempts.team1)
data$total.attempts.team2 <- as.numeric(data$total.attempts.team2)
data$number.of.goals.team1 <- as.numeric(data$number.of.goals.team1)
data$number.of.goals.team2 <- as.numeric(data$number.of.goals.team2)

all_attempts <- c(data$total.attempts.team1, data$total.attempts.team2)
all_goals <- c(data$number.of.goals.team1, data$number.of.goals.team2)
group <- rep(c("Team1", "Team2"), times = c(length(data$total.attempts.team1), length(data$total.attempts.team2)))
bartlett_test <- bartlett.test(all_attempts, group)
print(bartlett_test)
```

    ## 
    ##  Bartlett test of homogeneity of variances
    ## 
    ## data:  all_attempts and group
    ## Bartlett's K-squared = 1.5006, df = 1, p-value = 0.2206

``` r
if (bartlett_test$p.value > 0.05) {
  cat("Variance is equal. Performing t-test with equal variances.\n")
  t_test_result <- t.test(all_attempts, all_goals, var.equal = TRUE)
} else {
  cat("Variance is not equal. Performing t-test with unequal variances.\n")
  t_test_result <- t.test(all_attempts, all_goals, var.equal = FALSE)
}
```

    ## Variance is equal. Performing t-test with equal variances.

``` r
print(t_test_result)
```

    ## 
    ##  Two Sample t-test
    ## 
    ## data:  all_attempts and all_goals
    ## t = 20.114, df = 254, p-value < 2.2e-16
    ## alternative hypothesis: true difference in means is not equal to 0
    ## 95 percent confidence interval:
    ##   8.901081 10.833294
    ## sample estimates:
    ## mean of x mean of y 
    ##  11.21094   1.34375

``` r
if (t_test_result$p.value < 0.05) {
  cat("There is a statistically significant difference between attempts and goals, suggesting that more attempts might lead to more goals.\n")
} else {
  cat("There is no statistically significant difference between attempts and goals, suggesting that more attempts do not necessarily lead to more goals.\n")
}
```

    ## There is a statistically significant difference between attempts and goals, suggesting that more attempts might lead to more goals.

``` r
data$possession.team1 <- as.numeric(sub("%", "", data$possession.team1))
data$possession.team2 <- as.numeric(sub("%", "", data$possession.team2))

data$team_wins <- ifelse(data$number.of.goals.team1 > data$number.of.goals.team2, "Team 1 Wins",
                         ifelse(data$number.of.goals.team1 < data$number.of.goals.team2, "Team 2 Wins", "Draw"))

data <- data %>%
  mutate(winning_possession = case_when(
    team_wins == "Team 1 Wins" ~ possession.team1,
    team_wins == "Team 2 Wins" ~ possession.team2,
    TRUE ~ NA  # Exclude draws for the t-test
  )) %>%
  mutate(non_winning_possession = case_when(
    team_wins == "Team 1 Wins" ~ possession.team2,
    team_wins == "Team 2 Wins" ~ possession.team1,
    team_wins == "Draw" ~ NA  # Exclude draws for the t-test
  ))

winning_possession <- na.omit(data$winning_possession)
non_winning_possession <- na.omit(data$non_winning_possession)

possession <- c(winning_possession, non_winning_possession)
group <- c(rep("Winning", length(winning_possession)), rep("Non-Winning", length(non_winning_possession)))

bartlett_test <- bartlett.test(possession, group)
print(bartlett_test)
```

    ## 
    ##  Bartlett test of homogeneity of variances
    ## 
    ## data:  possession and group
    ## Bartlett's K-squared = 0.23033, df = 1, p-value = 0.6313

``` r
if (bartlett_test$p.value > 0.05) {  
  t_test_possession <- t.test(winning_possession, non_winning_possession, var.equal = TRUE)
} else {
  t_test_possession <- t.test(winning_possession, non_winning_possession, var.equal = FALSE)
}

print(t_test_possession)
```

    ## 
    ##  Two Sample t-test
    ## 
    ## data:  winning_possession and non_winning_possession
    ## t = -1.0582, df = 96, p-value = 0.2926
    ## alternative hypothesis: true difference in means is not equal to 0
    ## 95 percent confidence interval:
    ##  -7.747300  2.359544
    ## sample estimates:
    ## mean of x mean of y 
    ##  42.71429  45.40816

``` r
print("Based on the analysis, the null hypothesis test indicates that there is no significant difference in possession between winning and non-winning teams.")
```

    ## [1] "Based on the analysis, the null hypothesis test indicates that there is no significant difference in possession between winning and non-winning teams."

``` r
data$penalty_involved <- with(data, ifelse(penalties.scored.team1 > 0 | penalties.scored.team2 > 0, "With Penalties", "Without Penalties"))

data$penalty_involved <- as.factor(data$penalty_involved)

data$total_goals <- with(data, number.of.goals.team1 + number.of.goals.team2)

t_test_penalties <- t.test(total_goals ~ penalty_involved, data = data, var.equal = TRUE)
print(t_test_penalties)
```

    ## 
    ##  Two Sample t-test
    ## 
    ## data:  total_goals by penalty_involved
    ## t = 3.3013, df = 62, p-value = 0.0016
    ## alternative hypothesis: true difference in means between group With Penalties and group Without Penalties is not equal to 0
    ## 95 percent confidence interval:
    ##  0.676261 2.752310
    ## sample estimates:
    ##    mean in group With Penalties mean in group Without Penalties 
    ##                        4.000000                        2.285714

``` r
print("As we can see we are rejecting the null hypothesis that means that teams with more penalties have more goals scored in the end of the match")
```

    ## [1] "As we can see we are rejecting the null hypothesis that means that teams with more penalties have more goals scored in the end of the match"

``` r
data$TotalCards <- rowSums(data[, c('yellow.cards.team1', 'red.cards.team1', 
                                    'yellow.cards.team2', 'red.cards.team2')], na.rm = TRUE)

data$TotalFouls <- rowSums(data[, c('fouls.against.team1', 'fouls.against.team2')], na.rm = TRUE)

data$AggressionLevel <- cut(data$TotalFouls + data$TotalCards,
                            breaks = quantile(data$TotalFouls + data$TotalCards, 
                                              probs = c(0, 1/3, 2/3, 1), na.rm = TRUE),
                            labels = c("Low", "Medium", "High"), include.lowest = TRUE)

data$AggressionLevel <- as.factor(data$AggressionLevel)

data$total_goals <- with(data, number.of.goals.team1 + number.of.goals.team2)

anova_results <- aov(total_goals ~ AggressionLevel, data = data)
summary_anova <- summary(anova_results)

print(summary_anova)
```

    ##                 Df Sum Sq Mean Sq F value Pr(>F)
    ## AggressionLevel  2   2.99   1.493   0.409  0.666
    ## Residuals       61 222.76   3.652

``` r
print("The physical aggression does not influence the average goals per match ")
```

    ## [1] "The physical aggression does not influence the average goals per match "

``` r
# Function to Combine Teams
combining_teams <- function(col1, col2) {
  # Combine two columns into one dataframe
  team.stat <- rbind(
    data.frame(team = data$team1, total = col1),
    data.frame(team = data$team2, total = col2)
  )
  # Aggregate the total for each team
  aggregate(total ~ team, data = team.stat, sum)
}
```

``` r
# Function to Calculate Top N Teams
calculate_top_n_teams <- function(data, col, n) {
  # Order data and select top N teams
  data[order(data[[col]], decreasing = TRUE), ][1:n, ]
}
```

``` r
# Function to Combine Attempts and Goals Data
combine_attempts_goals <- function(attempts_col1, attempts_col2, goals_data) {
  # Combine attempts data and goals data for each team
  team.attempts <- combining_teams(attempts_col1, attempts_col2)
  team.goals.attempts <- merge(team.attempts, goals_data, by = "team")
  # Rename columns for clarity
  colnames(team.goals.attempts) <- c("team", "total.attempts", "total.goals")
  # Order teams by total goals
  team.goals.attempts <- team.goals.attempts[order(team.goals.attempts$total.goals, decreasing = TRUE), ]
  team.goals.attempts
}
```

``` r
# Define Teams for Different Stages
round_of_16 <- c("Netherlands", "Senegal", "England", "USA", "Argentina", "Poland", "France", "Australia", 
                 "Japan", "Spain", "Morocco", "Croatia", "Brazil", "Switzerland", "Portugal", "South Korea")
quarter_finals <- c("Netherlands", "England", "Argentina", "France", "Spain", "Brazil", "Portugal", "Croatia")
semi_finals <- c("Croatia", "Argentina", "Morocco", "France")
final <- c("Argentina", "France")
```

``` r
# Create Qualification Dataframe
all_teams <- unique(c(round_of_16, quarter_finals, semi_finals, final))
qualification <- data.frame(
  team = all_teams,
  stage_reached = NA_character_
)

# Assign stages reached to teams
qualification$stage_reached[qualification$team %in% round_of_16] <- "Round of 16"
qualification$stage_reached[qualification$team %in% quarter_finals] <- "Quarter-finals"
qualification$stage_reached[qualification$team %in% semi_finals] <- "Semi-finals"
qualification$stage_reached[qualification$team %in% final] <- "Final"

qualification$stage_reached[is.na(qualification$stage_reached)] <- "Did not qualify to R16"
qualification$team <- toupper(qualification$team)
```

``` r
# Calculate Team Goals
team_goals <- combining_teams(data$number.of.goals.team1, data$number.of.goals.team2)
top_10_teams_goals <- calculate_top_n_teams(team_goals, "total", 10)

# Calculate Possession Data
data$possession.team1 <- as.numeric(sub("%", "", data$possession.team1))
data$possession.team2 <- as.numeric(sub("%", "", data$possession.team2))
possession_by_team <- combining_teams(data$possession.team1, data$possession.team2)
possession_by_team$average_possession <- possession_by_team$total / table(c(data$team1, data$team2))[possession_by_team$team]
top_10_teams_pos <- calculate_top_n_teams(possession_by_team, "total", 10)

# Calculate Team Attempts and Goals
team_goals_attempts <- combine_attempts_goals(data$total.attempts.team1, data$total.attempts.team2, team_goals)
```

``` r
# Merge Dataframes
combined_data <- merge(team_goals_attempts, qualification, by = "team", all.x = TRUE)
combined_data$stage_reached[is.na(combined_data$stage_reached)] <- "Did not qualify to R16"


team_goals_attempts$goals.per.attempts <- team_goals_attempts$total.attempts / team_goals_attempts$total.goals
top_10_conv <- head(team_goals_attempts[order(team_goals_attempts$goals.per.attempts, decreasing = TRUE), ], 10)


combined_data <- merge(combined_data, possession_by_team, by = "team")


conceding.data <- combining_teams(data$conceded.team1, data$conceded.team2)
combined_data <- merge(combined_data, conceding.data, by = "team")
colnames(combined_data)[colnames(combined_data) == "total.y"] <- "total.conceded"
combined_data$total.x <- NULL
top_10_conceded <- calculate_top_n_teams(combined_data, "total.conceded", 10)


total.passes<-combining_teams(data$passes.team1,data$passes.team2)
combined_data<-merge(combined_data,total.passes,by.x="team",by.y = "team")
colnames(combined_data)[colnames(combined_data) == "total"] <- "total.passes"


passes.completed<-combining_teams(data$passes.completed.team1,data$passes.completed.team2)
combined_data<-merge(combined_data,passes.completed,by.x="team",by.y = "team")
colnames(combined_data)[colnames(combined_data)=="total"]<-"passes.completed"
combined_data$pass.accuracy<-(combined_data$passes.completed/combined_data$total.passes)*100


yellow.cards<-combining_teams(data$yellow.cards.team1,data$yellow.cards.team2)
combined_data<-merge(combined_data,yellow.cards,by.x="team",by.y = "team")
colnames(combined_data)[colnames(combined_data)=="total"]<-"yellow.cards"


red.cards<-combining_teams(data$red.cards.team1,data$red.cards.team2)
combined_data<-merge(combined_data,red.cards,by.x="team",by.y = "team")
colnames(combined_data)[colnames(combined_data)=="total"]<-"red.cards"


team_regions <- data.frame(
  team = c("ARGENTINA", "AUSTRALIA", "BELGIUM", "BRAZIL", "CAMEROON", "CANADA", "COSTA RICA", "CROATIA", "DENMARK", "ECUADOR", 
           "ENGLAND", "FRANCE", "GERMANY", "GHANA", "IRAN", "JAPAN", "KOREA REPUBLIC", "MEXICO", "MOROCCO", "NETHERLANDS", 
           "POLAND", "PORTUGAL", "QATAR", "SAUDI ARABIA", "SENEGAL", "SERBIA", "SPAIN", "SWITZERLAND", "TUNISIA", "UNITED STATES", 
           "URUGUAY", "WALES"),
  region = c("South America", "Oceania", "Europe", "South America", "Africa", "North America", "North America", "Europe", "Europe", "South America", 
             "Europe", "Europe", "Europe", "Africa", "Asia", "Asia", "Asia", "North America", "Africa", "Europe", "Europe", "Europe", 
             "Asia", "Asia", "Africa", "Europe", "Europe", "Europe", "Africa", "North America", "South America", "Europe")
)

combined_data <- merge(combined_data, team_regions, by = "team")
continent_counts <- table(combined_data$region)
percentages <- round(100 * continent_counts / sum(continent_counts), 1)


goal.preventions<-combining_teams(data$goal.preventions.team1,data$goal.preventions.team2)
combined_data<-merge(combined_data,goal.preventions,by.x = "team",by.y="team")
colnames(combined_data)[colnames(combined_data)=="total"]<-"goal.preventions"



combined_data$goal_prevention_ratio <- combined_data$goal.preventions / combined_data$total.conceded
pie_data <- data.frame(Continent = names(continent_counts), Percentage = percentages)
```

``` r
# Top 10 Scoring Teams
ggplot(data = top_10_teams_goals, aes(x = reorder(team, total), y = total)) +
  geom_col(fill = "maroon", color = "gold") +
  labs(x = "Team", y = "Total Goals", title = "Top 10 Scoring Teams") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 0.9),
        plot.title = element_text(hjust = 0.5))
```

![](Title_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->

``` r
# Top 10 Teams (Average Possession)
ggplot(data = top_10_teams_pos, aes(x = reorder(team, average_possession), y = average_possession)) +
  geom_col(fill = "maroon", color = "gold") +
  labs(x = "Team", y = "Possession %", title = "Top 10 Teams (Average Possession)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 0.9),
        plot.title = element_text(hjust = 0.5))
```

    ## Don't know how to automatically pick scale for object of type <table>.
    ## Defaulting to continuous.

![](Title_files/figure-gfm/unnamed-chunk-16-1.png)<!-- -->

``` r
# Total Attempts vs. Total Goals
ggplot(data = combined_data, aes(x = total.attempts, y = total.goals)) +
  geom_jitter(aes(color = stage_reached)) +
  geom_boxplot(alpha = 0.5, outlier.colour = NA,fill="maroon") +
  labs(x = "Total Attempts", y = "Total Goals", title = "Total Attempts vs. Total Goals", color = "Stage Reached") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 0.9),
        plot.title = element_text(hjust = 0.5))
```

    ## Warning: Continuous x aesthetic
    ## ℹ did you forget `aes(group = ...)`?

![](Title_files/figure-gfm/unnamed-chunk-17-1.png)<!-- -->

``` r
# Conversion Rate
ggplot(data = team_goals_attempts, aes(x = reorder(team, goals.per.attempts), y = goals.per.attempts)) +
  geom_col(fill = "maroon", color = "gold") +
  labs(x = "Team", y = "Goals/Attempts", title = "Conversion Rate") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 0.9),
        plot.title = element_text(hjust = 0.5))
```

![](Title_files/figure-gfm/unnamed-chunk-18-1.png)<!-- -->

``` r
# Total Goals vs. Average Possession
ggplot(data=combined_data,aes(x=average_possession,y=total.goals))+
  geom_point(aes(color=stage_reached))+
  labs(x = "Average Possession", y = "Total Goals", title = "Total Goals vs. Average Possession", color = "Stage Reached") +
  theme_minimal() +
  theme(plot.title = element_text(hjust=0.5))
```

    ## Don't know how to automatically pick scale for object of type <table>.
    ## Defaulting to continuous.

![](Title_files/figure-gfm/unnamed-chunk-19-1.png)<!-- -->

``` r
# Histogram/Density Plot of Average Possession
ggplot(data = combined_data, aes(x = average_possession)) +
  geom_histogram(aes(y = ..density..), fill = "maroon", color = "gold", bins = 10) +
  geom_density(fill = "gold", alpha = 0.3) +
  labs(x = "Average Possession", title = "Histogram/Density Plot of Average Possession") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
```

    ## Warning: The dot-dot notation (`..density..`) was deprecated in ggplot2 3.4.0.
    ## ℹ Please use `after_stat(density)` instead.
    ## This warning is displayed once every 8 hours.
    ## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
    ## generated.

    ## Don't know how to automatically pick scale for object of type <table>.
    ## Defaulting to continuous.

![](Title_files/figure-gfm/unnamed-chunk-20-1.png)<!-- -->

``` r
# Top 10 Goals Conceded
ggplot(data = top_10_conceded, aes(x = reorder(team, total.conceded), y = total.conceded)) +
  geom_col(fill = "maroon", color = "gold") +
  labs(x = "Team", y = "Goals Conceded", title = "Top 10 Goals Conceded") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 0.9),
        plot.title = element_text(hjust = 0.5))
```

![](Title_files/figure-gfm/unnamed-chunk-21-1.png)<!-- -->

``` r
# Total Passes By Team
ggplot(data = combined_data, aes(x = reorder(team, total.passes), y = total.passes)) +
  geom_col(fill = "maroon", color = "gold") +
  labs(x = "Teams", y = "Total Passes", title = "Total Passes By Team") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 0.9),
        plot.title = element_text(hjust = 0.5))
```

![](Title_files/figure-gfm/unnamed-chunk-22-1.png)<!-- -->

``` r
# Pass Accuracy By Team
ggplot(data = combined_data, aes(x = reorder(team, pass.accuracy), y = pass.accuracy)) +
  geom_col(fill = "maroon", color = "gold") +
  coord_cartesian(ylim = c(70,100)) +
  labs(x = "Teams", y = "Pass Accuracy %", title = "Pass Accuracy By Team") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 0.9),
        plot.title = element_text(hjust = 0.5))
```

![](Title_files/figure-gfm/unnamed-chunk-23-1.png)<!-- -->

``` r
# Passes Completed vs. Average Possession
ggplot(data = combined_data, aes(x = passes.completed, y = average_possession)) +
  geom_point(color = "orange") +
  geom_boxplot(fill = "maroon", alpha = 0.4, outlier.colour = "black") +
  labs(x = "Passes Completed", y = "Average Possession", title = "Passes Completed vs. Average Possession") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(hjust = 0.5, vjust = -0.5))
```

    ## Don't know how to automatically pick scale for object of type <table>.
    ## Defaulting to continuous.

    ## Warning: Continuous x aesthetic
    ## ℹ did you forget `aes(group = ...)`?

![](Title_files/figure-gfm/unnamed-chunk-24-1.png)<!-- -->

``` r
# Passes Completed vs. Total Goals
ggplot(data = combined_data, aes(x = passes.completed, y = total.goals)) +
  geom_point(color = "orange") +
  geom_boxplot(fill = "maroon", alpha = 0.4, outlier.colour = "black") +
  labs(x = "Passes Completed", y = "Total Goals", title = "Passes Completed vs. Total Goals") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(hjust = 0.5, vjust = -0.5))
```

    ## Warning: Continuous x aesthetic
    ## ℹ did you forget `aes(group = ...)`?

![](Title_files/figure-gfm/unnamed-chunk-25-1.png)<!-- -->

``` r
# Histogram/Density (Passes Completed)
ggplot(data = combined_data, aes(x = passes.completed)) +
  geom_histogram(aes(y = ..density..), fill = "maroon", color = "gold", bins = 10) +
  geom_density(fill = "gold", alpha = 0.3) +
  labs(x = "Passes Completed", title = "Histogram/Density (Passes Completed)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
```

![](Title_files/figure-gfm/unnamed-chunk-26-1.png)<!-- -->

``` r
# Red and Yellow Cards by Team
ggplot(combined_data, aes(x = reorder(team, yellow.cards + red.cards), y = yellow.cards + red.cards, fill = factor(red.cards))) +
  geom_col(position = "stack", width = 0.7) +
  scale_fill_manual(values = c("gold", "maroon"), name = "Red Cards") +
  labs(x = "Team", y = "Number of Cards", title = "Red and Yellow Cards by Team") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 0.5),
        plot.title = element_text(hjust = 0.5)) +
  coord_flip()
```

![](Title_files/figure-gfm/unnamed-chunk-27-1.png)<!-- -->

``` r
# Participation by Continent
colors <- c("#800000", "#9c1125", "#c02147", "#f5c24a", "#eaa021", "#d89000")
ggplot(pie_data, aes(x = "", y = Percentage.Freq, fill = Continent)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  labs(title = "Participation by Continent") +
  scale_fill_manual(values = colors) +  
  theme_void() +
  theme(legend.position = "right",
        plot.title = element_text(hjust = 0.5)) +
  geom_text(aes(label = paste0(Percentage.Freq, "%")), 
            position = position_stack(vjust = 0.5),
            color = "white")
```

![](Title_files/figure-gfm/unnamed-chunk-28-1.png)<!-- -->

``` r
# Goal Prevention Rate
ggplot(data = combined_data, aes(x = reorder(team, goal_prevention_ratio), y = goal_prevention_ratio)) +
  geom_col(fill = "maroon", color = "gold") +
  labs(x = "Team", y = "Goals/Attempts", title = "Goal Prevention Rate") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 0.9),
        plot.title = element_text(hjust = 0.5))
```

![](Title_files/figure-gfm/unnamed-chunk-29-1.png)<!-- -->

``` r
# Load required libraries
library(dplyr)

# Calculate average goals for each team
avg_goals_team1 <- mean(data$number.of.goals.team1)
avg_goals_team2 <- mean(data$number.of.goals.team2)

# Perform t-tests
# Student's t-test
t_test_student <- t.test(data$number.of.goals.team1, data$number.of.goals.team2)

# Welch's t-test
t_test_welch <- t.test(data$number.of.goals.team1, data$number.of.goals.team2, var.equal = FALSE)

# Perform ANOVA
anova_result <- aov(number.of.goals.team1 ~ number.of.goals.team2, data = data)

# Summarize results
cat("Average goals for Team 1:", avg_goals_team1, "\n")
```

    ## Average goals for Team 1: 1.578125

``` r
cat("Average goals for Team 2:", avg_goals_team2, "\n\n")
```

    ## Average goals for Team 2: 1.109375

``` r
cat("Student's t-test result:\n")
```

    ## Student's t-test result:

``` r
print(t_test_student)
```

    ## 
    ##  Welch Two Sample t-test
    ## 
    ## data:  data$number.of.goals.team1 and data$number.of.goals.team2
    ## t = 1.9984, df = 111.06, p-value = 0.04812
    ## alternative hypothesis: true difference in means is not equal to 0
    ## 95 percent confidence interval:
    ##  0.003946293 0.933553707
    ## sample estimates:
    ## mean of x mean of y 
    ##  1.578125  1.109375

``` r
cat("\nWelch's t-test result:\n")
```

    ## 
    ## Welch's t-test result:

``` r
print(t_test_welch)
```

    ## 
    ##  Welch Two Sample t-test
    ## 
    ## data:  data$number.of.goals.team1 and data$number.of.goals.team2
    ## t = 1.9984, df = 111.06, p-value = 0.04812
    ## alternative hypothesis: true difference in means is not equal to 0
    ## 95 percent confidence interval:
    ##  0.003946293 0.933553707
    ## sample estimates:
    ## mean of x mean of y 
    ##  1.578125  1.109375

``` r
cat("\nANOVA result:\n")
```

    ## 
    ## ANOVA result:

``` r
print(summary(anova_result))
```

    ##                       Df Sum Sq Mean Sq F value Pr(>F)
    ## number.of.goals.team2  1   0.05  0.0543   0.022  0.882
    ## Residuals             62 151.56  2.4444

``` r
# Remove '%' symbol and convert to numeric
data$Winner <- ifelse(data$number.of.goals.team1 > data$number.of.goals.team2, 1,
                    ifelse(data$number.of.goals.team1 < data$number.of.goals.team2, 2, 0))
```

``` r
selected_features <- c( "total.attempts.team1", "total.attempts.team2",
                       "on.target.attempts.team1", "on.target.attempts.team2", "off.target.attempts.team1", 
                       "off.target.attempts.team2", "assists.team1", "assists.team2", "goal.preventions.team1",
                       "goal.preventions.team2", "passes.team1", "passes.team2", "crosses.team1", "crosses.team2",
                       "defensive.pressures.applied.team1","defensive.pressures.applied.team2","penalty_involved", "Winner")

# Create a subset with selected features
data_subset <- data[selected_features]

# Convert possession to numeric
#data_subset$possession.team1 <- as.numeric(sub("%", "", data_subset$possession.team1))/100
#data_subset$possession.team2 <- as.numeric(sub("%", "", data_subset$possession.team2))/100
```

``` r
# Split data into training and testing sets
set.seed(123) # for reproducibility
train_index <- createDataPartition(data_subset$Winner, p = 0.8, list = FALSE)
train_data <- data_subset[train_index, ]
test_data <- data_subset[-train_index, ]

train_data$Winner <- factor(train_data$Winner, levels = c(0, 1, 2))
test_data$Winner <- factor(test_data$Winner, levels = c(0, 1, 2))

# Train a random forest classifier
model <- train(Winner ~ ., data = train_data, method = "rf")

# Predict on test data
predictions <- predict(model, newdata = test_data)

# Evaluate the model
confusionMatrix(predictions, test_data$Winner)
```

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction 0 1 2
    ##          0 1 2 0
    ##          1 1 4 1
    ##          2 0 0 3
    ## 
    ## Overall Statistics
    ##                                           
    ##                Accuracy : 0.6667          
    ##                  95% CI : (0.3489, 0.9008)
    ##     No Information Rate : 0.5             
    ##     P-Value [Acc > NIR] : 0.1938          
    ##                                           
    ##                   Kappa : 0.4667          
    ##                                           
    ##  Mcnemar's Test P-Value : NA              
    ## 
    ## Statistics by Class:
    ## 
    ##                      Class: 0 Class: 1 Class: 2
    ## Sensitivity           0.50000   0.6667   0.7500
    ## Specificity           0.80000   0.6667   1.0000
    ## Pos Pred Value        0.33333   0.6667   1.0000
    ## Neg Pred Value        0.88889   0.6667   0.8889
    ## Prevalence            0.16667   0.5000   0.3333
    ## Detection Rate        0.08333   0.3333   0.2500
    ## Detection Prevalence  0.25000   0.5000   0.2500
    ## Balanced Accuracy     0.65000   0.6667   0.8750

``` r
# Train logistic regression model using one-vs-all approach
log_model <- train(Winner ~ ., data = train_data, method = "multinom", trControl = trainControl(method = "repeatedcv", number = 10, repeats = 3))
```

    ## # weights:  57 (36 variable)
    ## initial  value 51.634778 
    ## iter  10 value 37.418345
    ## iter  20 value 22.930147
    ## iter  30 value 1.764617
    ## iter  40 value 0.002559
    ## final  value 0.000038 
    ## converged
    ## # weights:  57 (36 variable)
    ## initial  value 51.634778 
    ## iter  10 value 37.427285
    ## iter  20 value 23.968071
    ## iter  30 value 10.151713
    ## iter  40 value 9.338727
    ## iter  50 value 9.291846
    ## iter  60 value 9.291163
    ## iter  70 value 9.291128
    ## final  value 9.291128 
    ## converged
    ## # weights:  57 (36 variable)
    ## initial  value 51.634778 
    ## iter  10 value 37.418354
    ## iter  20 value 22.931260
    ## iter  30 value 1.846165
    ## iter  40 value 0.379404
    ## iter  50 value 0.333429
    ## iter  60 value 0.283558
    ## iter  70 value 0.257838
    ## iter  80 value 0.243272
    ## iter  90 value 0.226518
    ## iter 100 value 0.212708
    ## final  value 0.212708 
    ## stopped after 100 iterations
    ## # weights:  57 (36 variable)
    ## initial  value 51.634778 
    ## iter  10 value 38.641857
    ## iter  20 value 19.470893
    ## iter  30 value 0.708103
    ## iter  40 value 0.001044
    ## final  value 0.000062 
    ## converged
    ## # weights:  57 (36 variable)
    ## initial  value 51.634778 
    ## iter  10 value 38.667937
    ## iter  20 value 20.839923
    ## iter  30 value 10.383323
    ## iter  40 value 9.735154
    ## iter  50 value 9.683899
    ## iter  60 value 9.683458
    ## final  value 9.683453 
    ## converged
    ## # weights:  57 (36 variable)
    ## initial  value 51.634778 
    ## iter  10 value 38.641883
    ## iter  20 value 19.472405
    ## iter  30 value 0.779184
    ## iter  40 value 0.220210
    ## iter  50 value 0.195380
    ## iter  60 value 0.179932
    ## iter  70 value 0.168086
    ## iter  80 value 0.160166
    ## iter  90 value 0.155411
    ## iter 100 value 0.150517
    ## final  value 0.150517 
    ## stopped after 100 iterations
    ## # weights:  57 (36 variable)
    ## initial  value 52.733390 
    ## iter  10 value 35.313782
    ## iter  20 value 22.044417
    ## iter  30 value 1.338248
    ## iter  40 value 0.001836
    ## final  value 0.000059 
    ## converged
    ## # weights:  57 (36 variable)
    ## initial  value 52.733390 
    ## iter  10 value 35.331668
    ## iter  20 value 22.510358
    ## iter  30 value 11.009196
    ## iter  40 value 10.428136
    ## iter  50 value 10.353748
    ## iter  60 value 10.352104
    ## final  value 10.352081 
    ## converged
    ## # weights:  57 (36 variable)
    ## initial  value 52.733390 
    ## iter  10 value 35.313800
    ## iter  20 value 22.044904
    ## iter  30 value 1.425408
    ## iter  40 value 0.333552
    ## iter  50 value 0.297609
    ## iter  60 value 0.242309
    ## iter  70 value 0.209428
    ## iter  80 value 0.201225
    ## iter  90 value 0.189594
    ## iter 100 value 0.176332
    ## final  value 0.176332 
    ## stopped after 100 iterations
    ## # weights:  57 (36 variable)
    ## initial  value 50.536165 
    ## iter  10 value 38.076696
    ## iter  20 value 21.199736
    ## iter  30 value 1.626330
    ## iter  40 value 0.003076
    ## final  value 0.000044 
    ## converged
    ## # weights:  57 (36 variable)
    ## initial  value 50.536165 
    ## iter  10 value 38.089774
    ## iter  20 value 22.177105
    ## iter  30 value 10.421889
    ## iter  40 value 8.924498
    ## iter  50 value 8.860420
    ## iter  60 value 8.852364
    ## iter  70 value 8.852338
    ## final  value 8.852337 
    ## converged
    ## # weights:  57 (36 variable)
    ## initial  value 50.536165 
    ## iter  10 value 38.076709
    ## iter  20 value 21.200793
    ## iter  30 value 1.676242
    ## iter  40 value 0.262927
    ## iter  50 value 0.239372
    ## iter  60 value 0.210653
    ## iter  70 value 0.183590
    ## iter  80 value 0.164544
    ## iter  90 value 0.154693
    ## iter 100 value 0.145378
    ## final  value 0.145378 
    ## stopped after 100 iterations
    ## # weights:  57 (36 variable)
    ## initial  value 50.536165 
    ## iter  10 value 39.611505
    ## iter  20 value 22.165220
    ## iter  30 value 1.600798
    ## iter  40 value 0.002249
    ## final  value 0.000073 
    ## converged
    ## # weights:  57 (36 variable)
    ## initial  value 50.536165 
    ## iter  10 value 39.624172
    ## iter  20 value 22.847965
    ## iter  30 value 10.980062
    ## iter  40 value 10.438439
    ## iter  50 value 10.422644
    ## iter  60 value 10.422236
    ## final  value 10.422225 
    ## converged
    ## # weights:  57 (36 variable)
    ## initial  value 50.536165 
    ## iter  10 value 39.611518
    ## iter  20 value 22.165939
    ## iter  30 value 1.662036
    ## iter  40 value 0.299377
    ## iter  50 value 0.274494
    ## iter  60 value 0.261870
    ## iter  70 value 0.233487
    ## iter  80 value 0.201234
    ## iter  90 value 0.193260
    ## iter 100 value 0.186335
    ## final  value 0.186335 
    ## stopped after 100 iterations
    ## # weights:  57 (36 variable)
    ## initial  value 50.536165 
    ## iter  10 value 35.652793
    ## iter  20 value 17.503347
    ## iter  30 value 0.647651
    ## iter  40 value 0.000931
    ## final  value 0.000065 
    ## converged
    ## # weights:  57 (36 variable)
    ## initial  value 50.536165 
    ## iter  10 value 35.675706
    ## iter  20 value 18.430704
    ## iter  30 value 10.189549
    ## iter  40 value 9.848339
    ## iter  50 value 9.843018
    ## iter  60 value 9.842569
    ## final  value 9.842564 
    ## converged
    ## # weights:  57 (36 variable)
    ## initial  value 50.536165 
    ## iter  10 value 35.652816
    ## iter  20 value 17.504333
    ## iter  30 value 0.733502
    ## iter  40 value 0.241626
    ## iter  50 value 0.219967
    ## iter  60 value 0.201335
    ## iter  70 value 0.181179
    ## iter  80 value 0.169632
    ## iter  90 value 0.164760
    ## iter 100 value 0.159658
    ## final  value 0.159658 
    ## stopped after 100 iterations
    ## # weights:  57 (36 variable)
    ## initial  value 50.536165 
    ## iter  10 value 34.170827
    ## iter  20 value 19.988895
    ## iter  30 value 2.994366
    ## iter  40 value 0.003566
    ## final  value 0.000055 
    ## converged
    ## # weights:  57 (36 variable)
    ## initial  value 50.536165 
    ## iter  10 value 34.198901
    ## iter  20 value 20.817073
    ## iter  30 value 11.487291
    ## iter  40 value 10.861085
    ## iter  50 value 10.803167
    ## iter  60 value 10.799130
    ## final  value 10.799080 
    ## converged
    ## # weights:  57 (36 variable)
    ## initial  value 50.536165 
    ## iter  10 value 34.170855
    ## iter  20 value 19.989794
    ## iter  30 value 3.051079
    ## iter  40 value 0.396185
    ## iter  50 value 0.350402
    ## iter  60 value 0.325890
    ## iter  70 value 0.271271
    ## iter  80 value 0.259699
    ## iter  90 value 0.245774
    ## iter 100 value 0.232681
    ## final  value 0.232681 
    ## stopped after 100 iterations
    ## # weights:  57 (36 variable)
    ## initial  value 51.634778 
    ## iter  10 value 32.196751
    ## iter  20 value 17.024053
    ## iter  30 value 1.137955
    ## iter  40 value 0.001770
    ## final  value 0.000060 
    ## converged
    ## # weights:  57 (36 variable)
    ## initial  value 51.634778 
    ## iter  10 value 32.232091
    ## iter  20 value 16.779958
    ## iter  30 value 8.501598
    ## iter  40 value 8.238967
    ## iter  50 value 8.215165
    ## iter  60 value 8.213565
    ## iter  70 value 8.213551
    ## final  value 8.213551 
    ## converged
    ## # weights:  57 (36 variable)
    ## initial  value 51.634778 
    ## iter  10 value 32.196786
    ## iter  20 value 17.025245
    ## iter  30 value 1.188098
    ## iter  40 value 0.223537
    ## iter  50 value 0.200766
    ## iter  60 value 0.179653
    ## iter  70 value 0.155386
    ## iter  80 value 0.139812
    ## iter  90 value 0.134054
    ## iter 100 value 0.127928
    ## final  value 0.127928 
    ## stopped after 100 iterations
    ## # weights:  57 (36 variable)
    ## initial  value 51.634778 
    ## iter  10 value 36.243420
    ## iter  20 value 22.620635
    ## iter  30 value 2.774712
    ## iter  40 value 0.005681
    ## final  value 0.000050 
    ## converged
    ## # weights:  57 (36 variable)
    ## initial  value 51.634778 
    ## iter  10 value 36.256447
    ## iter  20 value 23.106656
    ## iter  30 value 11.198928
    ## iter  40 value 9.215722
    ## iter  50 value 9.151864
    ## iter  60 value 9.144273
    ## iter  70 value 9.143996
    ## final  value 9.143996 
    ## converged
    ## # weights:  57 (36 variable)
    ## initial  value 51.634778 
    ## iter  10 value 36.243433
    ## iter  20 value 22.621144
    ## iter  30 value 2.832448
    ## iter  40 value 0.427906
    ## iter  50 value 0.368052
    ## iter  60 value 0.323206
    ## iter  70 value 0.269826
    ## iter  80 value 0.237458
    ## iter  90 value 0.211926
    ## iter 100 value 0.202023
    ## final  value 0.202023 
    ## stopped after 100 iterations
    ## # weights:  57 (36 variable)
    ## initial  value 52.733390 
    ## iter  10 value 38.496308
    ## iter  20 value 22.490479
    ## iter  30 value 2.456692
    ## iter  40 value 0.003143
    ## final  value 0.000043 
    ## converged
    ## # weights:  57 (36 variable)
    ## initial  value 52.733390 
    ## iter  10 value 38.511095
    ## iter  20 value 23.377555
    ## iter  30 value 12.147049
    ## iter  40 value 11.463921
    ## iter  50 value 11.368985
    ## iter  60 value 11.367617
    ## final  value 11.367609 
    ## converged
    ## # weights:  57 (36 variable)
    ## initial  value 52.733390 
    ## iter  10 value 38.496322
    ## iter  20 value 22.491427
    ## iter  30 value 2.522551
    ## iter  40 value 0.373257
    ## iter  50 value 0.334490
    ## iter  60 value 0.287076
    ## iter  70 value 0.261683
    ## iter  80 value 0.253898
    ## iter  90 value 0.240685
    ## iter 100 value 0.220833
    ## final  value 0.220833 
    ## stopped after 100 iterations
    ## # weights:  57 (36 variable)
    ## initial  value 50.536165 
    ## iter  10 value 39.133087
    ## iter  20 value 23.284663
    ## iter  30 value 2.335577
    ## iter  40 value 0.004288
    ## final  value 0.000072 
    ## converged
    ## # weights:  57 (36 variable)
    ## initial  value 50.536165 
    ## iter  10 value 39.140752
    ## iter  20 value 23.895671
    ## iter  30 value 11.910353
    ## iter  40 value 10.963964
    ## iter  50 value 10.855978
    ## iter  60 value 10.851609
    ## iter  70 value 10.851544
    ## final  value 10.851544 
    ## converged
    ## # weights:  57 (36 variable)
    ## initial  value 50.536165 
    ## iter  10 value 39.133095
    ## iter  20 value 23.285305
    ## iter  30 value 2.393439
    ## iter  40 value 0.353204
    ## iter  50 value 0.317781
    ## iter  60 value 0.281314
    ## iter  70 value 0.261118
    ## iter  80 value 0.243339
    ## iter  90 value 0.226260
    ## iter 100 value 0.209170
    ## final  value 0.209170 
    ## stopped after 100 iterations
    ## # weights:  57 (36 variable)
    ## initial  value 52.733390 
    ## iter  10 value 36.509504
    ## iter  20 value 23.033870
    ## iter  30 value 0.778551
    ## iter  40 value 0.001126
    ## final  value 0.000080 
    ## converged
    ## # weights:  57 (36 variable)
    ## initial  value 52.733390 
    ## iter  10 value 36.524822
    ## iter  20 value 23.811250
    ## iter  30 value 10.886308
    ## iter  40 value 10.361929
    ## iter  50 value 10.349698
    ## iter  60 value 10.349534
    ## final  value 10.349517 
    ## converged
    ## # weights:  57 (36 variable)
    ## initial  value 52.733390 
    ## iter  10 value 36.509519
    ## iter  20 value 23.034690
    ## iter  30 value 0.895652
    ## iter  40 value 0.323192
    ## iter  50 value 0.300952
    ## iter  60 value 0.274911
    ## iter  70 value 0.259648
    ## iter  80 value 0.246082
    ## iter  90 value 0.237313
    ## iter 100 value 0.218507
    ## final  value 0.218507 
    ## stopped after 100 iterations
    ## # weights:  57 (36 variable)
    ## initial  value 51.634778 
    ## iter  10 value 38.030310
    ## iter  20 value 24.664988
    ## iter  30 value 1.586281
    ## iter  40 value 0.004024
    ## final  value 0.000025 
    ## converged
    ## # weights:  57 (36 variable)
    ## initial  value 51.634778 
    ## iter  10 value 38.041778
    ## iter  20 value 25.434870
    ## iter  30 value 11.718122
    ## iter  40 value 10.621054
    ## iter  50 value 10.522952
    ## iter  60 value 10.521166
    ## iter  70 value 10.521125
    ## final  value 10.521124 
    ## converged
    ## # weights:  57 (36 variable)
    ## initial  value 51.634778 
    ## iter  10 value 38.030322
    ## iter  20 value 24.665814
    ## iter  30 value 1.708697
    ## iter  40 value 0.498892
    ## iter  50 value 0.460629
    ## iter  60 value 0.363460
    ## iter  70 value 0.323507
    ## iter  80 value 0.295502
    ## iter  90 value 0.266567
    ## iter 100 value 0.254496
    ## final  value 0.254496 
    ## stopped after 100 iterations
    ## # weights:  57 (36 variable)
    ## initial  value 50.536165 
    ## iter  10 value 38.610814
    ## iter  20 value 18.543201
    ## iter  30 value 1.205730
    ## iter  40 value 0.001333
    ## final  value 0.000085 
    ## converged
    ## # weights:  57 (36 variable)
    ## initial  value 50.536165 
    ## iter  10 value 38.708659
    ## iter  20 value 20.010826
    ## iter  30 value 8.445994
    ## iter  40 value 7.930426
    ## iter  50 value 7.887575
    ## iter  60 value 7.884964
    ## final  value 7.884925 
    ## converged
    ## # weights:  57 (36 variable)
    ## initial  value 50.536165 
    ## iter  10 value 38.610912
    ## iter  20 value 18.544775
    ## iter  30 value 1.241253
    ## iter  40 value 0.168608
    ## iter  50 value 0.146177
    ## iter  60 value 0.140674
    ## iter  70 value 0.125483
    ## iter  80 value 0.113812
    ## iter  90 value 0.108155
    ## iter 100 value 0.104003
    ## final  value 0.104003 
    ## stopped after 100 iterations
    ## # weights:  57 (36 variable)
    ## initial  value 52.733390 
    ## iter  10 value 34.530241
    ## iter  20 value 22.676901
    ## iter  30 value 1.004272
    ## iter  40 value 0.001151
    ## final  value 0.000070 
    ## converged
    ## # weights:  57 (36 variable)
    ## initial  value 52.733390 
    ## iter  10 value 34.542299
    ## iter  20 value 23.053534
    ## iter  30 value 8.277463
    ## iter  40 value 7.825338
    ## iter  50 value 7.812220
    ## iter  60 value 7.811424
    ## iter  70 value 7.811397
    ## final  value 7.811396 
    ## converged
    ## # weights:  57 (36 variable)
    ## initial  value 52.733390 
    ## iter  10 value 34.530253
    ## iter  20 value 22.677294
    ## iter  30 value 1.075077
    ## iter  40 value 0.254716
    ## iter  50 value 0.226268
    ## iter  60 value 0.206899
    ## iter  70 value 0.172707
    ## iter  80 value 0.163049
    ## iter  90 value 0.147327
    ## iter 100 value 0.140959
    ## final  value 0.140959 
    ## stopped after 100 iterations
    ## # weights:  57 (36 variable)
    ## initial  value 52.733390 
    ## iter  10 value 35.995409
    ## iter  20 value 15.861151
    ## iter  30 value 0.576207
    ## iter  40 value 0.000678
    ## final  value 0.000083 
    ## converged
    ## # weights:  57 (36 variable)
    ## initial  value 52.733390 
    ## iter  10 value 36.046258
    ## iter  20 value 16.871323
    ## iter  30 value 9.928308
    ## iter  40 value 9.204900
    ## iter  50 value 9.191923
    ## iter  60 value 9.190441
    ## final  value 9.190439 
    ## converged
    ## # weights:  57 (36 variable)
    ## initial  value 52.733390 
    ## iter  10 value 35.995460
    ## iter  20 value 15.862227
    ## iter  30 value 0.655158
    ## iter  40 value 0.211096
    ## iter  50 value 0.192050
    ## iter  60 value 0.177847
    ## iter  70 value 0.166093
    ## iter  80 value 0.160449
    ## iter  90 value 0.156417
    ## iter 100 value 0.150502
    ## final  value 0.150502 
    ## stopped after 100 iterations
    ## # weights:  57 (36 variable)
    ## initial  value 50.536165 
    ## iter  10 value 38.787449
    ## iter  20 value 21.422711
    ## iter  30 value 3.714754
    ## iter  40 value 0.005720
    ## final  value 0.000040 
    ## converged
    ## # weights:  57 (36 variable)
    ## initial  value 50.536165 
    ## iter  10 value 38.795449
    ## iter  20 value 22.442868
    ## iter  30 value 12.393901
    ## iter  40 value 11.648164
    ## iter  50 value 11.633482
    ## iter  60 value 11.633325
    ## final  value 11.633324 
    ## converged
    ## # weights:  57 (36 variable)
    ## initial  value 50.536165 
    ## iter  10 value 38.787457
    ## iter  20 value 21.423831
    ## iter  30 value 3.764520
    ## iter  40 value 0.347435
    ## iter  50 value 0.310474
    ## iter  60 value 0.276497
    ## iter  70 value 0.238049
    ## iter  80 value 0.228295
    ## iter  90 value 0.218141
    ## iter 100 value 0.206160
    ## final  value 0.206160 
    ## stopped after 100 iterations
    ## # weights:  57 (36 variable)
    ## initial  value 50.536165 
    ## iter  10 value 38.104183
    ## iter  20 value 20.886200
    ## iter  30 value 1.070586
    ## iter  40 value 0.001857
    ## final  value 0.000057 
    ## converged
    ## # weights:  57 (36 variable)
    ## initial  value 50.536165 
    ## iter  10 value 38.128700
    ## iter  20 value 21.851847
    ## iter  30 value 10.773164
    ## iter  40 value 10.277132
    ## iter  50 value 10.257457
    ## iter  60 value 10.256835
    ## iter  70 value 10.256794
    ## iter  70 value 10.256794
    ## iter  70 value 10.256794
    ## final  value 10.256794 
    ## converged
    ## # weights:  57 (36 variable)
    ## initial  value 50.536165 
    ## iter  10 value 38.104208
    ## iter  20 value 20.887243
    ## iter  30 value 1.182876
    ## iter  40 value 0.393599
    ## iter  50 value 0.355533
    ## iter  60 value 0.288360
    ## iter  70 value 0.249055
    ## iter  80 value 0.237675
    ## iter  90 value 0.218720
    ## iter 100 value 0.208559
    ## final  value 0.208559 
    ## stopped after 100 iterations
    ## # weights:  57 (36 variable)
    ## initial  value 50.536165 
    ## iter  10 value 35.929883
    ## iter  20 value 19.535117
    ## iter  30 value 1.603697
    ## iter  40 value 0.001781
    ## final  value 0.000058 
    ## converged
    ## # weights:  57 (36 variable)
    ## initial  value 50.536165 
    ## iter  10 value 35.937854
    ## iter  20 value 20.358861
    ## iter  30 value 10.216433
    ## iter  40 value 9.622942
    ## iter  50 value 9.596821
    ## iter  60 value 9.595790
    ## final  value 9.595788 
    ## converged
    ## # weights:  57 (36 variable)
    ## initial  value 50.536165 
    ## iter  10 value 35.929890
    ## iter  20 value 19.535999
    ## iter  30 value 1.670240
    ## iter  40 value 0.306178
    ## iter  50 value 0.283569
    ## iter  60 value 0.249930
    ## iter  70 value 0.224877
    ## iter  80 value 0.213734
    ## iter  90 value 0.202595
    ## iter 100 value 0.187133
    ## final  value 0.187133 
    ## stopped after 100 iterations
    ## # weights:  57 (36 variable)
    ## initial  value 51.634778 
    ## iter  10 value 34.794834
    ## iter  20 value 21.375036
    ## iter  30 value 2.163223
    ## iter  40 value 0.002208
    ## final  value 0.000070 
    ## converged
    ## # weights:  57 (36 variable)
    ## initial  value 51.634778 
    ## iter  10 value 34.808060
    ## iter  20 value 21.837774
    ## iter  30 value 9.790977
    ## iter  40 value 8.873526
    ## iter  50 value 8.856347
    ## iter  60 value 8.854657
    ## iter  70 value 8.854579
    ## final  value 8.854573 
    ## converged
    ## # weights:  57 (36 variable)
    ## initial  value 51.634778 
    ## iter  10 value 34.794847
    ## iter  20 value 21.375742
    ## iter  30 value 2.225206
    ## iter  40 value 0.346672
    ## iter  50 value 0.290297
    ## iter  60 value 0.247679
    ## iter  70 value 0.205995
    ## iter  80 value 0.197910
    ## iter  90 value 0.185205
    ## iter 100 value 0.171404
    ## final  value 0.171404 
    ## stopped after 100 iterations
    ## # weights:  57 (36 variable)
    ## initial  value 51.634778 
    ## iter  10 value 35.867075
    ## iter  20 value 20.058666
    ## iter  30 value 0.992715
    ## iter  40 value 0.001460
    ## final  value 0.000046 
    ## converged
    ## # weights:  57 (36 variable)
    ## initial  value 51.634778 
    ## iter  10 value 35.887866
    ## iter  20 value 19.694551
    ## iter  30 value 10.112732
    ## iter  40 value 9.656549
    ## iter  50 value 9.640885
    ## iter  60 value 9.640379
    ## final  value 9.640370 
    ## converged
    ## # weights:  57 (36 variable)
    ## initial  value 51.634778 
    ## iter  10 value 35.867096
    ## iter  20 value 20.060981
    ## iter  30 value 1.050523
    ## iter  40 value 0.213885
    ## iter  50 value 0.190586
    ## iter  60 value 0.181550
    ## iter  70 value 0.167520
    ## iter  80 value 0.153377
    ## iter  90 value 0.148581
    ## iter 100 value 0.140575
    ## final  value 0.140575 
    ## stopped after 100 iterations
    ## # weights:  57 (36 variable)
    ## initial  value 51.634778 
    ## iter  10 value 36.414427
    ## iter  20 value 22.829068
    ## iter  30 value 1.043907
    ## iter  40 value 0.001447
    ## final  value 0.000081 
    ## converged
    ## # weights:  57 (36 variable)
    ## initial  value 51.634778 
    ## iter  10 value 36.424635
    ## iter  20 value 23.388951
    ## iter  30 value 10.036273
    ## iter  40 value 9.168572
    ## iter  50 value 9.106719
    ## iter  60 value 9.092532
    ## iter  70 value 9.092291
    ## final  value 9.092288 
    ## converged
    ## # weights:  57 (36 variable)
    ## initial  value 51.634778 
    ## iter  10 value 36.414437
    ## iter  20 value 22.829660
    ## iter  30 value 1.131881
    ## iter  40 value 0.291875
    ## iter  50 value 0.259589
    ## iter  60 value 0.237034
    ## iter  70 value 0.208742
    ## iter  80 value 0.200076
    ## iter  90 value 0.187175
    ## iter 100 value 0.167150
    ## final  value 0.167150 
    ## stopped after 100 iterations
    ## # weights:  57 (36 variable)
    ## initial  value 50.536165 
    ## iter  10 value 34.382037
    ## iter  20 value 18.868987
    ## iter  30 value 1.162604
    ## iter  40 value 0.001379
    ## final  value 0.000085 
    ## converged
    ## # weights:  57 (36 variable)
    ## initial  value 50.536165 
    ## iter  10 value 34.394797
    ## iter  20 value 19.542094
    ## iter  30 value 9.596335
    ## iter  40 value 9.054337
    ## iter  50 value 9.032992
    ## iter  60 value 9.032149
    ## iter  70 value 9.032127
    ## final  value 9.032125 
    ## converged
    ## # weights:  57 (36 variable)
    ## initial  value 50.536165 
    ## iter  10 value 34.382049
    ## iter  20 value 18.869696
    ## iter  30 value 1.254969
    ## iter  40 value 0.312636
    ## iter  50 value 0.287457
    ## iter  60 value 0.251521
    ## iter  70 value 0.216770
    ## iter  80 value 0.201799
    ## iter  90 value 0.190532
    ## iter 100 value 0.177518
    ## final  value 0.177518 
    ## stopped after 100 iterations
    ## # weights:  57 (36 variable)
    ## initial  value 51.634778 
    ## iter  10 value 37.831932
    ## iter  20 value 20.194364
    ## iter  30 value 1.271986
    ## iter  40 value 0.001501
    ## final  value 0.000085 
    ## converged
    ## # weights:  57 (36 variable)
    ## initial  value 51.634778 
    ## iter  10 value 37.846559
    ## iter  20 value 21.212830
    ## iter  30 value 10.242305
    ## iter  40 value 9.445483
    ## iter  50 value 9.437244
    ## iter  60 value 9.437115
    ## final  value 9.437106 
    ## converged
    ## # weights:  57 (36 variable)
    ## initial  value 51.634778 
    ## iter  10 value 37.831946
    ## iter  20 value 20.195459
    ## iter  30 value 1.344422
    ## iter  40 value 0.282832
    ## iter  50 value 0.255902
    ## iter  60 value 0.230645
    ## iter  70 value 0.188370
    ## iter  80 value 0.179283
    ## iter  90 value 0.165173
    ## iter 100 value 0.156380
    ## final  value 0.156380 
    ## stopped after 100 iterations
    ## # weights:  57 (36 variable)
    ## initial  value 51.634778 
    ## iter  10 value 38.353005
    ## iter  20 value 21.796156
    ## iter  30 value 1.484215
    ## iter  40 value 0.003921
    ## final  value 0.000025 
    ## converged
    ## # weights:  57 (36 variable)
    ## initial  value 51.634778 
    ## iter  10 value 38.368182
    ## iter  20 value 23.523741
    ## iter  30 value 11.015118
    ## iter  40 value 10.257811
    ## iter  50 value 10.194075
    ## iter  60 value 10.193316
    ## final  value 10.193296 
    ## converged
    ## # weights:  57 (36 variable)
    ## initial  value 51.634778 
    ## iter  10 value 38.353021
    ## iter  20 value 21.797974
    ## iter  30 value 1.542386
    ## iter  40 value 0.283291
    ## iter  50 value 0.257892
    ## iter  60 value 0.238287
    ## iter  70 value 0.211397
    ## iter  80 value 0.198095
    ## iter  90 value 0.186972
    ## iter 100 value 0.176410
    ## final  value 0.176410 
    ## stopped after 100 iterations
    ## # weights:  57 (36 variable)
    ## initial  value 50.536165 
    ## iter  10 value 35.173427
    ## iter  20 value 18.646383
    ## iter  30 value 0.502385
    ## iter  40 value 0.000716
    ## final  value 0.000089 
    ## converged
    ## # weights:  57 (36 variable)
    ## initial  value 50.536165 
    ## iter  10 value 35.186768
    ## iter  20 value 19.647134
    ## iter  30 value 10.312297
    ## iter  40 value 10.037188
    ## iter  50 value 10.030291
    ## iter  60 value 10.030105
    ## final  value 10.030081 
    ## converged
    ## # weights:  57 (36 variable)
    ## initial  value 50.536165 
    ## iter  10 value 35.173440
    ## iter  20 value 18.647469
    ## iter  30 value 0.623731
    ## iter  40 value 0.302374
    ## iter  50 value 0.287758
    ## iter  60 value 0.251218
    ## iter  70 value 0.218177
    ## iter  80 value 0.211867
    ## iter  90 value 0.202927
    ## iter 100 value 0.193944
    ## final  value 0.193944 
    ## stopped after 100 iterations
    ## # weights:  57 (36 variable)
    ## initial  value 50.536165 
    ## iter  10 value 34.735868
    ## iter  20 value 21.189938
    ## iter  30 value 0.845177
    ## iter  40 value 0.001233
    ## final  value 0.000079 
    ## converged
    ## # weights:  57 (36 variable)
    ## initial  value 50.536165 
    ## iter  10 value 34.754828
    ## iter  20 value 22.521222
    ## iter  30 value 9.958098
    ## iter  40 value 9.377220
    ## iter  50 value 9.353411
    ## iter  60 value 9.350594
    ## iter  70 value 9.350497
    ## final  value 9.350497 
    ## converged
    ## # weights:  57 (36 variable)
    ## initial  value 50.536165 
    ## iter  10 value 34.735887
    ## iter  20 value 21.191387
    ## iter  30 value 0.911311
    ## iter  40 value 0.230355
    ## iter  50 value 0.204858
    ## iter  60 value 0.192157
    ## iter  70 value 0.179932
    ## iter  80 value 0.177648
    ## iter  90 value 0.173523
    ## iter 100 value 0.165251
    ## final  value 0.165251 
    ## stopped after 100 iterations
    ## # weights:  57 (36 variable)
    ## initial  value 52.733390 
    ## iter  10 value 35.599027
    ## iter  20 value 18.870114
    ## iter  30 value 1.360102
    ## iter  40 value 0.002144
    ## final  value 0.000044 
    ## converged
    ## # weights:  57 (36 variable)
    ## initial  value 52.733390 
    ## iter  10 value 35.634930
    ## iter  20 value 20.008733
    ## iter  30 value 9.366801
    ## iter  40 value 8.836484
    ## iter  50 value 8.824425
    ## iter  60 value 8.823891
    ## iter  70 value 8.823875
    ## final  value 8.823874 
    ## converged
    ## # weights:  57 (36 variable)
    ## initial  value 52.733390 
    ## iter  10 value 35.599063
    ## iter  20 value 18.871329
    ## iter  30 value 1.412532
    ## iter  40 value 0.254330
    ## iter  50 value 0.228517
    ## iter  60 value 0.214676
    ## iter  70 value 0.182169
    ## iter  80 value 0.165867
    ## iter  90 value 0.159684
    ## iter 100 value 0.150481
    ## final  value 0.150481 
    ## stopped after 100 iterations
    ## # weights:  57 (36 variable)
    ## initial  value 51.634778 
    ## iter  10 value 39.276871
    ## iter  20 value 24.102167
    ## iter  30 value 1.881542
    ## iter  40 value 0.003357
    ## final  value 0.000035 
    ## converged
    ## # weights:  57 (36 variable)
    ## initial  value 51.634778 
    ## iter  10 value 39.296229
    ## iter  20 value 24.697565
    ## iter  30 value 12.056777
    ## iter  40 value 11.483836
    ## iter  50 value 11.456611
    ## iter  60 value 11.455770
    ## final  value 11.455766 
    ## converged
    ## # weights:  57 (36 variable)
    ## initial  value 51.634778 
    ## iter  10 value 39.276891
    ## iter  20 value 24.102797
    ## iter  30 value 1.956983
    ## iter  40 value 0.374275
    ## iter  50 value 0.330406
    ## iter  60 value 0.277799
    ## iter  70 value 0.240903
    ## iter  80 value 0.226464
    ## iter  90 value 0.219112
    ## iter 100 value 0.208556
    ## final  value 0.208556 
    ## stopped after 100 iterations
    ## # weights:  57 (36 variable)
    ## initial  value 51.634778 
    ## iter  10 value 37.523702
    ## iter  20 value 19.655541
    ## iter  30 value 1.131249
    ## iter  40 value 0.001515
    ## final  value 0.000051 
    ## converged
    ## # weights:  57 (36 variable)
    ## initial  value 51.634778 
    ## iter  10 value 37.533563
    ## iter  20 value 20.603430
    ## iter  30 value 10.055565
    ## iter  40 value 9.702007
    ## iter  50 value 9.686336
    ## iter  60 value 9.683582
    ## iter  70 value 9.683548
    ## final  value 9.683548 
    ## converged
    ## # weights:  57 (36 variable)
    ## initial  value 51.634778 
    ## iter  10 value 37.523711
    ## iter  20 value 19.656566
    ## iter  30 value 1.188902
    ## iter  40 value 0.239143
    ## iter  50 value 0.219037
    ## iter  60 value 0.201154
    ## iter  70 value 0.180221
    ## iter  80 value 0.168762
    ## iter  90 value 0.157219
    ## iter 100 value 0.150235
    ## final  value 0.150235 
    ## stopped after 100 iterations
    ## # weights:  57 (36 variable)
    ## initial  value 57.127839 
    ## iter  10 value 41.099737
    ## iter  20 value 22.611806
    ## iter  30 value 12.244749
    ## iter  40 value 11.726148
    ## iter  50 value 11.697630
    ## iter  60 value 11.696970
    ## final  value 11.696969 
    ## converged

``` r
# Predict on test data
log_predictions <- predict(log_model, newdata = test_data)

# Evaluate the logistic regression model
confusionMatrix(log_predictions, test_data$Winner)
```

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction 0 1 2
    ##          0 1 1 1
    ##          1 1 5 0
    ##          2 0 0 3
    ## 
    ## Overall Statistics
    ##                                           
    ##                Accuracy : 0.75            
    ##                  95% CI : (0.4281, 0.9451)
    ##     No Information Rate : 0.5             
    ##     P-Value [Acc > NIR] : 0.073           
    ##                                           
    ##                   Kappa : 0.6             
    ##                                           
    ##  Mcnemar's Test P-Value : NA              
    ## 
    ## Statistics by Class:
    ## 
    ##                      Class: 0 Class: 1 Class: 2
    ## Sensitivity           0.50000   0.8333   0.7500
    ## Specificity           0.80000   0.8333   1.0000
    ## Pos Pred Value        0.33333   0.8333   1.0000
    ## Neg Pred Value        0.88889   0.8333   0.8889
    ## Prevalence            0.16667   0.5000   0.3333
    ## Detection Rate        0.08333   0.4167   0.2500
    ## Detection Prevalence  0.25000   0.5000   0.2500
    ## Balanced Accuracy     0.65000   0.8333   0.8750

``` r
# Train SVM model
svm_model <- train(Winner ~ ., data = train_data, method = "svmLinear")

# Predict on test data
svm_predictions <- predict(svm_model, newdata = test_data)

# Evaluate the SVM model
confusionMatrix(svm_predictions, test_data$Winner)
```

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction 0 1 2
    ##          0 0 1 2
    ##          1 1 4 0
    ##          2 1 1 2
    ## 
    ## Overall Statistics
    ##                                           
    ##                Accuracy : 0.5             
    ##                  95% CI : (0.2109, 0.7891)
    ##     No Information Rate : 0.5             
    ##     P-Value [Acc > NIR] : 0.6128          
    ##                                           
    ##                   Kappa : 0.2174          
    ##                                           
    ##  Mcnemar's Test P-Value : 0.7212          
    ## 
    ## Statistics by Class:
    ## 
    ##                      Class: 0 Class: 1 Class: 2
    ## Sensitivity            0.0000   0.6667   0.5000
    ## Specificity            0.7000   0.8333   0.7500
    ## Pos Pred Value         0.0000   0.8000   0.5000
    ## Neg Pred Value         0.7778   0.7143   0.7500
    ## Prevalence             0.1667   0.5000   0.3333
    ## Detection Rate         0.0000   0.3333   0.1667
    ## Detection Prevalence   0.2500   0.4167   0.3333
    ## Balanced Accuracy      0.3500   0.7500   0.6250

``` r
1
```

    ## [1] 1

Note that the `echo = FALSE` parameter was added to the code chunk to
prevent printing of the R code that generated the plot.
