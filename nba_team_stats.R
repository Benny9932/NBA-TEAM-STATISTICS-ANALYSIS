library(ggplot2)
#################################### 
#Getting a general look at the data

head(nba_team_stats)
dim(nba_team_stats) 
colnames(nba_team_stats)


length(unique(nba_team_stats$SEASON))
# [1] 21, there are 21 variables which means there are 21 seasons of data in this data set

summary(nba_team_stats)

# Stats for points per game 
summary(nba_team_stats$PTS)
sd(nba_team_stats$PTS) # [1] 6.566727

# Stats for 3 pointers attempted per game 
summary(nba_team_stats$'3PA')
sd(nba_team_stats$'3PA') # [1] 7.460323

# Stats for assists per game
summary(nba_team_stats$AST)
sd(nba_team_stats$AST) # [1] 2.093977

# Stats for rebounds per game
summary(nba_team_stats$REB)
sd(nba_team_stats$REB) # [1] 2.06563


# Looking at standard deviation for every numerical variable
nba_team_stats_col = 2:25
nba_teams_sd = apply(nba_team_stats[nba_team_stats_col], 2, sd)
nba_teams_sd

# TOP 2 SD
#PTS = 6.5667271, 3PA = 7.4603226
# The high standard deviation indicates that there are a large range of values and be a point of interest to look further at later on alongside with seasons.


# Teams had either changed names/new team added/relocated

is.factor(nba_team_stats$TEAM)
nba_team_stats$TEAM = as.factor(nba_team_stats$TEAM)
all_teams = summary(nba_team_stats$TEAM)
older_teams = all_teams[which(all_teams == 21)]
newer_teams = all_teams[which(all_teams < 21)]
print(older_teams)
print(newer_teams)

# By looking at the amount of times that specific teams has appeared throughout the data set you can tell which teams have been around for the full 21 years and
# also see which teams are newer due to name changes/moving cities/new team.


####################################
# ANALYZING VARIABLES TOGETHER

is.factor(nba_team_stats$SEASON)
nba_team_stats$SEASON = as.factor(nba_team_stats$SEASON)

# Looking at the average points per season 
mean_pts_per_season = aggregate(PTS ~ SEASON, data = nba_team_stats, mean)
mean_pts_per_season

ggplot(mean_pts_per_season, aes(x = SEASON, y = PTS)) +
  geom_bar(stat = "identity", fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Average PTS Over Seasons",
       x = "Season",
       y = "Average PTS") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ylim(0,180)

# Mean 3 point attempts per season
mean_3pa_per_season = aggregate(`3PA` ~ SEASON, data = nba_team_stats, mean)

ggplot(mean_3pa_per_season, aes(x = SEASON, y = `3PA`)) +
  geom_bar(stat = "identity", fill = "red", color = "black", alpha = 0.7) +
  labs(title = "Average 3PA Over Seasons",
       x = "Season",
       y = "Average 3PA") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# From the first graph we can see that there wasn't a significant change in the amount of points scored per game.
# Conversely we can see with the 3pa bar graph that the amount of 3 point shots had drastically changed over time.



# Looking at the correlation between points and 3 point attempts
cor(nba_team_stats$PTS, nba_team_stats$'3PA') # [1] 0.7932252

# Creating a scatter plot between points and 3 point attempts
ggplot(nba_team_stats, aes(x = PTS, y = `3PA`)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +  
  labs(title = "Scatter Plot of PTS and 3PA with Trend Line",
       x = "PTS",
       y = "3PA") +
  theme_minimal()

# Looking at the correlation between points and assists
cor(nba_team_stats$PTS, nba_team_stats$AST) # [1] 0.6252684

# Creating a scatter plot between points and assists
ggplot(nba_team_stats, aes(x = PTS, y = AST)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +  
  labs(title = "Scatter Plot of PTS and AST with Trend Line",
       x = "PTS",
       y = "AST") +
  theme_minimal()


# Looking at the correlation between points and defensive rebounds
cor(nba_team_stats$PTS, nba_team_stats$DREB) # [1] 0.7254422

# Creating a scatter plot between points and defensive rebounds
ggplot(nba_team_stats, aes(x = PTS, y = DREB)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +  
  labs(title = "Scatter Plot of PTS and DREB with Trend Line",
       x = "PTS",
       y = "DREB") +
  theme_minimal()

# Taking a look at the correlation between every variable for points.
nba_team_stats_col = 2:26
nba_team_stats$SEASON = as.numeric(nba_team_stats$SEASON)
numeric_col = nba_team_stats[nba_team_stats_col]
nba_teams_cor_pts = cor(numeric_col$PTS, numeric_col)
nba_teams_cor_pts



# When looking at these scatter plots you can see that there are obvious variables that would increase the amount of points scored like 3PM and AST's,
# on the flip side it is also interesting to see something like DREB that isn't directly related to scoring have such a high correlation.




# Taking a look at the correlation with Wins, nothing particularly outstanding,
# but it makes sense for FG% and 3P% to be higher than others as scoring more = scoring more
nba_teams_cor_win = cor(numeric_col$W, numeric_col)
nba_teams_cor_win


# Taking a different look at Win% by season

season21 = numeric_col[numeric_col$SEASON == 21,]
season16 = numeric_col[numeric_col$SEASON == 16,]
season11 = numeric_col[numeric_col$SEASON == 11,]
season6 = numeric_col[numeric_col$SEASON == 6,]
season1 = numeric_col[numeric_col$SEASON == 1,]

cor(season21$`WIN%`,season21)
cor(season16$`WIN%`,season16)
cor(season11$`WIN%`,season11)
cor(season6$`WIN%`,season6)
cor(season1$`WIN%`,season1)

# Looking at which teams have won the most championships over the 20 year span
champions = nba_team_stats[nba_team_stats$CHAMPION == 1,]
champions

ggplot(champions, aes(x = TEAM, fill = TEAM)) +
  geom_bar(width = 0.8, color = "white") +
  theme_minimal() +
  labs(title = "Championships Won", x = "Team", y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


###############################################################################

# General look at linear regression model for WINS and 3PA

nba_team_stats$SEASON = as.numeric(nba_team_stats$SEASON)

lm.win = lm(`W` ~ `3P%`, data = nba_team_stats)
lm.win

summary(lm.win)$r.squared # 0.2341054

ggplot(nba_team_stats, aes(x = `3P%`, y = W)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(x = "3P%",
       y = "Wins (W)",
       title = "Linear Regression of All Seasons: Wins ~ 3P%") +
  theme_minimal()

# Linear regression models for the first 10 seasons & graph 

first10 = nba_team_stats[nba_team_stats$SEASON < 11,]

ggplot(first10, aes(x = `3P%`, y = W)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(x = "3P%",
       y = "Wins (W)",
       title = "Linear Regression of 2001-2010 Seasons: Wins ~ 3P%") +
  theme_minimal()

lm.win_first10_single = lm(`W` ~ `3P%`, data = first10)
lm.win_first10_single
summary(lm.win_first10_single)$r.squared # 0.1880091

lm.win_first10 = lm(`W` ~ `3P%`+ DREB + AST + BLK, data = first10)
lm.win_first10
summary(lm.win_first10)$r.squared # 0.413431

# Linear regression model for Latest 10 seasons with WINS and 3PA, later on adding more variables

last10 = nba_team_stats[nba_team_stats$SEASON >= 11,]

ggplot(last10, aes(x = `3P%`, y = W)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(x = "3P%",
       y = "Wins (W)",
       title = "Linear Regression of 2011-2021 Seasons: Wins ~ 3P%") +
  theme_minimal()

lm.win_last10_single = lm(`W` ~ `3P%`, data = last10)
lm.win_last10_single
summary(lm.win_last10_single)$r.squared # 0.2977682

lm.win_last10 = lm(`W` ~ `3P%` + DREB + AST + BLK, data = last10)
lm.win_last10
summary(lm.win_last10)$r.squared # 0.368893


# With the multiple linear regression model we can see that the r^2 value for the latest 10 seasons is lower while the first 10 seasons is higher
# this could be attributed to teams in the first 10 seasons not being as reliant on 3 pointers to get more wins.


