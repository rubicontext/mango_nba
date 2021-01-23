#R script for NBA free throw data exploration

################################################################
##### INIT ENVIRONMENT           ###############################
################################################################

#LIBs loading
# install.packages("tidyverse")
# install.packages("DataExplorer")
# install.packages("dataQualityR")
library(dataQualityR)
library(tidyverse)
library(DataExplorer)

#INIT environment global variables
env_file_path_mango <- '/rubi1/mango_nba/'
env_file_path_mango_data <- paste0(env_file_path_mango, 'data/')
env_file_path_mango_out <- paste0(env_file_path_mango, 'output/')

#INIT business analysis parameters
env_min_shot_per_season <- 10 #a player with less than x shot in a season will not appear in hall of fame

################################################################
##### LOAD DATA                  ###############################
################################################################

#LOAD dataset
free_throws_raw <- read.csv(paste(env_file_path_mango_data, "free_throws.csv", sep=""), 
                           sep=",", header = T,  encoding = "UTF-8")

#get a glimpse of the dataset
summary(free_throws_raw)
head(free_throws_raw)
glimpse(free_throws_raw)

#Dataset dimensions
nrow(free_throws_raw)
ncol(free_throws_raw)

################################################################
##### DATA QUALITY               ###############################
################################################################

#dataQualityR package, works but we will prefer a manual approach

# file_report_num <- paste(env_file_path_mango_out, "report_num.csv", sep="")
# file_report_category <- paste(env_file_path_mango_out, "report_cat.csv", sep="")
# if(!file.exists(file_report_num)) {
#   file.create(file_report_num)
# }
# if(!file.exists(file_report_category)) {
#   file.create(file_report_category)
# }
# checkDataQuality(data= free_throws_raw, 
#                  out.file.num = file_report_num,
#                  out.file.cat = file_report_category)

#DataExplorer package works well, but we will still demonstrate below how we can assess the data quality with a manual approach
#it is usefull for a quick overview, but we need to prepare the data first

# create_report(free_throws_raw)

#data quality manual approach
#check for missing values
are_na_present <- any(is.na(free_throws_raw))
are_na_present

#get a count of missing values per column
count_na <- apply(free_throws_raw, 2, function(x) sum(is.na(x)))
count_na

#get the row(s) in error
row_na_filter <- free_throws_raw[is.na(free_throws_raw$period) | is.na(free_throws_raw$shot_made),]
row_na_filter
# end_result      game   game_id period play player playoffs score season shot_made time
# 616677   100 - 90 IND - TOR 400874380     NA                                          NA  

#We decide to filter this row(s) as we do not have sufficient information to reconstruct the data
free_throws_filter <- subset(free_throws_raw,
                            !is.na(free_throws_raw$period) &
                            !is.na(free_throws_raw$shot_made))
nrow(free_throws_raw)
nrow(free_throws_filter)

################################################################
##### PREPARE DATA FOR ANALYSIS         ########################
################################################################

#Prepare data set for analysis
free_throws_clean <- free_throws_filter
glimpse(free_throws_clean)

#Split END SCORE, compute score difference
free_throws_clean <- separate(data = free_throws_clean, col = end_result, into = c("end_score_home", "end_score_visitor"), sep = " - ")
free_throws_clean$end_score_home <- as.numeric(free_throws_clean$end_score_home)
free_throws_clean$end_score_visitor <- as.numeric(free_throws_clean$end_score_visitor)
free_throws_clean$end_score_diff <- free_throws_clean$end_score_home - free_throws_clean$end_score_visitor

#add an end score type
free_throws_clean$end_scrore_type <-"Default"
free_throws_clean$end_scrore_type[free_throws_clean$end_score_diff < 0]="Loss"
free_throws_clean$end_scrore_type[free_throws_clean$end_score_diff > 0]="Win"
free_throws_clean$end_scrore_type[free_throws_clean$end_score_diff == 0]="Tie"
free_throws_clean$end_scrore_type <- as.factor(free_throws_clean$end_scrore_type)

#Split CURRENT SCORE, compute score difference
free_throws_clean <- separate(data = free_throws_clean, col = score, into = c("score_home", "score_visitor"), sep = " - ")
free_throws_clean$score_home <- as.numeric(free_throws_clean$score_home)
free_throws_clean$score_visitor <- as.numeric(free_throws_clean$score_visitor)
free_throws_clean$score_diff <- free_throws_clean$score_home - free_throws_clean$score_visitor

#add a current score type
free_throws_clean$score_type <-"Default"
free_throws_clean$score_type[free_throws_clean$score_diff < 0]="Loss"
free_throws_clean$score_type[free_throws_clean$score_diff > 0]="Win"
free_throws_clean$score_type[free_throws_clean$score_diff == 0]="Tie"
free_throws_clean$score_type <- as.factor(free_throws_clean$score_type)

#Split TEAMS 
free_throws_clean <- separate(data = free_throws_clean, col = game, into = c("team_home", "team_visitor"), sep = " - ")

#detect total number of throws
free_throws_clean$play_type_total <- -1
free_throws_clean$play_type_total[str_detect(free_throws_clean$play, "technical free throw")
                            | str_detect(free_throws_clean$play, "makes free throw$")
                            | str_detect(free_throws_clean$play, "makes flagrant free throw$")
                            | str_detect(free_throws_clean$play, "misses free throw$")]=1
free_throws_clean$play_type_total[str_detect(free_throws_clean$play, "of 1")]=1
free_throws_clean$play_type_total[str_detect(free_throws_clean$play, "of 2")]=2
free_throws_clean$play_type_total[str_detect(free_throws_clean$play, "of 3")]=3

#check if we have other cases
free_throws_clean$play[free_throws_clean$play_type_total ==-1]
#2 line are incorrect, as they concern 2 free throws, but the play count is only one!
#[1] "Jason Maxiell makes 2 free throws" "Joe Johnson makes two free throws"
#We would need to duplicate these lines to reflect the reality, and keep only 2 values for shot_made 0/1
free_throws_clean$play_type_total[str_detect(free_throws_clean$play, "makes 2 free throws$")
                            | str_detect(free_throws_clean$play, "makes two free throws$")]=2
free_throws_clean$play_type_count[str_detect(free_throws_clean$play, "makes 2 free throws$")
                            | str_detect(free_throws_clean$play, "makes two free throws$")]=1
rows_to_duplicate <- free_throws_clean[str_detect(free_throws_clean$play, "makes 2 free throws$")
                                  | str_detect(free_throws_clean$play, "makes two free throws$"),]
rows_to_duplicate$play_type_count <- 2
free_throws_clean <- rbind(free_throws_clean, rows_to_duplicate)

#Detect current throw number out of total
free_throws_clean$play_type_count <- -1
free_throws_clean$play_type_count[str_detect(free_throws_clean$play, "misses technical free throw")
                            | str_detect(free_throws_clean$play, "misses free throw")
                            | str_detect(free_throws_clean$play, "makes free throw$") 
                            | str_detect(free_throws_clean$play, "makes flagrant free throw$")]=1
free_throws_clean$play_type_count[str_detect(free_throws_clean$play, "makes technical free throw")]=1
free_throws_clean$play_type_count[str_detect(free_throws_clean$play, "1 of")]=1
free_throws_clean$play_type_count[str_detect(free_throws_clean$play, "2 of")
                            | str_detect(free_throws_clean$play, "makes 2 free throws$")
                            | str_detect(free_throws_clean$play, "makes two free throws$")]=2
free_throws_clean$play_type_count[str_detect(free_throws_clean$play, "3 of")]=3

#check if we have other cases
free_throws_clean$play[free_throws_clean$play_type_count ==-1]

#check draws
# nrow(free_throws_clean[,free_throws_clean$end_score_diff ==0])
#avergage score diff
# mean(free_throws_clean$end_score_diff)

#factorize seasons
free_throws_clean$season <- as.factor(free_throws_clean$season)
#discretion of time variable? NOT DONE YET, we have the period for now

#check period values, we have values >4 (8)
#
summary(free_throws_clean$period)

glimpse(free_throws_clean)

################################################################
##### DATA ANALYSIS              ###############################
################################################################

#First we select only the features that may be used in a medel to explain the shot made
# free_throws <- free_throws_clean
# free_throws <- free_throws[,c("shot_made", "time", "period", "player", "playoffs", "season", "score_type", "play_type_total", "play_type_count")]

#a bit more elegant with dplyr
free_throws <- free_throws_clean  %>%
  select(shot_made, time, period, player, playoffs, season, score_type)

glimpse(free_throws)

#we want to select the best players for each season
#env_min_shot_per_season : we have few 100% success rate on very few shots. This is not very helpful so we filter nb shots total < parameters

best_players_by_season <- free_throws %>% 
  group_by(player, season) %>% 
  summarise(total_shot=n(), total_shot_made=sum(shot_made)) %>% 
  mutate(percentage_made=total_shot_made/total_shot*100) %>% 
  filter(total_shot > env_min_shot_per_season) %>%
  top_n(10, percentage_made) %>% 
  arrange(desc(percentage_made))
best_players_by_season


#we want to select the best players for most recent season
best_players_last_seasons <- free_throws %>% 
  group_by(player, season) %>% 
  summarise(total_shot=n(), total_shot_made=sum(shot_made)) %>% 
  mutate(percentage_made=total_shot_made/total_shot*100) %>% 
  filter(total_shot > env_min_shot_per_season, season=="2015 - 2016") %>%
  top_n(20, percentage_made) %>% 
  arrange(desc(percentage_made)) %>%
  select(c(player, percentage_made, total_shot_made, total_shot))
best_players_last_seasons

#we want to select the best time to throw
#first by period
best_period_to_throw <- free_throws %>% 
  group_by(period) %>% 
  summarise(total_shot=n(), total_shot_made=sum(shot_made)) %>% 
  mutate(percentage_made=total_shot_made/total_shot*100)
best_period_to_throw
plot_bar(best_period_to_throw)
#we want to select the best period and time to score

myplot <- ggplot(best_period_to_throw) + 
  geom_bar(aes(y = percentage_made))

myplot


#data explorer package
# create_report(free_throws)
#we want to predict if the throw is made or not, auto report for exploration
create_report(free_throws, y="shot_made")

#we want to predict if the game is won or not
#WE ARE MISSING THE team of the player! This would be very interesting to add tot the study
# create_report(free_throws, y="end_score_type")


################################################################
##### WITH a little MORE TIME / BUDGET   #######################
################################################################

# -Add player career length at the time of the game to improve recruiting strat
# -Add player height and age
# -Add player time on the court for this game to predict best rotation strategy
# -Add player current team for each game, to set if he is in the home or visitor team. This would help a lot and I suspect the current score difference and type (winning/losing) could explain the success rate!
# -Add coaches name and coaching style to detect best coaches and coaching strategies
# -Shiny + Highcharts plots for professional and dynamic rendering

