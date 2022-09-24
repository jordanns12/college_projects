library(dplyr)
library(tidyr)
library(stringr)

#Read Data
okc_shots = read.csv("shots_data.csv")

#Get shot type and distribution

okc_shot_type = okc_shots %>% mutate(fg_type = case_when( y > 7.8 & sqrt((abs(x)^2 + abs(y)^2)) > 23.75 ~ "Non_corner_3", y <= 7.8 & sqrt((abs(x)^2 + abs(y)^2)) > 22.0 ~ "Corner_3",TRUE ~ "Two_pt"))
okc_shot_dist = okc_shot_type %>% group_by(team, fg_type) %>% summarise(total = n(), fg_made = sum(fgmade)) %>% mutate(distribution = (total / sum(total)) * 100)

#Separate teams

team_A_stats = okc_shot_dist %>% filter(team == "Team A")
team_B_stats = okc_shot_dist %>% filter(team == "Team B")

#Calculate 3 pt made and fg efficiency percent for each shot type 

fg3_made = okc_shot_dist %>% filter(fg_type == "Corner_3" | fg_type == "Non_corner_3") %>% group_by(team) %>% summarise(fg3m = sum(fg_made))

team_A = c(fg3_made$fg3m[1])
team_B = c(fg3_made$fg3m[2])
team_A_stats = team_A_stats %>% mutate(efg = ((fg_made + (.5 * team_A)) / total) * 100)
team_B_stats = team_B_stats %>% mutate(efg = ((fg_made + (.5 * team_B)) / total) * 100)

