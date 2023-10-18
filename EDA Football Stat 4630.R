require(nflreadr)
require(nflfastR)
require(ggplot2)
require(dplyr)
require(tidyverse)
require(car)

df1 <- load_pbp(2020:2021)

group <- df1 %>%
  group_by(play_type) %>%
  summarise(avg = mean(yards_gained))

df2 <- group[c(5,9), ]

ggplot(df2, aes(play_type, avg)) + geom_col(fill = "blue", width = .7) +        
  theme_classic() +
  ggtitle("Average yards gained by Play Type")+
  labs(x = "Play Type",
       y = "Average Yards Gained")+
  theme(axis.title = element_text(size = 12, face = "bold"),
        axis.text = element_text(size = 10),
        plot.title = element_text(size = 15, hjust = 0.5),
        plot.subtitle = element_text(size = 15, hjust= 0.5))

df1$yards_gained <- ifelse(is.na(df1$yards_gained), 0, df1$yards_gained)
total_yards <- df1 %>%
  group_by(posteam) %>%
  summarise(sum = sum(yards_gained))
total_yards <- total_yards[-33,]

ggplot(total_yards, aes(posteam, sum)) + geom_col(fill = "blue", width = .7) +        
  theme_classic() +
  ggtitle("Average yards gained by Play Type")+
  labs(x = "Play Type",
       y = "Average Yards Gained")+
  theme(axis.title = element_text(size = 12, face = "bold"),
        axis.text = element_text(size = 10),
        plot.title = element_text(size = 15, hjust = 0.5),
        plot.subtitle = element_text(size = 15, hjust= 0.5))

only_run_or_pass <- subset(df1, play_type == "pass" | play_type == "run")

ggplot(only_run_or_pass, aes(x = yards_gained, color = play_type)) +
  geom_density() + xlim(-10,50)

fourth_downs <- subset(only_run_or_pass, down == 4)
fourth_downs$one <- 1
total_4_atts <- fourth_downs %>%
  group_by(home_team) %>%
  summarise(atts = sum(one))


ggplot(total_4_atts, aes(home_team, atts)) + geom_col(fill = "blue", width = .7) +        
  theme_classic()

ggplot(df2, aes(play_type, avg)) + geom_col(fill = "blue", width = .7) +        
  theme_classic() +
  ggtitle("Average yards gained by Play Type")+
  labs(x = "Play Type",
       y = "Average Yards Gained")+
  theme(axis.title = element_text(size = 12, face = "bold"),
        axis.text = element_text(size = 10),
        plot.title = element_text(size = 15, hjust = 0.5),
        plot.subtitle = element_text(size = 15, hjust= 0.5))


















