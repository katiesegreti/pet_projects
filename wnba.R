library(tidyverse)
library(rvest)
library(purrr)
library(forcats)
library(rebus)
library(tidyr)
library(ggthemes)
library(plotly)
library(RColorBrewer)
library(scales)
library(lubridate)
library(readr)
library(janitor)
library(snakecase)
library(stringr)

wnba_2018 <- read_csv("wnba_2018.csv")
wnba_2018 <- clean_names(wnba_2018)

# wnba url
wnba_url <- "https://www.wnba.com/stats/player-stats/"

# get all the player names in snake case (but with - not _) for player urls
players <- wnba_2018$player %>%
  str_remove_all("\'") %>%
  tolower() %>%
  str_replace_all(" ", "-") %>%
  str_replace_all("á", "a") %>%
  str_replace_all("ü", "u")

# get player id from their player pages
player_ids <- c()
for (k in 1:length(players)) {
  url <- paste0("https://www.wnba.com/player/", players[k], "/#/stats")
  player_id <- read_html(url) %>%
    html_nodes('.player-profile') %>%
    html_attr("data-playerid")
  player_ids <- c(player_ids, player_id)
}

# bind player ids and snake case names to wnba_2018
wnba_2018 <- wnba_2018 %>%
  mutate(player_id = player_ids,
         snake_name = players)

#save the new one to the file so you don't have to extract ids every time
write_csv(wnba_2018, "wnba_2018_1.csv")


# data to get from player pages
# team
#dob
# height
# weight
# position
# college

wnba_2018 <- wnba_2018 %>%
  mutate(team = 0,
         dob = 0,
         height = 0,
         weight = 0,
         position = 0,
         college = 0)


library(httr)

wnba_url1 <- "https://www.wnba.com/player/aja-wilson/#/stats"
wnba_url <- "https://a.data.nba.com/wnba/player/1628932"



w <-GET(wnba_url)
wnba_js <- content(w, "parsed")

# loop throught all players to get their details
for(j in 1:length(players)) {
  p_url <- paste0("https://a.data.nba.com/wnba/player/", wnba_2018$player_id[j])
  p <- GET(p_url)
  details <- content(p, "parsed")
  wnba_2018$team[j] <- ifelse(length(details$data$info$ta)>0, details$data$info$ta, NA)
  wnba_2018$dob[j] <- ifelse(length(details$data$info$dob)>0, details$data$info$dob, NA)
  wnba_2018$height[j] <- ifelse(length(details$data$info$ht)>0, details$data$info$ht, NA)
  wnba_2018$weight[j] <- ifelse(length(details$data$info$wt)>0, details$data$info$wt, NA)
  wnba_2018$position[j] <- ifelse(length(details$data$info$pos)>0, details$data$info$pos, NA)
  wnba_2018$college[j] <- ifelse(length(details$data$info$hcc)>0, details$data$info$hcc, NA)
}

#save this again!!!
write_csv(wnba_2018, "wnba_2018_2.csv")


# cleaning - make dob a date, convert height to inches, etc.
wnba_2018 <- wnba_2018 %>%
  separate(height, c("feet", "inches"), "-") %>%
  mutate(feet = as.numeric(feet),
         inches = as.numeric(inches),
         height = feet * 12 + inches,
         dob = ymd(dob),
         college = str_remove(college, "/USA"))


#save this again!!!
write_csv(wnba_2018, "wnba_2018_3.csv")

#totals by college / country
colleges <- wnba_2018 %>%
  filter(college != "") %>%
  group_by(college) %>%
  summarize(total = n()) %>%
  arrange(desc(total))

wnba_2018 %>%
  group_by(team) %>%
  summarize(total = n())

colleges %>%
  filter(total > 3) %>%
  mutate(college = str_replace(college, " ", "\n")) %>%
  ggplot(aes(x = reorder(college, - total), y = total)) +
  geom_col(fill = "#F37C20") +
  geom_text(aes(label = total, y = total - 1), color = "white") +
  a_theme  +
  labs(
    x = "",
    y = "",
    title = "Most common alma maters of active WNBA players"
  )

bg_color <- "gray95"
a_theme <- theme_wsj() +
  theme(
    panel.background = element_rect(fill = bg_color),
    plot.background = element_rect(fill = bg_color),
    legend.position = "none",
    axis.line.x.bottom = element_blank(),
    axis.ticks.x.bottom = element_blank(),
    axis.text.x = element_text(face = "plain"),
    panel.grid.major.y = element_line( colour = "darkgray"),
    plot.subtitle = element_text(size = 15),
    plot.title = element_text(size = 18)
  )


# average age by team
age_by_team <- wnba_2018 %>%
  mutate(age = as.numeric((today() - dob) / 365)) %>%
  group_by(team) %>%
  summarize(avg_age = mean(age))

age_by_team %>%
  ggplot(aes(x = reorder(team, -avg_age), y = avg_age)) +
  geom_col(fill = "#F37C20") +
  geom_text(aes(label = round(avg_age, 1), y = avg_age - 1), color = "white") +
  a_theme +
  labs(
    x = "",
    y = "",
    title = "Average age of WNBA players by team"
  )
 
# rebounds by height
wnba_2018 %>%
  filter(g > 15 & ft_percent > 0) %>%
  ggplot(aes(x = ft_percent, y = height)) +
  geom_point()
  
