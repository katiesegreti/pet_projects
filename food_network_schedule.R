library(tidyverse)
library(rvest)
library(purrr)
library(forcats)
library(rebus)
library(geosphere)
library(tidyr)
library(ggbeeswarm)
library(ggthemes)
library(plotly)
library(RColorBrewer)
library(scales)
library(lubridate)

#get food network schedule for week of 3/25 - 3/31
food_network <- c()
for (k in 25:31) {
  url <- paste0('https://www.foodnetwork.com/shows/tv-schedule.2019.03.', 
                k, '.EST')
  day <- read_html(url) %>%
    html_node('.o-ProgramSchedule__a-AssetInfo') %>%
    html_text()
  weekday <- read_html(url) %>%
    html_node('.o-ProgramSchedule__a-HeadlineText') %>%
    html_text()
  food_network <- read_html(url) %>%
    html_nodes('.o-ProgramSchedule') %>%
    map_df(~list(time = html_nodes(.x, '.m-DateBlock__a-Time') %>%
                   html_text(),
                 am_pm = html_nodes(.x, '.m-DateBlock__a-Period') %>%
                   html_text(),
                 show = html_nodes(.x, '.m-MediaBlock__a-SubHeadline') %>%
                   html_text())) %>%
    mutate(day = day, weekday = weekday) %>%
    rbind(food_network, .)
}

food_network1 <- food_network %>%
  mutate(show = str_remove_all(show, "\n") %>%
           trimws(),
         weekday = str_remove_all(weekday, ",") %>%
           trimws() %>%
           as.factor(),
         guy = ifelse((str_detect(show, "Guy") | 
                         str_detect(show, "Dives")), 1, 0))

#relevel weekday so they're in order
food_network1$weekday <- fct_relevel(food_network1$weekday, "Monday", "Tuesday", "Wednesday", "Thursday",
            "Friday", "Saturday", "Sunday")

fct_relevel(food_network1$weekday)
shows <- unique(food_network1$show)

food_network1 %>%
  filter(guy == 1) %>%
  group_by(weekday) %>%
  summarize(shows = n()) %>%
  complete(weekday, fill = list(shows = 0)) %>%
  ggplot(aes(x = weekday, y = shows)) +
  geom_col(fill = "#C5083C") +
  geom_text(color = "gray95", aes(x = weekday, y = shows - 2, label = ifelse(shows > 0, shows, ""))) +
  theme_fivethirtyeight() +
  labs(
    x = "",
    y = "",
    title = "Friday is Guy Day",
    subtitle = "# of shows starring Guy Fieri on Food Network, week of 3/25/19 - 3/31/19"
  ) +
  theme(
    axis.title = element_blank(),
    #plot.background = element_rect(fill = "gray95"),
    axis.text.x = element_text(size = 10),
    plot.title = element_text(size = 18),
    plot.subtitle = element_text(size = 13)
  ) 
