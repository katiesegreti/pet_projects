library(rvest)
library(purrr)
library(tidyr)
library(ggthemes)
library(dplyr)
library(stringr)
library(ggplot2)
library(lubridate)


#scrape episode data for seasons 1-5 of Broad City
bc_episodes <- c()
for(k in 1:5){
  url <- paste0("https://www.imdb.com/title/tt2578560/episodes?season=", k)
  bc_html <- read_html(url)
  episode_number <- bc_html %>%
    html_nodes('.image') %>%
    html_text()
  bc_episodes <- bc_html %>%
    html_nodes('.info') %>%
    map_df(~list(airdate = html_nodes(.x, '.airdate') %>%
                   html_text(),
                 title = html_nodes(.x, 'strong a') %>%
                   html_attr("title"),
                 summary = html_nodes(.x, '.item_description') %>%
                   html_text(),
                 rating = html_nodes(.x, '.ipl-rating-star.small span.ipl-rating-star__rating') %>%
                   html_text() %>% 
                   {if(length(.) == 0){NA} else {.}},
                 votes = html_nodes(.x, '.ipl-rating-star__total-votes') %>%
                   html_text() %>% 
                   {if(length(.) == 0){NA} else {.}})) %>%
    mutate(episode_number = episode_number) %>%
    rbind(bc_episodes, .)
}

#Cleaning
broad_city <- bc_episodes %>%
  mutate(airdate = dmy(airdate),
         rating = as.numeric(rating),
         votes = as.numeric(str_remove_all(votes, "[()]")),
         episode_number = str_remove_all(episode_number, "\\n") %>%
           trimws() %>%
           str_remove("Add Image ")) %>%
  separate(episode_number, c("season", "episode"), ",") %>%
  mutate(season = as.numeric(str_remove(season, "S")),
         episode = as.numeric(str_remove(episode, "Ep")),
         total_episode = row_number(),
         season = paste0("season ", season))

#background color for plot
bg_color <- "gray95"
#make a ratings plot
broad_city %>%
  filter(!is.na(rating)) %>%
  ggplot(aes(x = as.factor(episode), y = rating, fill = season, size = votes)) +
  geom_point(shape = 22) +
  geom_text(aes(label = title), size = 3, angle = -90, hjust = 0, nudge_y = -0.08) +
  facet_grid(.~season) +
  scale_fill_manual(values = c("#EE3BBE", "#FEEA17", "#009ACF", "#FEAB52", "#01D0B6")) +
  theme_wsj() +
  theme(
    panel.background = element_rect(fill = bg_color),
    plot.background = element_rect(fill = bg_color),
    strip.background = element_rect(fill = bg_color),
    axis.text.x = element_blank(),
    legend.position = "none",
    axis.line.x.bottom = element_blank(),
    axis.ticks.x.bottom = element_blank(),
    panel.grid.major.y = element_line( colour = "darkgray"),
    plot.subtitle = element_text(size = 15),
    plot.title = element_text(size = 25)
  ) +
  labs(
    x = "",
    y = "",
    
    
    title = "Broad City episodes ranked by IMDB",
    subtitle = "Larger squares indicate more votes"
  ) +
  ylim(6.0, 10.0)

#summaries for text mining
show_summaries <- broad_city$summary %>%
  str_remove_all("\\n") %>%
  trimws()
# Make a vector source: bc_source
bc_source <- VectorSource(show_summaries)
# Make a volatile corpus: bc_corpus
bc_corpus <- VCorpus(bc_source)


bc_corpus[[15]][1]


#ok for now let's count the mentions of each character in the summaries

bc_characters <- broad_city %>%
  mutate(ilanaz = str_count(summary, "Ilana"),
         abbiz = str_count(summary, "Abbi"),
         beversz = str_count(summary, "Bevers"),
         lincolnz = str_count(summary, "Lincoln"),
         jaimez = str_count(summary, "Jaime"),
         treyz = str_count(summary, "Trey"),
         jeremyz = str_count(summary, "Jeremy"),
         eliotz = str_count(summary, "Eliot")) %>%
  summarize(Ilana = sum(ilanaz),
            Abbi = sum(abbiz),
            Bevers = sum(beversz),
            Lincoln = sum(lincolnz),
            Jaime = sum(jaimez),
            Trey = sum(treyz),
            Eliot = sum(eliotz),
            Jeremy = sum(jeremyz)) %>%
  t() 

#add column names
bc_characters <- data.frame(names = row.names(bc_characters), bc_characters)
colnames(bc_characters) = c("character", "mentions")

#column chart for character mentions
ggplot(bc_characters, aes(x = reorder(character, -mentions), y = mentions, fill = character)) +
  geom_col() +
  geom_text(aes(label = mentions,
                y = ifelse(mentions > 50, mentions - 2, mentions + 2))) +
  scale_fill_manual(values = c("#EE3BBE",  "#009ACF", "#FEAB52",  "#01D0B6",
                               "#EE3BBE", "#FEEA17", "#FEEA17", "#009ACF")) +
  theme_wsj() +
  theme(
    panel.background = element_rect(fill = bg_color),
    plot.background = element_rect(fill = bg_color),
    legend.position = "none",
    axis.line.x.bottom = element_blank(),
    axis.ticks.x.bottom = element_blank(),
    panel.grid.major.y = element_line( colour = "darkgray"),
    plot.subtitle = element_text(size = 15),
    plot.title = element_text(size = 18)
  ) +
  labs(
    title = "Broad City characters mentioned in episode summaries",
    subtitle = "Number of mentions for each character"
  )
