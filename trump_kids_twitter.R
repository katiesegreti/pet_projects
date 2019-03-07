library(rtweet)
library(dplyr)
library(ggplot2)
library(ggthemes)

# Don jr likes
jr_likes <- get_favorites("DonaldJTrumpJr", n = 3000)

# twitter users Jr. likes
jr_likes_ppl <- jr_likes %>%
  count(screen_name) %>%
  arrange(desc(n)) %>%
  mutate(user = "DonaldJTrumpJr")

#Ivanka likes
ivanka_likes <- get_favorites("IvankaTrump", n = 3000)

#twitter users ivanka likes
ivanka_likes_ppl <- ivanka_likes %>%
  count(screen_name) %>%
  arrange(desc(n)) %>%
  mutate(user = "IvankaTrump")

#eric likes
eric_likes <- get_favorites("EricTrump", n = 3000)

#twitter users eric likes
eric_likes_ppl <- eric_likes %>%
  count(screen_name) %>%
  arrange(desc(n)) %>%
  mutate(user = "EricTrump")

# make one dataframe for all their likes
t_kids_faves <- rbind(jr_likes_ppl[1:10,], ivanka_likes_ppl[1:10,], eric_likes_ppl[1:10,])

# chart theme
theme_tweets <- function() {
  theme_minimal() +
    theme(
      plot.subtitle = element_text(size = 10),
      plot.caption = element_text(color = "gray30"),
      plot.background = element_rect(fill = "gray95"),
      plot.title = element_text(size = 15),
      axis.text.x = element_blank()
    )
}

#Don Jr chart
jr_chart <- t_kids_faves %>%
  filter(user == "DonaldJTrumpJr") %>%
  ggplot(aes(x = fct_reorder(screen_name, n), y = n)) +
  geom_col(fill = "#15317E") +
  coord_flip() +
  theme_tweets() +
  labs(
    x = "",
    y = "",
    title = "Twitter accounts liked most by Donald Trump, Jr.",
    subtitle = "By number of their tweets he liked"
  ) +
  geom_text(
    aes(x = screen_name,
        y = n - 5,
        label = n
    ),
    size = 4,
    color = "gray95"
  )

# ivanka chart
ivanka_chart <- t_kids_faves %>%
  filter(user == "IvankaTrump") %>%
  ggplot(aes(x = fct_reorder(screen_name, n), y = n)) +
  geom_col(fill = "#FF6872") +
  coord_flip() +
  theme_tweets() +
  labs(
    x = "",
    y = "",
    title = "Twitter accounts liked most by Ivanka Trump",
    subtitle = "By number of their tweets she liked"
  ) +
  geom_text(
    aes(x = screen_name,
        y = n - 2,
        label = n
    ),
    size = 4,
    color = "gray95"
  )

# eric chart
eric_chart <- t_kids_faves %>%
  filter(user == "EricTrump") %>%
  ggplot(aes(x = fct_reorder(screen_name, n), y = n)) +
  geom_col(fill = "#494A4F") +
  coord_flip() +
  theme_tweets() +
  labs(
    x = "",
    y = "",
    title = "Twitter accounts liked most by Eric Trump",
    subtitle = "By number of their tweets he liked"
  ) +
  geom_text(
    aes(x = screen_name,
        y = n - 15,
        label = n
    ),
    size = 4,
    color = "gray95"
  )
