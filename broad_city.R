

broad_url <- "https://www.imdb.com/title/tt2578560/episodes?season=1"

broad_city1 <- read_html(broad_url) %>%
  html_nodes('.info') %>%
  map_df(~list(airdate = html_nodes(.x, '.airdate') %>%
                 html_text(),
               title = html_nodes(.x, 'strong a') %>%
                 html_attr("title"),
               summary = html_nodes(.x, '.item_description') %>%
                 html_text()))

read_html(broad_url) %>%
  #html_nodes('.info') %>%
  html_nodes('.info .airdate') %>%
  html_text()


broad_city <- read_html(broad_url) %>%
  html_nodes('.info') %>%
  map_df(~list(airdate = html_nodes(.x, '.airdate') %>%
                 html_text(),
               title = html_nodes(.x, 'strong a') %>%
                 html_attr("title")))

broad_city3 <- read_html(broad_url) %>%
  html_nodes('.info strong a') %>%
                 html_text()
