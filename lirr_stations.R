library(tidyverse)
library(rvest)
library(purrr)
library(forcats)
library(rebus)
library(geosphere)
library(tidyr)

#url for list of stations
lirr_url_0 <- "http://lirr42.mta.info/stations.php"
#get list of all station urls
stations <- c(read_html(lirr_url_0) %>%
                html_nodes('#leftcolumn a') %>%
                html_attr("href"),
              read_html(lirr_url_0) %>%
                html_nodes('#middlecolumn a') %>%
                html_attr("href"),
              read_html(lirr_url_0) %>%
                html_nodes('#middlecolumn2 a') %>%
                html_attr("href")) %>%
  str_replace_na() 

# remove the fucking MEADOWLANDS
stations <- str_remove(stations, "stationInfo.php\\?id\\=508")
#check for NAs and non stations
is_station <- str_detect(stations, "stationInfo")
stations <- stations[is_station]


lirr_url <- "http://lirr42.mta.info/"

lirr_stations <- c()
for (k in 1:length(stations)) {
  url <- paste0(lirr_url, stations[k])
  lirr_stations <- read_html(url) %>%
    html_nodes('#contentbox') %>%
    map_df(~list(station = html_nodes(.x, 'h1') %>%
                   html_text(),
                 branch = html_nodes(.x, 'h2') %>%
                   html_text() %>%
                   str_remove_all("\n") %>%
                   str_remove_all("\t") %>%
                   str_remove_all("\r") %>%
                   str_trim(),
                 location = html_nodes(.x, 'iframe') %>%
                   html_attr("src"))) %>%
    rbind(lirr_stations, .)
}
#pattern to extract the fare zone
fare <- "Fare Zone " %R% one_or_more(DGT)
#pattern to keep just the branch
branch_pattern <- ".*Branch"

lirr_stations1 <- lirr_stations %>%
  mutate(fare_zone = str_extract(branch, fare) %>%
           str_remove("Fare Zone ") %>%
           as.numeric(),
         branch = str_extract(branch, branch_pattern)) %>%
  filter(!is.na(branch))

# GET FARES (this didn't work)
# fare_url <- "http://web.mta.info/lirr/about/TicketInfo/Fares03-19-17.htm"
# 
# fare_table <- read_html(fare_url) %>%
#   html_node('table') %>%
#   html_table(header = TRUE)

#create fares dataframe
zone <- sort(unique(lirr_stations1$fare_zone))
monthly_ticket <- c(190, 226, 261, 297, 350, 391, 461, 500)
monthly_fares <- data.frame(zone, monthly_ticket)

#join fares to stations
lirr_stations1 <- lirr_stations1 %>%
  left_join(monthly_fares, by = c(fare_zone = "zone"))

#patterns for getting latitude and longitude
lat_lon1 <- "ll=" %R% one_or_more(DGT) %R% "." %R% one_or_more(DGT) %R% ",-" %R% one_or_more(DGT) %R% "." %R% one_or_more(DGT)
lat_lon2 <- "2d-" %R% one_or_more(DGT) %R% "." %R% one_or_more(DGT) %R% "!3d" %R% one_or_more(DGT) %R% "." %R% one_or_more(DGT)

#get latitude and longitude from the map url
lirr_stations1 <- lirr_stations1 %>%
  mutate(location_coords = ifelse(str_detect(location, "maps.google"),
                                  str_extract(location, lat_lon1),
                                  str_extract(location, lat_lon2)) %>%
           str_remove("ll=") %>%
           str_remove("2d") %>%
           str_remove("3d") %>%
           str_replace("!", ",")) 

#assign penn station latitude and longitude
penn_lat <- 40.750053
penn_long <- -73.992362

#separate latitude and longitude
