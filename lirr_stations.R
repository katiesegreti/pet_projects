library(tidyverse)
library(rvest)
library(purrr)
library(lubridate)
library(forcats)

lirr_url <- "http://lirr42.mta.info/stations.php"


stations <- c(read_html(lirr_url) %>%
                html_nodes('#leftcolumn a') %>%
                html_attr("href"),
              read_html(lirr_url) %>%
                html_nodes('#middlecolumn a') %>%
                html_attr("href"),
              read_html(lirr_url) %>%
                html_nodes('#middlecolumn2 a') %>%
                html_attr("href")) %>%
  str_replace_na() 

is_station <- str_detect(stations, "stationInfo")
stations <- stations[is_station]

lynbrook_url <- "http://lirr42.mta.info/stationInfo.php?id=100"

read_html(lynbrook_url) %>%
  html_nodes('#contentbox h1') %>%
  html_text()

read_html(lynbrook_url) %>%
  html_nodes('#contentbox h2') %>%
  html_text() %>%
  str_remove_all("\n") %>%
  str_remove_all("\t") %>%
  str_remove_all("\r")

read_html(lynbrook_url) %>%
  html_nodes('#contentbox iframe') %>%
  html_attr("src")
