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
library(httr)

ion_url <- "https://iontelevision.com/api/v1/programs?date=2019-04-19"

ion_1 <- GET(ion_url)
ion_parsed <- content(ion_1, "parsed")

ion_sched <- data.frame(date = 0, start_time = 0, show = 0,
                        ep_number = 0)
for(j in 1:length(ion_parsed$programs$data)) {
  #ion_sched$date[j] = ymd("2019-04-19")
  ion_sched$show[j] = ion_parsed$programs$data[[j]]$name
}


length(ion_parsed$programs$data[[2]]$name)
