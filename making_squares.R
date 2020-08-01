library(tidyverse)
library(lubridate)
library(dplyr)
library(stringr)
library(formattable)
library(data.table)


#create df of squares made per day by color
#26, 5, 2 from 5/1 to 5/13
squares_per_day <- data.frame(date = as.Date(1:60, origin="2020-05-01"), 
                              turquoise = 0, 
                              yellow = 0, 
                              black = 0)
as_date("2020-05-11") - as_date("2020-05-02")
26 / 8

squares_per_day <- squares_per_day %>%
  mutate(turquoise = case_when(date == "2020-05-13" ~ 2,
                               date == "2020-05-14" ~ 4,
                               TRUE ~ turquoise),
         yellow = case_when(date < "2020-05-10" ~ 3,
                            date == "2020-05-11" ~ 2,
                            TRUE ~ yellow),
         black = case_when(date == "2020-05-02" ~ 1,
                           date == "2020-05-12" ~ 4,
                           date == "2020-05-14" ~ 1,
                            TRUE ~ black))


squares_per_day %>%
  summarize(turquoise_total = sum(turquoise),
            yellow_total = sum(yellow),
            black_total = sum(black),
            all = turquoise_total + yellow_total + black_total)



#write a function to add to squares df every day with # of squares made
add_squares <- function(d, t, y, b) {
  squares_per_day %>% 
    mutate(turquoise = case_when(date == d ~ t,
                                 TRUE ~ turquoise),
           yellow = case_when(date == d ~ y,
                             TRUE ~ yellow),
           black = case_when(date == d ~ b,
                             TRUE ~ black))
}
#add 3 black and 1 turquoise for 5/15
squares_per_day <- add_squares("2020-5-15", 1, 0, 3)
#add 4 black for 5/16
squares_per_day<- add_squares("2020-5-16", 0, 0, 4)
#add 6 turquoise for 5/17
squares_per_day<- add_squares("2020-5-17", 6, 0, 0)
#add 2 turquoise for 5/19
squares_per_day<- add_squares("2020-5-19", 2, 0, 0)
#add 1 turquoise for 5/20
squares_per_day<- add_squares("2020-5-20", 1, 0, 0)
#add 4 turquoise for 5/21
squares_per_day<- add_squares("2020-5-21", 4, 0, 0)



totals_now <- squares_per_day %>%
  summarize(turquoise_total = sum(turquoise),
            yellow_total = sum(yellow),
            black_total = sum(black),
            all = turquoise_total + yellow_total + black_total) 

all_squares <-  data.frame(turquoise_total = 164, yellow_total = 98, black_total = 62, all = 324)

totals_now / all_squares
all_squares[1]

squares_per_day %>%
  summarize(turquoise_total = sum(turquoise),
            yellow_total = sum(yellow),
            black_total = sum(black),
            all = turquoise_total + yellow_total + black_total) / all_squares

square_tbl <- data.frame(t(rbind(rbind(all_squares, totals_now), totals_now / all_squares)))
colnames(square_tbl) <- c("total", "done", "percent_done")
square_tbl <- tibble::rownames_to_column(square_tbl, "color")

square_tbl <- square_tbl %>%
  mutate(remaining = total - done )

square_tbl <- square_tbl %>%
  mutate(color = str_remove(color, "_total"))

formattable(square_tbl)

percent(square_tbl$percent_done)

star <- "#EEC639"
bg <- "#87CEEB"
outline <- "#0F0C08"
white <- "#FFFFFF"
red <- "#ff7f7f"

formattable(square_tbl, #align =c("l","c","c","c","c", "c", "c", "c", "r"), 
            list(
  color = 
    formatter("span", style = ~ style(color = "grey",font.weight = "bold")), 
  turquoise = color_tile(white, bg),
  yellow = color_tile(white, star),
  black = color_tile(white, outline),
   all = color_tile(white, red)
))

data.table(square_tbl)
