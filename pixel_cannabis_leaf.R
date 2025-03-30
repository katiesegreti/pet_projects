library(ggplot2)
library(dplyr)



a_green <- "#00FF00"
bg <- "#2F4F4F"
outline <- "#A9A9A9"
datachips_green <- "#36ff33"
datachips_grey <- "#6D6D6F"
datachips_pink <- "#ea27c2"
lavender = "#bf80ff"
dark <- "#34495e"
datachips_yellow <- "#FFFF1A"

#blank theme
pix_theme <- theme(panel.grid = element_blank(), 
                   panel.background = element_rect(fill = dark),
                   plot.background = element_rect(fill = dark),
                   axis.text = element_blank(), 
                   axis.ticks = element_blank(), 
                   axis.title = element_blank(),
                   panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   legend.position = "none",
                   plot.title = element_text(
                     family = "mono" ,
                     size = 48,
                     hjust = 0.5,
                     vjust = 0.05,
                     face = "bold",
                     color = "white"
                     
                   )
)

#populate data frame with coordinates
blank_leaf <- data.frame(x = rep(-19:19, each = 38), y = rep(1:38, times = 39), 
                    c = 0)

a_leaf <- blank_leaf %>%
  mutate(c = case_when((x == 0 & y %in% 2:37) ~ 1,
                       (abs(x) == 1 & y %in% 2:35) ~ 1,
                       (abs(x) == 2 & y %in% 8:33) ~ 1,
                       (abs(x) == 3 & y %in% 7:29) ~ 1,
                       (abs(x) == 4 & y %in% 7:18) ~ 1,
                       (abs(x) == 5 & y %in% 6:19) ~ 1,
                       (abs(x) == 6 & y %in% 6:20) ~ 1,
                       (abs(x) == 7 & y %in% 6:21) ~ 1,
                       (abs(x) == 8 & y %in% c(6:8, 10:14, 16:21)) ~ 1,
                       (abs(x) == 9 & y %in% c(6:7, 10:14, 17:22)) ~ 1,
                       (abs(x) == 10 & y %in% c(6, 10:14, 18:23)) ~ 1,
                       (abs(x) == 11 & y %in% c(10:14, 19:24)) ~ 1,
                       (abs(x) == 12 & y %in% c(10:13, 20:25)) ~ 1,
                       (abs(x) == 13 & y %in% c(10:13, 21:25)) ~ 1,
                       (abs(x) == 14 & y %in% c(11:13, 22:26)) ~ 1,
                       (abs(x) == 15 & y %in% c(11:13, 23:26)) ~ 1,
                       (abs(x) == 16 & y %in% c(12:13, 26:27)) ~ 1,
                       (abs(x) == 17 & y %in% 12:13) ~ 1,
                       (abs(x) == 18 & y == 13) ~ 1,
                       TRUE ~ c) 
                  ) %>%
  # make a heart
  mutate(c = case_when((x == 0 & y %in% 9:15) ~ 2,
                       (abs(x) == 1 & y %in% 10:16) ~ 2,
                       (abs(x) == 2 & y %in% 11:17) ~ 2,
                       (abs(x) == 3 & y %in% 12:17) ~ 2,
                       (abs(x) == 4 & y %in% 13:16) ~ 2,
                       (abs(x) == 5 & y %in% 14:15) ~ 2,
                       TRUE ~ c)
         ) %>%
  # add white section to heart?
  mutate(c = ifelse(((x == -3 & y == 15) ), 3, c))
                       
         


a_leaf %>%
  ggplot(aes(x, y, fill = factor(c))) +
  geom_point(size = 7, shape = 22, color="transparent") +
  scale_fill_manual(values = c( dark, datachips_green, datachips_pink, "white")) +
  pix_theme +
  xlim(-20, 20) +
  ylim(-2, 38) 

