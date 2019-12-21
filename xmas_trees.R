library(ggplot2)
library(dplyr)
library(gridExtra)

#blank theme
pix_theme <- theme(panel.grid = element_blank(), 
                   panel.background = element_rect(fill = "#d1d3f9"),
                   axis.text = element_blank(), 
                   axis.ticks = element_blank(), 
                   axis.title = element_blank(),
                   legend.position = "none")
#xmas tree
trunk <- "#3A2F30"
star <- "#f4e646"
tree <- "#02492C"
pink <- "#f75da5"
blue <- "#47b0e5"
gold <- "#E0DA8D"
beige <- "#faedd2"
ltgreen <- "#80C49B"
red <- "#C1202D"
teal <- "#009796"

good_seeds <- c(1110, 411, 1981, 109, 11209, 41, 27, 54, 1987)
set.seed(1987)
xmas <- data.frame(x = rep(1:15, each = 19), y = rep(1:19, times = 15), c = 0,
                   change = runif(285)) %>%
  mutate(c = case_when(y <= 2 & x %in% 7:9 ~ 1,
                       (y == 18 & x %in% 7:9) | (y == 19 & x == 8) ~ 2,
                       (y %in% 3:4) | (y %in% 5:6 & x %in% 2:14) | 
                         (y %in% 7:8 & x %in% 3:13) | (y %in% 9:10 & x %in% 4:12) |
                         (y %in% 11:12 & x %in% 5:11) | (y %in% 13:14 & x %in% 6:10) |
                         (y %in% 15:16 & x %in% 7:9) | (y == 17 & x == 8)  ~ 3)) %>%
  mutate(clr = case_when(is.na(c) ~ 0,
                         c == 1 ~ 1,
                         c == 2 ~ 2,
                         (c == 3 & (change >= 0.18 | y == 17)) ~ 3,
                         (c == 3 & change < 0.06) ~ 4,
                         (c == 3 & change  < 0.12) ~ 5,
                         (c == 3 & change < 0.18) ~ 6))
xmas %>%
  filter(c > 0) %>%
  ggplot(aes(x, y, fill = factor(clr))) +
  geom_point(size = 11, shape = 22) +
  scale_fill_manual(values = c(trunk, star, tree, blue, gold, 
                               pink)) +
  pix_theme +
  xlim(-1, 17) +
  ylim(1, 19)


grid.arrange(xmas_109, xmas_1110, xmas_11209, xmas_1981, 
             xmas_1987, xmas_27, xmas_41, xmas_411, xmas_54,
             nrow = 3, ncol = 3)

xmas_109
xmas_1110
xmas_11209
xmas_1981
xmas_1987
xmas_27
xmas_41
xmas_411
xmas_54
