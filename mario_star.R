library(ggplot2)
library(dplyr)
library(reactable)

#blank theme
pix_theme <- theme(panel.grid = element_blank(), 
                   panel.background = element_rect(fill = "grey60"),
                   plot.background = element_rect(fill = "black"),
                   axis.text = element_blank(), 
                   axis.ticks = element_blank(), 
                   axis.title = element_blank(),
                   legend.position = "none")
#colors
star <- "#EEC639"
bg <- "#87CEEB"

outline <- "#0F0C08"

mario_star <- data.frame(x = rep(1:18, each = 18), y = rep(1:18, times = 18), 
                         c = 0, a = 0)
star1 <- mario_star %>% 
  mutate(c = case_when((y == 2 & x %in% c(2, 3, 4, 15, 16, 17)) |
                         (y == 3 & x %in% c(2, 5, 6, 13, 14, 17)) |
                         (y == 4 & x %in% c(3, 7, 8, 11, 12, 16)) |
                        (y == 5 & x %in% c(3, 9, 10, 16)) |
                        (y %in% c(6, 7) & x %in% c(4, 15)) |
                        (y %in% c(8, 9) & x %in% c(5, 14)) |
                        (y == 10 & x %in% c(4, 15)) |
                        (y == 11 & x %in% c(3, 16)) |
                        (y == 12 & x %in% c(2, 17)) |
                        (y == 13 & x %in% c(2:7, 12:17)) |
                        (y == 14 & x %in% c(7, 12)) |
                        (y %in% c(15, 16) & x %in% c(8, 11)) |
                        (y == 17 & x %in% c(9, 10)) |
                        (y %in% c(9:11) & x %in% c(8, 11))  ~ 1,
                       (y == 3 & x %in% c(3, 4, 15, 16)) | 
                        (y == 4 & x %in% c(4, 5, 6, 13, 14, 15)) |
                        (y == 5 & x %in% c(4:8, 11:15)) |
                        (y %in% c(6,7) & x %in% c(5:14)) |
                        (y == 8 & x %in% c(6:13))  ~ 2,
                       TRUE ~ c)
  
)
#star1$c
star1 %>%
  ggplot(aes(x, y, fill = factor(c))) +
  geom_point(size = 11, shape = 22) +
  scale_fill_manual(values = c(bg,outline, star)) +
  pix_theme +
  xlim(1, 19) +
  ylim(1, 19)



star2 <- mario_star %>%
  mutate(c = case_when((x %in% c(9, 10) & y %in% c(5, 17))  |
                        (x %in% c(8, 11) & y %in% c(4, 9, 10, 11, 15, 16)) |
                        (x %in% c(7, 12) & y %in% c(4, 13, 14)) |
                        (x %in% c(6, 13) & y %in% c(3, 13)) |
                        (x %in% c(5, 14) & y %in% c(3, 8, 9, 13)) |
                        (x %in% c(4, 15) & y %in% c(2, 6, 7, 10, 13)) |
                        (x %in% c(3, 16) & y %in% c(2, 4, 5, 11, 13)) |
                        (x %in% c(2, 17) & y %in% c(2, 3, 12, 13)) ~ 1,
                       (x %in% c(9, 10) & y %in% c(6:16)) |
                        (x %in% c(8, 11) & y %in% c(5:8, 12:14)) |
                        (x %in% c(7, 12) & y %in% c(5:12)) |
                        (x %in% c(6, 13) & y %in% c(4:12)) |
                        (x %in% c(5, 14) & y %in% c(4:7, 10:12)) |
                        (x %in% c(4, 15) & y %in% c(3:5, 11:12)) |
                        (x %in% c(3, 16) & y %in% c(3, 12))  ~ 2,
                       TRUE ~ c))

star2 %>%
  ggplot(aes(x, y, fill = factor(c))) +
  geom_point(size = 11, shape = 22) +
  scale_fill_manual(values = c(bg,outline, star)) +
  pix_theme +
  xlim(1, 19) +
  ylim(1, 19)


star2 <- star2 %>% 
  mutate(color = case_when(c == 0 ~ "turquoise",
                           c == 1 ~ "black",
                           c == 2 ~ "yellow"))





#redo square_tbl
square_tbl1 <- data.frame("color" = c("turquoise", "yellow", "black", "all"),
                          "total" = c(164, 98, 62, 324),
                          "done" = c(61, 26, 24, 111)
                          ) %>%
  mutate(remaining = total - done,
         pct_done = done / total)


star3 <- star2 %>% 
  left_join(square_tbl1, by = "color") %>%
  group_by(color) %>% 
  arrange(y) %>%
  mutate(counter = row_number(color)) %>%
  mutate(a = case_when(counter <= done ~ 1,
                       TRUE ~ 0.5))

star3 %>%
  ggplot(aes(x, y, fill = factor(c))) +
  geom_point(size = 11, shape = 22, aes(alpha = a)) +
  scale_fill_manual(values = c(bg,outline, star)) +
  scale_alpha(range = c(0.08, 1)) +
  pix_theme +
  xlim(1, 18) +
  ylim(1, 18)



reactable(square_tbl1)
