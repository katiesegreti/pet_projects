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

#populate data frame with coordinates
star1 <- data.frame(x = rep(1:18, each = 18), y = rep(1:18, times = 18), 
                         c = 0, a = 0)



mario_star <- star1 %>%
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
                       TRUE ~ c),
         color = case_when(
           c == 0 ~ "turquoise",
           c == 1 ~ "black",
           c == 2 ~ "yellow")
         )

mario_star %>%
  ggplot(aes(x, y, fill = factor(c))) +
  geom_point(size = 11, shape = 22) +
  scale_fill_manual(values = c(bg,outline, star)) +
  pix_theme +
  xlim(1, 19) +
  ylim(1, 19)



# square_tbl to track progress
square_tbl <- data.frame("color" = c("turquoise", "yellow", "black", "all"),
                          "total" = c(164, 98, 62, 324),
                          "done" = c(61, 32, 29, 122)
) %>%
  mutate(remaining = total - done,
         pct_done = done / total)


mario_star_progress <- mario_star %>% 
  left_join(square_tbl, by = "color") %>%
  group_by(color) %>% 
  arrange(y) %>%
  mutate(counter = row_number(color)) %>%
  mutate(a = case_when(counter <= done ~ 1,
                       TRUE ~ 0.5))

mario_star_progress %>%
  ggplot(aes(x, y, fill = factor(c))) +
  geom_point(size = 11, shape = 22, aes(alpha = a)) +
  scale_fill_manual(values = c(bg,outline, star)) +
  scale_alpha(range = c(0.08, 1)) +
  pix_theme +
  xlim(1, 18) +
  ylim(1, 18)



reactable(square_tbl)


