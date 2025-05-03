library(ggplot2)
library(dplyr)



a_green <- "#00FF00"
bg <- "#2F4F4F"
outline <- "#A9A9A9"
datachips_green <- "#36ff33"
datachips_green2 <- "#1e8449"
datachips_grey <- "#6D6D6F"
datachips_pink <- "#ea27c2"
lavender = "#bf80ff"
dark <- "#34495e"
datachips_yellow <- "#FFFF1A"
datachips_light <- "#e1f1ef"

#blank theme
pix_theme <- theme(panel.grid = element_blank(), 
                   #panel.background = element_rect(fill = dark),
                   #plot.background = element_rect(fill = dark),
                   axis.text = element_blank(), 
                   axis.ticks = element_blank(), 
                   axis.title = element_blank(),
                   #panel.grid.major = element_blank(),
                   #panel.grid.minor = element_blank(),
                   legend.position = "none",
                    plot.title = element_text(
                      family = "mono" ,
                      size = 52,
                   
                       hjust = 0.5,
                       vjust = -1,
                      face = "bold",
                      color = "white"
                   )
                   )



#populate data frame with coordinates
blank_wu <- data.frame(x = rep(-10:10, each = 21), y = rep(-10:10, times = 21), 
                    c = 0)

wu_tang <- blank_wu %>%
  mutate(c = case_when((x == 0 & y %in% c(-6, -2:4)) ~ 1,
                       (x == 1 & y %in% c(-8:-7, -3:1, 5)) ~ 1,
                       (x %in% 2:3 & y %in% -7:6) ~ 1,
                       (x %in% 4:5 & y %in% -6:7) ~ 1,
                       (x == 6 & y %in% -5:8) ~ 1,
                       (x == 7 & y %in% -3:6) ~ 1,
                       (x == 8 & y %in% -2:3) ~ 1,
                       (x == -1 & y %in% c(-6:-5, -3:3)) ~ 1,
                       (x == -2 & y %in% c(-6:1)) ~ 1,
                       (x == -3 & y %in% c(-6:3, 5)) ~ 1,
                       (x == -4 & y %in% c(-6:5)) ~ 1,
                       (x %in% -6:-5 & y %in% c(-5:5)) ~ 1,
                       (x == -7 & y %in% c(-4:5)) ~ 1,
                       (x == -8 & y %in% c(-3:6)) ~ 1,
                       (x == -9 & y %in% c(-1:4)) ~ 1,
                       TRUE ~ c) 
                  ) 
         


wu_tang %>%
  ggplot(aes(x, y, fill = factor(c))) +
  geom_point(size = 12, shape = 22, color="black") +
  scale_fill_manual(values = c( "black", "yellow")) +
  pix_theme +
  xlim(-10, 10) +
  ylim(-10, 10) #+
  # geom_label(
  #   label="data-chips.com/canna",
  #   # x= 0,
  #   # y=11.5,
  #   x = 0,
  #   y = -1.1,
  #   size = 15,
  #   family = "mono",
  #   color = "black",
  #   fill = datachips_yellow,
  #   label.size = 0,
  #   label.padding = unit(0.75, "lines"),
  # ) +
  # geom_text(
  #   label="data consultancy",
  #   # x= 0,
  #   # y=11.5,
  #   x = 0,
  #   y = 32,
  #   size = 18,
  #   family = "mono",
  #   color = "white",
  #   face = "bold"#,
  #   #fill = datachips_grey,
  #   #label.size = 0
  # )

  labs(
    x = "",
    y = "",
    title = "data consultancy"
  )

blank_leaf2 <- data.frame(x = rep(-19:19, each = 38), y = rep(1:38, times = 39), 
                         c = 0, a = 0)


a_leaf %>%
  ggplot(aes(x, y, fill = factor(c))) +
  geom_point(size = 7, shape = 22, color="transparent") +
  scale_fill_manual(values = c("LightSteelBlue", "forestgreen")) +
  pix_theme +
  xlim(-20, 20) +
  ylim(1, 38)

a_leaf %>%
  ggplot(aes(x, y, fill = factor(c))) +
  geom_point(size = 7, shape = 22, color="transparent") +
  scale_fill_manual(values = c("purple", "seagreen")) +
  pix_theme +
  xlim(-20, 20) +
  ylim(1, 38)

a_leaf %>%
  ggplot(aes(x, y, fill = factor(c))) +
  geom_point(size = 7, shape = 22, color="transparent") +
  scale_fill_manual(values = c("cornflowerblue", "springgreen")) +
  pix_theme +
  xlim(-20, 20) +
  ylim(1, 38)

a_leaf %>%
  ggplot(aes(x, y, fill = factor(c))) +
  geom_point(size = 7, shape = 22, color="transparent") +
  scale_fill_manual(values = c("DarkTurquoise", "OliveDrab")) +
  pix_theme +
  xlim(-20, 20) +
  ylim(1, 38)

xValue <- 1:10
yValue <- cumsum(rnorm(10))
data <- data.frame(xValue,yValue)

# Plot
ggplot(data, aes(x=xValue, y=yValue)) +
  geom_line()

line_x <- c(-18, -13, -8, -3, 2, 7, 12, 17)
line_y <- c(0, 6, 14, 19, 27, 25, 31, 35)
line_data <- data.frame(line_x,line_y)


# ggplot(line_data, aes(x=line_x, y=line_y)) +
#   geom_line(color = datachips_pink, size = 4) +
#   pix_theme +
#   xlim(-20, 20) +
#   ylim(1, 38)




  ggplot() +
  geom_point(data = a_leaf, aes(x, y, fill = factor(c)), size = 7, shape = 22, color="transparent") +
  scale_fill_manual(values = c( "transparent", datachips_green,
                                datachips_pink, "white", datachips_grey)) +
  geom_line(data = line_data, aes(x=line_x, y=line_y),
            color = lavender, size = 3, arrow = arrow(angle = 40)) +
  pix_theme +
  xlim(-20, 20) +
  ylim(-2, 38) #+
# geom_label(
#   data = a_leaf,
#   label="data-chips.com/canna",
#   # x= 0,
#   # y=11.5,
#   x = 0,
#   y = -1,
#   size = 10,
#   family = "mono",
#   color = "black",
#   fill = datachips_yellow,
#   label.size = 0
# ) +
# labs(
#   x = "",
#   y = "",
#   title = "data consultancy"
# )
  