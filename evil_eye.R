library(ggplot2)
library(dplyr)
library(gridExtra)

#blank theme
pix_theme <- theme(panel.grid = element_blank(), 
                   panel.background = element_rect(fill = "grey77"),
                   axis.text = element_blank(), 
                   axis.ticks = element_blank(), 
                   axis.title = element_blank(),
                   legend.position = "none")

#create dataframe 21 x 11
x <- rep(-10:10, each = 11)
y <- rep(-5:5, times = 21)
c <- rep(0, 231)
eye <- data.frame(x, y, c)

# dark blue: 1, white: 2, light blue: 3, black: 4
#assign colors for x == 0
zeroes <- c(4, 4, 4, 3, 2, 1)

#assign repeat times for each color
lt_blue <- c(2, 2, 2, 1, 0, 0)
white <- c(4, 3, 2, 2, 2, 0)
dk_blue <- c(4, 5, 5, 4, 4, 4)
blank <- c(0, 0, 1, 3, 4, 6)

# loop to assign c
for(i in 0:5) {
  pattern <- c(rep(3, times = lt_blue[i+1]), 
               rep(2, times = white[i+1]),
               rep(1, times = dk_blue[i+1]),
               rep(0, times = blank[i+1]))
  #assign c for this y
  eye$c[y == i] <- c(rev(pattern), zeroes[i+1], pattern)
  #assign c for this y, negative version
  eye$c[y == -i] <- c(rev(pattern), zeroes[i+1], pattern)
}

eye1 <- eye %>%
  filter(c > 0) %>%
  ggplot(aes(x = x, y = y, fill = factor(c))) +
  geom_point(size = 11, shape = 22) +
  pix_theme +
  scale_fill_manual(values = c("blue", "white", "cyan", "black")) +
  xlim(-11, 11) +
  ylim(-6, 6)

#blank theme
pix_theme2 <- theme(panel.grid = element_blank(), 
                   panel.background = element_rect(fill = "#FFD700"),
                   axis.text = element_blank(), 
                   axis.ticks = element_blank(), 
                   axis.title = element_blank(),
                   legend.position = "none")
eye2 <- eye %>%
  filter(c > 0) %>%
  ggplot(aes(x = x, y = y, fill = factor(c))) +
  geom_point(size = 11, shape = 22) +
  pix_theme2 +
  scale_fill_manual(values = c("blue", "white", "cyan", "black")) +
  xlim(-11, 11) +
  ylim(-6, 6)

grid.arrange(eye1, eye2)
