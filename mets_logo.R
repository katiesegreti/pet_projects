library(ggplot2)
library(dplyr)
library(gridExtra)

#blank theme
pix_theme <- theme(panel.grid = element_blank(), 
                   panel.background = element_rect(fill = "white"),
                   axis.text = element_blank(), 
                   axis.ticks = element_blank(), 
                   axis.title = element_blank(),
                   legend.position = "none")

#mets
# 0: white, 1: orange, 2: blue
x <- rep(0:25, each = 31)
y <- rep(0:30, times = 26)
c <- rep(0, 806)
mets <- data.frame(x, y, c)

mets$c[y==0] <- c(rep(0, 9), rep(1, 8), rep(0, 9))
mets$c[y==1] <- c(rep(0, 9), 1, rep(2, 6), 1, rep(0, 9))
mets$c[y==2] <- c(rep(0, 9), 1, 2, 1, 2, 2, 1, 2, 1, rep(0, 9))
mets$c[y==3] <- c(rep(0, 9), 1, 1, 1, 2, 2, 1, 1, 1, rep(0, 9))
mets$c[y==4] <- c(rep(0, 11), 1, 2, 2, 1, rep(0, 11))
mets$c[y==5] <- c(rep(0, 4), rep(1, 4), rep(0, 3), 1, 2, 2, 1, rep(0, 3), rep(1, 4), rep(0, 4))
mets$c[y==6] <- c(0, 0, 1, 1, rep(2, 4), 1, 1, 0, 1, 2, 2, 1, 0, 1, 1, rep(2, 4), 1, 1, 0, 0)
mets$c[y==7] <- c(0, 0, 1, rep(2, 6), 1, 0, 1, 2, 2, 1, 0, 1, rep(2, 6), 1, 0, 0)
mets$c[y==8] <- c(0, 0, 1, 2, 1, 2, 2, 1, 2, 1, 0, 1, 2, 2, 1, 0, 1, 2, 1, 2, 2, 1, 2, 1, 0, 0)
mets$c[y==9] <- c(0, 0, 1, 1, 1, 2, 2, 1, 1, 1, 0, 1, 2, 2, 1, 0, 1, 1, 1, 2, 2, 1, 1, 1, 0, 0)
mets$c[y==10] <- c(rep(0, 4), 1, 2, 2, 1, 0, 0, 0, 1, 2, 2, 1, 0, 0, 0, 1, 2, 2, 1, rep(0,4))
mets$c[y==11] <- c(rep(0, 4), 1, 2, 2, 1, 0, 0, 0, 1, 2, 2, 1, 0, 0, 1, 2, 2, 2, 1, rep(0,4))
mets$c[y==12] <- c(rep(0, 4), 1, 2, 2, 1, 0, 0, 0, 1, 2, 2, 1, 0, 1, 2, 2, 2, 2, 1, rep(0,4))
mets$c[y==13] <- c(rep(0, 4), 1, 2, 2, 1, 0, 0, 1, 2, 2, 2, 1, 1, 2, 2, 2, 2, 2, 1, rep(0,4))
mets$c[y==14] <- c(rep(0, 4), 1, 2, 2, 1, 0, 1, rep(2, 11), 1, rep(0, 4))
mets$c[y==15] <- c(1, 1, 0, 0, 1, 2, 2, 1, 0, 1, 2, 2, 2, 1, 2, 2, 2, 2, 1, 2, 2, 1, 0, 1, 1, 0)
mets$c[y==16] <- c(1, 2, 1, 0, 1, 2, 2, 1, 1, 2, 2, 2, 1, 1, 2, 2, 2, 1, 1, 2, 2, 1, 0, 1, 2, 1)
mets$c[y==17] <- c(1, 2, 2, 1, 1, 2, 2, 1, 2, 2, 2, 1, 1, rep(2, 5), 1, 2, 2, 1, 1, 2, 2, 1)
mets$c[y==18] <- c(1, rep(2, 10), 1, rep(2, 6), 1, rep(2, 6), 1)
mets$c[y==19] <- c(1, rep(2, 9), 1, 1, 2, 2, 2, 1, 1, rep(2, 8), 1)
mets$c[y==20] <- c(0, 1, rep(2, 8), 1, 2, 2, 2, rep(1, 4), rep(2, 6), 1, 0)
mets$c[y==21] <- c(0, 0, 1, rep(2, 6), 1, 2, 2, 2, 1, 1, 0, 0, 1, rep(2, 5), 1, 1, 0)
mets$c[y==22] <- c(0, 0, 0, 1, 1, 2, 2, 1, 1, 2, 2, 2, 1, 1, rep(0, 4), 1, 2, 2, 1, 1, 1, 0, 0)
mets$c[y==23] <- c(rep(0, 4), 1, 2, 2, 1, 2, 2, 2, 1, 1, rep(0, 5), 1, 2, 2, 1, rep(0,4))
mets$c[y==24] <- c(rep(0, 4), 1, rep(2, 5), 1, 1, rep(0, 6), 1, 2, 2, 1, rep(0,4))
mets$c[y==25] <- c(rep(0, 4), 1, rep(2, 4), 1, 1, rep(0, 7), 1, 2, 2, 1, rep(0, 4))
mets$c[y==26] <- c(rep(0, 4), 1, 2, 2, 2, 1, 1, rep(0, 8), 1, 2, 2, 1, rep(0,4))
mets$c[y==27] <- c(0, 0, 1, 1, 1, 2, 2, 1, 1, 1, rep(0, 6), 1, 1, 1, 2, 2, 1, 1, 1, 0, 0)
mets$c[y==28] <- c(0, 0, 1, 2, 1, 2, 2, 1, 2, 1, rep(0, 6), 1, 2, 1, 2, 2, 1, 2, 1, 0, 0)
mets$c[y==29] <- c(0, 0, 1, rep(2, 6), 1, rep(0, 6), 1, rep(2, 6), 1, 0, 0)
mets$c[y==30] <- c(0, 0, rep(1, 8), rep(0, 6), rep(1, 8), 0, 0)

mets %>%
  ggplot(aes(x = x, y = y, fill = as.factor(c))) +
  geom_point(size = 17, shape = 22) +
  scale_fill_manual(values = c("white", "orange", "blue")) +
  pix_theme +
  xlim(0, 25) +
  ylim(0, 30)