library(tidyverse)
library(ggthemes)
library(grid)
library(gridExtra)

scorecard <- read_csv("scorecard.csv")
str(scorecard)
scorecard$urgent_1 <- as.factor(scorecard$urgent_1)
scorecard$urgent_2 <- as.factor(scorecard$urgent_2)
scorecard$urgent_1 <- factor(scorecard$urgent_1, levels = c("low", "medium", "high"))
scorecard$urgent_2 <- factor(scorecard$urgent_2, levels = c("low", "medium", "high"))

bg_color = "white"
sc_1theme <- #theme_wsj() +
  theme(
    panel.background = element_rect(fill = bg_color),
    plot.background = element_rect(fill = bg_color),
    legend.background = element_rect(fill = bg_color),
    legend.position = "bottom",
    axis.ticks.y = element_blank(),
    axis.text.y = element_text(hjust = 0.5, face = "bold"),
    panel.grid.major = element_line(color = "gray95"),
    panel.border = element_blank()
  )

sc_2theme <- #theme_wsj() +
  theme(
    panel.background = element_rect(fill = bg_color),
    plot.background = element_rect(fill = bg_color),
    legend.background = element_rect(fill = bg_color),
    legend.position = "bottom",
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    plot.title = element_text(hjust = 0.5),
    panel.grid.major = element_line(color = "gray95")
  )
  
sc_theme <- #theme_wsj() +
  theme(
    panel.background = element_rect(fill = bg_color),
    plot.background = element_rect(fill = bg_color),
    legend.background = element_rect(fill = bg_color),
    legend.position = "bottom",
    axis.ticks.y = element_blank(),
    axis.text.y = element_text(face = "bold", size = 11, hjust = 1),
    axis.title.y = element_blank(),
    panel.grid.major = element_line(color = "gray95"),
    panel.border = element_blank(),
    plot.title = element_text(size = 15)
  )
sc_theme_vertical <- #theme_wsj() +
  theme(
    panel.background = element_rect(fill = bg_color),
    plot.background = element_rect(fill = bg_color),
    legend.background = element_rect(fill = bg_color),
    legend.position = "bottom",
    axis.ticks.y = element_blank(),
    axis.text.x = element_text(face = "bold", size = 12),
    panel.grid.major = element_line(color = "gray95"),
    panel.border = element_blank()
  )

sc_1 <- scorecard %>%
  ggplot(aes(x = topic, y = important_1, fill = urgent_1)) +
  geom_col() +
  coord_flip() +
  geom_text(aes(label = important_1, y = important_1 - 1)) +
  scale_fill_manual(values = c("yellow", "orange", "red")) +
  
  ylim(0, 10) +
  sc_1theme +
  labs(
    x = "",
    y = "importance",
    fill = "urgency",
    title = "wife's responses"
  ) 

sc_2 <- scorecard %>%
  #mutate(important_2 = important_2 * -1) %>%
  ggplot(aes(x = topic, y = important_2, fill = urgent_2)) +
  geom_col() +
  coord_flip() +
  geom_text(aes(label = important_2, y = important_2 - 1)) +
  scale_fill_manual(values = c("yellow", "orange", "red")) +
  
  ylim(10, -0) +
  sc_2theme +
  labs(
    x = "",
    y = "importance",
    fill = "urgency",
    title = "husband's responses"
  ) 
  
grid.arrange(sc_2, sc_1, nrow = 1, ncol = 2, widths = c(0.85, 1.15), 
             top = textGrob("Wealth Momentum Scorecard",gp=gpar(fontsize=18,font=3)))


#gather to make scorecard long
scorecard_long <- read_csv("scorecard1.csv")
scorecard_long$urgent <- factor(scorecard_long$urgent, levels = c("low", "medium", "high"))
str(scorecard_long)

#change to 5 point scale
scorecard_long <- scorecard_long %>%
  mutate(important = as.integer(important / 2) + 1)


scorecard_long %>%
  ggplot(aes(x = topic, y = important, color = partner, fill = urgent)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = c("yellow", "orange", "red")) +
  coord_flip() +
  sc_1theme +
  ylim(0, 5)


scorecard_long <- scorecard_long %>%
  mutate(partner = str_replace(partner, "husband", "Harry"),
         partner = str_replace(partner, "wife", "Meghan"))
family_mgmt <- scorecard_long[1:12,]
financial_mgmt <- scorecard_long[13:24,]


#family_mgmt$topic <- factor(family_mgmt$topic, levels = family_mgmt$topic)
family_mgmt %>%
  ggplot(aes(x = topic, y = important,  color = partner, fill = urgent)) +
  geom_col(position = position_dodge2(reverse = TRUE), width = 1.5) +
  scale_fill_manual(values = c("yellow", "orange", "red")) +
  geom_text(aes(label = partner, vjust = ifelse(partner == "Meghan", 1.75, -0.8)), y = 0.25, color = "black") +
  coord_flip() +
  sc_theme +
  ylim(0, 5) +
  scale_x_discrete(limits=rev(family_mgmt$topic))

#financial_mgmt$topic <- factor(family_mgmt$topic, levels = family_mgmt$topic)
financial <- financial_mgmt %>%
  ggplot(aes(x = topic, y = important,  group = partner, fill = urgent)) +
  geom_col(position = position_dodge2(reverse = TRUE), width = 1.5) +
  scale_fill_manual(values = c("#8c96c6", "#88419d", "#4d004b")) +
  geom_text(aes(label = partner, hjust = 0, vjust = ifelse(partner == "Meghan", 1.75, -1)), 
            y = 0.1,  color = "white") +
  coord_flip() +
  sc_theme +
  ylim(0, 5) +
  scale_x_discrete(limits=rev(financial_mgmt$topic)) +
  labs(
    x = "",
    y = "importance",
    fill = "urgency: ",
    title = "Financial Management"
  ) 
#financial_mgmt$topic <- factor(family_mgmt$topic, levels = family_mgmt$topic)
family <- family_mgmt %>%
  ggplot(aes(x = topic, y = important,  group = partner, fill = urgent)) +
  geom_col(position = position_dodge2(reverse = TRUE), width = 1.5) +
  scale_fill_manual(values = c("#8c96c6", "#88419d", "#4d004b")) +
  geom_text(aes(label = partner, hjust = 0, vjust = ifelse(partner == "Meghan", 1.75, -1)), 
            y = 0.1,  color = "white") +
  coord_flip() +
  sc_theme +
  ylim(0, 5) +
  scale_x_discrete(limits=rev(family_mgmt$topic)) +
  labs(
    x = "",
    y = "importance",
    fill = "urgency: ",
    title = "Family Management"
  ) 



grid.arrange(family, financial, nrow = 1, ncol = 2,  
             top = textGrob("Wealth Momentum Scorecard",gp=gpar(fontsize=21)))




#vertical bars
financial_mgmt %>%
  ggplot(aes(x = topic, y = important,  group = partner, fill = urgent)) +
  geom_col(position = position_dodge2(reverse = TRUE), width = 1.5) +
  scale_fill_manual(values = c("yellow", "orange", "red")) +
  geom_text(aes(label = partner,  vjust = ifelse(partner == "Megan", 1.75, -0.8)), y = 0, color = "black") +
  #coord_flip() +
  sc_theme_vertical +
  ylim(0, 5) +
  scale_x_discrete(limits=financial_mgmt$topic)

