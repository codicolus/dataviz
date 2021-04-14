# Author: Christoph von Matt
# Github: @codicolus
# Created on: 07-04-2021
#
######################################################################################################
# This script is licenced under: CC-BY-NC-SA
# Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International Public Licence
# For more information see here: https://creativecommons.org/licenses/by-nc-sa/4.0/legalcode
######################################################################################################

# load libraries
library(ggplot2)
library(extrafont)

# load fonts
#loadfonts(device = "win")

# create data set
get_data <- function(num){
  x <- rep(NA, num*num)
  y <- rep(NA, num*num)
  color <- rep(NA, num*num)
  # generate data
  for(i in seq(num)){
    counter <- (i-1)*num + 1
    end <- num*i
    x[counter:end] <- seq(num)
    y[counter:end] <- sin(1:num)
  }
  color <- y
  
  return(data.frame(x, y, color))
}


# FLOWER POWER
df2 <- get_data(18*15)
cols <- c("#D7624B", "#77256b", "#9966A9", "#E89838", "#70086E", "#9C010E", "#084594", "#005A32", "#1362A4", "#2D2D2D")
pals <- c("RdBu", "RdPu", "PRGn", "PuOr", "BuPu", "Reds", "Blues", "Greens", "GnBu", "Greys")

for(i in seq(length(pals))){
  p <- ggplot(df2, aes(x, y, color = -y, group = y)) +
    geom_area(lwd = 4, show.legend = F) +
    scale_color_distiller(palette = pals[i]) +
    scale_y_continuous(breaks = NULL) +
    scale_x_continuous(breaks = NULL) +
    xlab(NULL) + ylab(NULL) +
    coord_polar("x") +
    labs(caption = "Plot created by Christoph von Matt | Data: self-generated dataset",
         title = "Flower Power") +
    theme(
      plot.background = element_rect(fill = "white"),
      panel.background = element_blank(),
      text = element_text(family = "Comic Sans MS", color = cols[i]),
      plot.caption = element_text(hjust = 0.5, vjust = 15),
      plot.title = element_text(hjust = 0.5, vjust = -3, size = 25, face = "bold")
    )
  p
  ggsave(paste0("flower_power_", pals[i], ".png"), width = 6, height = 7)
}

# DISCONTINUITY
df <- get_data(2000)
p <- ggplot(df, aes(x, y)) +
  geom_bin2d(show.legend = F, binwidth = 20) +
  scale_fill_distiller() + 
  scale_y_continuous(breaks = NULL, expand = c(0,0)) +
  scale_x_continuous(breaks = NULL, expand = c(0,0)) +
  xlab(NULL) + ylab(NULL) +
  coord_polar("x") +
  labs(caption = "Plot created by Christoph von Matt (@chvonmatt) | Data: self-generated dataset",
       title = "Discontinuity") +
  theme(
    plot.background = element_rect(fill = "white"),
    panel.background = element_rect(fill = "white"),
    text = element_text(family = "Constantia", color = "#6BAED6"),
    plot.caption = element_text(hjust = 0.5, vjust = 10),
    plot.title = element_text(hjust = 0.5, vjust = -1.5, size = 35, face = "bold")
  )
p
ggsave(paste0("discontinuity.png"), width = 6, height = 7)

# LIRIDUM LARADUM
df <- get_data(2000)

p <- ggplot(df, aes(x, y)) +
  geom_bin2d(show.legend = F) +
  scale_fill_distiller(palette = "RdPu", direction = -1) +
  scale_x_continuous(breaks = NULL, limits = c(0, 2000), oob = scales::squish) +
  scale_y_continuous(breaks = NULL, limits = c(-1, NA), oob = scales::squish) +
  xlab(NULL) + ylab(NULL) +
  coord_polar("x", start = 7.83) +
  labs(caption = "Plot created by Christoph von Matt (@chvonmatt) | Data: self-generated dataset",
       title = "Liridum Laradum") +
  theme(
    plot.background = element_rect(fill = "white"),
    panel.background = element_rect(fill = "white"),
    text = element_text(family = "serif", color = "#D63F91"),
    plot.caption = element_text(hjust = 0.5, vjust = 15),
    plot.title = element_text(hjust = 0.5, vjust = -3, size = 30, face = "bold")
  )
p
ggsave(paste0("liridumlaradum.png"), width = 6, height = 7)