library(ggplot2)
library(rayshader)
library(tidyverse)
library(extrafont)

# load fonts
#font_import()
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

df <- get_data(2000)

df_lt <- df %>% filter(y < 1500)

subset <- df[sample(1:4000000, 5000),]

p <- ggplot(subset, aes(x, y)) + #color = y
  #geom_bin2d(show.legend = F, binwidth = 20) + # no bindwidth (colosseum), binwith = 20 (stripes)
  #geom_point(show.legend = F) +
  geom_line(aes(group = y)) +
  #scale_fill_distiller() + # RdBu, RdPu
  #scale_fill_gradient(low = "yellow", high = "grey50") +
  scale_y_continuous(breaks = NULL, expand = c(0,0)) +
  scale_x_continuous(breaks = NULL, expand = c(0,0)) +
  xlab(NULL) + ylab(NULL) +
  coord_polar("x") +
  theme(
    plot.background = element_blank(),
    panel.background = element_blank()
  )
p

plot_gg(p, multicore = TRUE, zoom = 0.5)
render_snapshot()


# FLOWER POWER
# variations: palette: RdBu + RdPu / color:  + 
df2 <- get_data(18*15)

cols <- c("#D7624B", "#77256b", "#9966A9", "#E89838", "#70086E", "#9C010E", "#084594", "#005A32", "#1362A4", "#2D2D2D")
pals <- c("RdBu", "RdPu", "PRGn", "PuOr", "BuPu", "Reds", "Blues", "Greens", "GnBu", "Greys")

for(i in seq(length(pals))){
  p <- ggplot(df2, aes(x, y, color = -y, group = y)) + #color = y
    geom_area(lwd = 4, show.legend = F) +
    scale_color_distiller(palette = pals[i]) +
    scale_y_continuous(breaks = NULL) +
    scale_x_continuous(breaks = NULL) +
    xlab(NULL) + ylab(NULL) +
    coord_polar("x") +
    labs(caption = "Plot created by Christoph von Matt (@chvonmatt) | Data: self-generated dataset",
         title = "Flower Power") +
    theme(
      plot.background = element_rect(fill = "white"),
      panel.background = element_blank(),
      text = element_text(family = "Comic Sans MS", color = cols[i]),
      plot.caption = element_text(hjust = 0.5, vjust = 15),
      plot.title = element_text(hjust = 0.5, vjust = -3, size = 25, face = "bold")
    )
  p
  ggsave(paste0("flower_power_", pals[i], ".png"))
}

# plot_gg(p, multicore = TRUE, zoom = 0.5)
# render_snapshot()
