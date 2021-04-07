library(ggplot2)
library(rayshader)
library(tidyverse)

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
