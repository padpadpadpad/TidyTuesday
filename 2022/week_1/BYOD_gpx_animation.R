#--------------------------------#
# week 1 of #TidyTuesday 2021 ####
#--------------------------------#

# THE PLAN
# going to try put multiple gganimates side by side
# make a plot of a gpx ride where the frame rate is dictated by speed
# this is a GPX file exported from Strava

#------------------#
# load packages ####
#------------------#

library(tidyverse)
library(gpx)
library(cowplot)
library(janitor)
library(rStrava)
library(ggmap)
library(gganimate)
library(gifski)

# first read in the GPX file
d <- read_gpx('data/2022/week_1/Afternoon_Ride.gpx')

# look inside it
head(d)

# clean this up
d <- d$tracks$`Afternoon Ride`
d <- clean_names(d)

# need to calculate distance and speed

# calculate distance
d <- arrange(d, time) %>%
  mutate(., distance = get_dists(longitude, latitude),
         dist_diff = c(0, diff(distance)))

# remove distances that are too close together in terms of hits (i.e. this is traffic)
# calculate reveal time as being the inverse of speed. Quicker transitions when going faster
d <- filter(d, dist_diff > 0.002) %>%
  mutate(# convert seconds to hours
    time_diff = c(0,diff(time))/60/60,
    speed = dist_diff / time_diff,
    n = 1:n(),
    reveal_time = 1/speed,
    reveal_time = ifelse(!is.finite(reveal_time), 1/mean(speed, na.rm = TRUE), reveal_time),
    reveal_time = cumsum(reveal_time))

d$distance %>% max()

max(d$time) - min(d$time)

# calculate speed
hist(d$speed)

#------------------#
# make map plot ####
#------------------#

# get bounding box for the ride
bbox <- c(min(d$longitude) - 0.01, min(d$latitude) - 0.01, max(d$longitude) + 0.01, max(d$latitude) + 0.01)
names(bbox) <- c('left', 'bottom', 'right', 'top')

# get map
p <- get_stamenmap(bbox, zoom = 14, maptype = "terrain") %>% 
  ggmap() 

# make some things blank
p <- p +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        panel.border = element_rect(fill = NA)) 

# add on track
p1_static <- p +
  geom_path(aes(x = longitude, y = latitude), data = d, size = 2, col = 'red') +
  geom_point(aes(x = longitude, y = latitude), data = d, size = 4, shape = 21, fill = 'white', col = 'red') +
  theme(plot.margin = margin(0,0,0,0))

# add reveal
p1 <- p1_static +
  transition_reveal(reveal_time)

# animate plot
p1_gif <- gganimate::animate(p1, width = 7, height = 6, units = 'in', renderer = magick_renderer(), res = 150)

# save out static version to play with cowplot before rendering gif
ggsave('2022/week_1/p1.png', p1_static, width = 7, height = 6, units = 'in')


#--------------------------------#
# make elevation profile plot ####
#--------------------------------#

d2 <- select(d, -reveal_time)

# make plot
p2_static <- ggplot(d2, aes(distance, elevation)) +
  geom_ribbon(aes(ymax = elevation, ymin = 0), fill = 'grey75') +
  geom_line(data = d, col = 'red', size = 2) +
  geom_point(shape = 21, fill = 'white', data = d, size = 4, col = 'red') +
  labs(x = 'Distance (km)',
       y = 'Elevation (m)') +
  theme_minimal(base_size = 12) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line.y.left = element_line(colour = 'black'),
        axis.line.x.bottom = element_line(colour = 'black'),
        text = element_text(family = 'Lato'),
        plot.margin = margin(0,0,0,0)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.03)), limits = c(0,200), n.breaks = 5) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.03)), limits = c(0,20), breaks = c(5,10,15,20))

p2_static

# add reveal
p2 <- p2_static +
  transition_reveal(reveal_time)

# animate plot
p2_gif <- gganimate::animate(p2, width = 7, height = 2, units = 'in', renderer = magick_renderer(), res = 150)

# save out static version to play with cowplot before rendering gif
ggsave('2022/week_1/p2.png', p2_static, width = 7, height = 2, units = 'in')

#--------------------------#
# make cow plot of ride ####
#--------------------------#

plot_test <- plot_grid(ggdraw() + draw_image('2022/week_1/p1.png', scale = 0.95), 
                       ggdraw() + draw_image('2022/week_1/p2.png', scale = 0.95, hjust = 0.05, vjust = -0.4),
                       ncol=1, 
                       rel_heights = c(0.8, 0.2),
                       align = 'hv') +
  theme(plot.margin = margin(0.1, 0.75, 0, 0.75, unit="cm"),
        plot.background = element_rect(fill = 'white'),
        text = element_text(family = 'Lato'))

plot_test <- ggdraw(plot_test) +
  draw_label(label="My 10 mile (ish) time trial route", x=0.5, y=0.94, size=30, fontfamily = 'Lato', fontface = "bold", color = "black") +
  draw_label(label="Faster reveals mean I am zooming!", x=0.5, y=0.91, size=12, fontfamily = 'Lato', fontface = "bold", color = "black") +
  draw_label(label="Average speed: 30 kph | Date: 11/09/2021", x=0.5, y=0.89, size=12, fontfamily = 'Lato', fontface = "bold", color = "black") +
  draw_line(x=c(0, 0.08), y=c(0.94, 0.94), size=4, color= "black") +
  draw_line(x=c(0.92, 1), y=c(0.94, 0.94), size=4, color= "black") +
  draw_label(label = 'Map tiles by Stamen Design, under CC BY 3.0. Data by OpenStreetMap, under ODbL', y=0.045, x = 0.3, size=9, fontfamily = 'Lato', color = "black", hjust = 0) +
  draw_line(x=c(0, 0.28), y=c(0.045, 0.045), size=2, color= "black")

ggsave(
  filename = '2022/week_1/static_plot.png', plot_test, width = 7, height = 10, units = 'in')

#-----------------------------------#
# make gif of ride using cowplot ####
#-----------------------------------#

# used the example from here:
# 1. here for calculating reveal time: https://stackoverflow.com/questions/53092216/any-way-to-pause-at-specific-frames-time-points-with-transition-reveal-in-gganim
# 2. here for getting multiple gifs in the same plot: https://stackoverflow.com/questions/61908979/multiple-gganimate-plots-both-stacked-and-side-by-side#62126064

# plot gifs
tdir <- tempdir()

for(i in 1:100){
  
  plot_temp <- plot_grid(ggdraw() + draw_image(p1_gif[i], scale = 0.95), 
                         ggdraw() + draw_image(p2_gif[i], scale = 0.95, hjust = 0.05, vjust = -0.4),
                         ncol=1, 
                         rel_heights = c(0.8, 0.2),
                         align = 'hv') +
    theme(plot.margin = margin(0.1, 0.75, 0, 0.75, unit="cm"),
          plot.background = element_rect(fill = 'white'),
          text = element_text(family = 'Lato'))
  
  plot_temp2 <- ggdraw(plot_temp) +
    draw_label(label="My 10 mile (ish) time trial route", x=0.5, y=0.94, size=30, fontfamily = 'Lato', fontface = "bold", color = "black") +
    draw_label(label="Faster reveals mean I am zooming!", x=0.5, y=0.9, size=12, fontfamily = 'Lato', fontface = "bold", color = "black") +
    draw_label(label="Average speed: 30 kph | Date: 11/09/2021", x=0.5, y=0.88, size=12, fontfamily = 'Lato', fontface = "bold", color = "black") +
    draw_line(x=c(0, 0.08), y=c(0.94, 0.94), size=4, color= "black") +
    draw_line(x=c(0.92, 1), y=c(0.94, 0.94), size=4, color= "black") +
    draw_label(label = 'Map tiles by Stamen Design, under CC BY 3.0. Data by OpenStreetMap, under ODbL', y=0.045, x = 0.3, size=9, fontfamily = 'Lato', color = "black", hjust = 0) +
    draw_line(x=c(0, 0.28), y=c(0.045, 0.045), size=2, color= "black")
  
  ggsave(
    filename = file.path(tdir, paste0("out_", sprintf("%03d", i), ".png")),
    plot = plot_temp2, width = 7, height = 10, units = 'in', device = "png")
}

png_files <- sort(list.files(path = tdir, pattern = "out_", full.names = TRUE))

# save out gifski
gifski(png_files, gif_file = "2022/week_1/BYOD_gpx_animation.gif", width = 700, height = 1000, delay = .09,
       progress = TRUE)