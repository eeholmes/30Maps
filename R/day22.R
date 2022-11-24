# Day 22
library(here)
library(tidyverse)
library(stringr)
library(sf)
library(ncdf4)

library(marmap)
library(tidyterra)
library(ggnewscale)
library(gganimate)
library(lubridate)


# Downloaded from https://dataselection.euro-argo.eu/
ncfile <- here::here("content", "data", "GL_PR_PF_6903766.nc")
argo <- ncdf4::nc_open(ncfile)
a <- ncdf4::ncvar_get(argo, "VERTICAL_SAMPLING_SCHEME")
good <- stringr::str_detect(a, "Primary sampling")


# Fix date
to <- as.POSIXlt("1950-01-01")
argo_df <- tibble(
  time = ncvar_get(argo, "TIME"),
  lon = ncvar_get(argo, "LONGITUDE"),
  lat = ncvar_get(argo, "LATITUDE"),
  temp = ncvar_get(argo, "TEMP")[1,],
  psal = ncvar_get(argo, "PSAL")[1,]
) %>% subset(good) %>%
  mutate(date = to+time*60*60*24) %>% # need to convert time to secs
  mutate(qtr = as.character(quarter(date)))

# Bounding box for bathymetry
y <- c(39, 47)
x <- c(26, 44)

# Get bathy
bathy <- marmap::getNOAA.bathy(x[1], x[2], y[1], y[2], resolution = 3)
bathy_df <- marmap::fortify.bathy(bathy)

# Test bathy plot
autoplot.bathy(bathy, geom=c("tile","contour")) +
    scale_fill_gradient2(low="dodgerblue4", mid="gainsboro", high="darkgreen")

# Make ggplot base map
basemap <- ggplot() +
  # water raster
  geom_raster(data = bathy_df %>% filter(z < 0), aes(x, y, fill = z), show.legend=FALSE) +
  scale_fill_hypso_c("etopo1_bathy") +
  # add contours
  geom_contour(data = bathy_df %>% filter(z < 0), 
               aes(x=x, y=y, z=z),
               breaks=c(-100),
               linewidth=c(0.3),
               colour="grey") +
  geom_contour(data = bathy_df %>% filter(z < 0), 
               aes(x=x, y=y, z=z),
               breaks=c(-500, -1000, -1500, -2000),
               linewidth=c(0.3),
               colour="white") +
  new_scale_fill() +
  # Add argo points
  geom_point(data = argo_df, aes(x = lon, y = lat, color = temp), size=3) +
  scale_colour_steps2(low = "blue", mid = "white", high = "red",
    midpoint = mean(argo_df$temp), name="temperature (C)") +
  new_scale_fill() +
  # land raster
  geom_raster(data = bathy_df %>% filter(z >= 0), aes(x, y, fill = z), show.legend=FALSE) +
  scale_fill_hypso_c("gmt_globe_hypso") +
  theme_void() +
  theme(
    legend.position = "bottom"
  ) + 
  ggtitle("An Argo bouy in the Black Sea")
basemap

# Make animation
argo.animate <- basemap +
 geom_label(
    data = argo_df %>% slice(seq(1,dim(argo_df)[1],14)),
    aes(label = format(date, "%b %Y")), x=35, y=43)+
  transition_time(date)+
  ease_aes("sine-in-out")+
  shadow_mark()

gganimate::animate(argo.animate)

gganimate::anim_save("argo.gif")

