# ----- libraries
library(tidyverse)
library(sf)
library(tidyterra) # for geom_spatraster
library(geodata)
library(scales)

# ----- Center
city_coords <- tibble(address = "Seattle, Washington") |> 
  tidygeocoder::geocode(address, method = 'osm', long = long, lat = lat)
long <- city_coords$long[1]
lat <- city_coords$lat[1]
crs2 <- 6384 # https://epsg.io/6384
coord_center <-
  tibble(long, lat) %>%
  sf::st_as_sf(coords = c("long", "lat"), crs = 4326)

crs_string <- paste0("+proj=ortho +lat_0=", lat, " +lon_0=", long)

# ----- elevations and hill shading
r <- geodata::elevation_global(res=10, path = tempdir())
names(r) <- "alt"

slope <- terra::terrain(r, "slope", unit = "radians")
aspect <- terra::terrain(r, "aspect", unit = "radians")
hill <- terra::shade(slope, aspect, 30, 270)

names(hill) <- "shades"


# ----- blue circle to make the ocean
# these are sf functions
ocean <- st_point(x = c(0,0)) %>%
  st_buffer(dist = 6371000) %>%
  st_sfc(crs = crs_string)


# ----- base plot
library(ggfx) # for with_shadow
p1 <- ggplot() +
  with_shadow(geom_sf(data = ocean, fill = "#BBDEFB", color = NA), sigma = 30, x_offset = 25, y_offset = 25, color = "#58595d") +
  geom_spatraster(data = hill, aes(fill=shades), maxcell = Inf) +
  geom_spatraster(data=r, maxcell = Inf) +
  scale_fill_hypso_tint_c(limits = as.vector(minmax(r)), 
                          palette = "dem_poster",
                          alpha =0.8,
                          labels = scales::label_comma(),
                          breaks = c(seq(0,1000, 250), seq(2000, 6000, 1000))) +
  guides(fill=guide_legend(title = "elevation", reverse = TRUE)) +
  coord_sf(crs = crs_string) +
  theme_void()
p1


# ----- annotate plot
library("ggrepel")
p2 <- p1 + geom_sf(data=coord_center) + 
  geom_text_repel(data = coord_center, aes(x=st_coordinates(coord_center)[1], y=st_coordinates(coord_center)[2]), label="Seattle", 
        nudge_x = -3371000, nudge_y = -1371000) +
  labs(
    title = "Globe with elevations",
    caption = "Graphic: EE Holmes"
  ) +
  theme(
    plot.background = element_rect(fill = "grey97", color = NA),
    plot.margin = unit(c(.5,.5,.5,.5), "cm"),
    plot.title = element_text(hjust = 0.045, size = 26, face = "bold")
  )
p2

# ----- save
ggsave(paste0("globe_elevation", ".png"), dpi = 320, width = 8, height = 8)

