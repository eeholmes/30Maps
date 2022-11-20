# libraries ----
library(tidyverse)
library(sf)

# Center and circle ----
city_coords <- tibble(address = "Mazama, Washington") |> 
  tidygeocoder::geocode(address, method = 'osm', long = long, lat = lat)
long <- city_coords$long[1]
lat <- city_coords$lat[1]
crs2 <- 6384 # https://epsg.io/6384
mazama_center <-
  tibble(long, lat) %>%
  st_as_sf(coords = c("long", "lat"), crs = 4326)

city_coords <- tibble(address = "Winthrop, Washington") |> 
  tidygeocoder::geocode(address, method = 'osm', long = long, lat = lat)
long <- city_coords$long[1]
lat <- city_coords$lat[1]
crs2 <- 6384 # https://epsg.io/6384
winthrop_center <-
  tibble(long, lat) %>%
  st_as_sf(coords = c("long", "lat"), crs = 4326)

# circle to crop in ----
dist <-  50000
circle <- tibble(long, lat) %>%
  st_as_sf(coords = c("long", "lat"), crs = 4326) %>%
  st_transform(crs = crs2) %>%
  st_buffer(dist = dist) %>%
  st_transform(crs = 4326)

# elevations ----
library(tidyterra)
library(geodata)
library(scales)
r <- elevation_30s(country = "USA", path = tempdir())
names(r) <- "alt"

r <- crop(r, circle)
r <- mask(r, circle, inverse=FALSE)

slope <- terrain(r, "slope", unit = "radians")
aspect <- terrain(r, "aspect", unit = "radians")
hill <- shade(slope, aspect, 30, 270)
hill <- mask(hill, circle, inverse=FALSE)
aspect <- mask(aspect, circle, inverse=FALSE)
slope <- mask(slope, circle, inverse=FALSE)

names(hill) <- "shades"

# Hill shading, but we need a palette
pal_greys <- hcl.colors(1000, "Grays")

index <- hill %>%
  mutate(index_col = rescale(shades, to = c(1, length(pal_greys)))) %>%
  mutate(index_col = round(index_col)) %>%
  pull(index_col)

# Get cols
vector_cols <- pal_greys[index]

# make plot ----
ggplot() +
  geom_spatraster(data = hill, fill=vector_cols, maxcell = Inf) +
  geom_spatraster(data=r, maxcell = Inf) +
  scale_fill_hypso_tint_c(limits = as.vector(minmax(r)), 
                          palette = "dem_poster",
                          alpha =0.8,
                          labels = scales::label_comma(),
                          breaks = c(seq(0,1000, 250), seq(2000, 6000, 1000))) +
  guides(fill=guide_legend(title = "elevation", reverse = TRUE)) +
  labs(title = "Methow Valley, WA") +
  geom_sf(data = circle, color = "black", fill =  NA, linewidth = 2) +
  geom_sf(data=winthrop_center) + 
  geom_text_repel(data = winthrop_center, aes(x=long, y=lat), label="Winthrop", 
        fontface = "bold", nudge_x = .1, nudge_y = 0.1) +
  geom_sf(data=mazama_center) + 
  geom_text_repel(data = mazama_center, aes(x=st_coordinates(mazama_center)[1], y=st_coordinates(mazama_center)[2]), label="Mazama", 
        fontface = "bold", nudge_x = .1, nudge_y = 0.1) +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5, size = 30))

