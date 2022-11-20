# Code from https://gist.github.com/walkerke/2d4785622ee1852edeca0c4890d344bd

# libraries
library(tigris)
library(tidyverse)
library(sf)

smallwood <- places("WA", cb = TRUE) %>%
  filter(str_detect(NAME, "Winthrop"))

smallwood_roads <- roads("WA", "Okanogan County") %>%
  st_intersection(smallwood) %>%
  filter(st_is(., "LINESTRING"))

ggplot(smallwood_roads) + 
  geom_sf() + 
  theme_minimal(base_size = 16) + 
  labs(title = "Roads in Winthrop, WA")

