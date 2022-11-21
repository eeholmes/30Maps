# libraries
require(devtools)
devtools::install_github("ropensci/rerddap")
devtools::install_github("rmendels/rerddapXtracto") 

# Download the SST data
filePath <- here::here("content", "data", "day7_sst_raster.RData")

theday <- "2021-06-19"
lats <- c(40.375, 50.375)
lons <- c(-141.875, -120.875)
if (!file.exists(filePath)) {
  # look at this to figure out how to spec the next line for downloading
  df_info <- rerddap::info("ncdcOisst21Agg_LonPM180")
  df <- rerddap::griddap("ncdcOisst21Agg_LonPM180", latitude = lats, longitude = lons, time = c(theday, theday), fields = "sst")$data
  save(df, file=filePath)
}else{
  load(filePath)
}

# reformat and make into a raster
df2 <- data.frame(x=df$lon, y=df$lat, z=df$sst)
ras <- raster::rasterFromXYZ(df2, crs = "+proj=longlat")

# get the coastline
coast <- rnaturalearth::ne_coastline(scale = 50, returnclass = "sp")
wa_or_coast <- raster::crop(coast, raster::extent(lons[1], lons[2], lats[1], lats[2]))

# Plot
library(raster)
library(ggplot2)
ggplot(df) +
  geom_raster(aes(longitude, latitude, fill = sst)) +
  scale_fill_gradient2(midpoint = mean(df$sst, na.rm = TRUE),
                       low = "blue",
                       mid = "white",
                       high = "red") +
  labs(x = NULL,
       y = NULL,
       fill = "Celcius",
       title = paste("Sea Surface Temperature (SST) on", theday)) +
  theme_bw() +
  scale_x_continuous(limits = lons, expand = c(-0.01, -0.01)) +
  scale_y_continuous(limits = lats, expand = c(-0.01, -0.01)) +
  geom_path(data=wa_or_coast,  aes(x=long,y=lat, grouping=id), size=1, na.rm=TRUE)
