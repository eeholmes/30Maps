# libraries
library(sp)
library(raster)
library(maps)
library(dismo) # a sdm package used to download records
library(here)

# get shape files
NHVT <- raster::extent(-73.61056, -70.60205, 42.48873, 45.37969)
usashp <- raster::getData('GADM', country='USA', level=1, path="data")
nhvtshp <- subset(usashp, NAME_1 %in% c("New Hampshire", "Vermont"))

# Download Trillium records
grandiflorum <- dismo::gbif("Trillium",
                            species = "grandiflorum",
                            nrecs = 300, geo = TRUE,
                            removeZeros = TRUE, ext = NHVT
)
undulatum <- dismo::gbif("Trillium",
                         species = "undulatum",
                         nrecs = 300, geo = TRUE,
                         removeZeros = TRUE, ext = NHVT
)
colsWeNeed <- c("species", "lat", "lon", "locality", "year", "coordinateUncertaintyInMeters", "occurrenceID", "occurrenceRemarks", "geodeticDatum")
grandiflorum <- subset(grandiflorum, select = colsWeNeed)
undulatum <- subset(undulatum, select = colsWeNeed)
trillium.raw <- rbind(grandiflorum, undulatum)

# Clean up records
trillium <- trillium.raw
sp::coordinates(trillium) <- c("lon", "lat")
# uncertainty under 200m
good <- which(trillium$coordinateUncertaintyInMeters < 200)
trillium <- trillium[good, ]

# plot the records
plot(nhvtshp, border = "blue", axes = TRUE)
plot(subset(trillium, species == "Trillium grandiflorum"), pch = 19, cex = 0.25, add = TRUE)
plot(subset(trillium, species == "Trillium undulatum"), pch = 19, cex = 0.25, col = "red", add = TRUE)
title("Trillium grandiflorum (black) and Trillium undulatum (red) records", cex.main=0.75)
