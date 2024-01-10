## Topographic Wetness Index from DEM
## 20th March 2023
## Guy Lomax
## G.Lomax@exeter.ac.uk

library(terra)
library(sf)
library(tidyverse)
library(tmap)
library(here)
library(whitebox)
library(raster)

tmap_mode("view")

# Load data and join

dem <- rast(here("data", "raw", "raster", "hydroSHEDS", "hydroshedsDem.tif"))

dem_reproj <- project(dem, "ESRI:54034")

# Using whitebox and raster package
whitebox::wbt_init()

writeRaster(dem_reproj,
            here("data", "processed", "raster", "hydroSHEDS", "hydroshedsDemReproj.tif"),
            overwrite = TRUE)

# Slope

wbt_slope(
  dem = here("data", "processed", "raster", "hydroSHEDS", "hydroshedsDemReproj.tif"),
  output = here("data", "processed", "raster", "hydroSHEDS", "hydroshedsSlope.tif")
)

slope <- rast(here("data", "processed", "raster", "hydroSHEDS", "hydroshedsSlope.tif"))

# Correct to replace zero slope cells with small values (0.1 deg)
# Zero slopes cause NA values in TWI calculation.
# 0.1 degrees is below minimum resolvable slope using dem (0.6 deg)

slope_corrected <- classify(slope, matrix(c(-0.1, 0.1, 0.1), nrow = 1))

writeRaster(slope_corrected, here("data", "processed", "raster", "hydroSHEDS",
                                  "hydroshedsSlopeCorrected.tif"))

# TWI with Freeman D8 flow accumulation algorithm
wbt_fd8_flow_accumulation(
  dem = here("data", "processed", "raster", "hydroSHEDS", "hydroshedsDemReproj.tif"),
  output = here("data", "processed", "raster", "hydroSHEDS", "hydrosheds_fa_fd8.tif"),
  out_type = "specific contributing area",
  threshold = 3000  # 
)

fa_fd8 <- raster(here("data", "processed", "raster", "hydroSHEDS", "hydrosheds_fa_fd8.tif"))

wbt_wetness_index(
  sca = here("data", "processed", "raster", "hydroSHEDS", "hydrosheds_fa_fd8.tif"),
  slope = here("data", "processed", "raster", "hydroSHEDS", "hydroshedsSlopeCorrected.tif"),
  output = here("data", "processed", "raster", "hydroSHEDS", "hydrosheds_twi_fd8.tif")
)

twi_fd8 <- rast(here("data", "processed", "raster", "hydroSHEDS", "hydrosheds_twi_fd8.tif"))

plot(twi_fd8, xlim = c(30, 30.5), ylim = c(-5, -4.5))

# # Infinite flow accumulation
# 
# wbt_md_inf_flow_accumulation(
#   dem = here("data", "processed", "raster", "hydroSHEDS", "hydroshedsDemReproj.tif"),
#   output = here("data", "processed", "raster", "hydroSHEDS", "hydrosheds_fa_dinf.tif"),
#   out_type = "specific contributing area",
#   threshold = 3000
# )
# 
# wbt_wetness_index(
#   sca = here("data", "processed", "raster", "hydroSHEDS", "hydrosheds_fa_dinf.tif"),
#   slope = here("data", "processed", "raster", "hydroSHEDS", "hydroshedsSlopeCorrected.tif"),
#   output = here("data", "processed", "raster", "hydroSHEDS", "hydrosheds_twi_dinf.tif")
# )
# 
# twi_dinf <- rast(here("data", "processed", "raster", "hydroSHEDS", "hydrosheds_twi_dinf.tif"))
# 
# plot(twi_dinf, xlim = c(30, 30.5), ylim = c(-5, -4.5))

# Extract twi (using Freeman D8 following Kopecky et al. (2021)) at sample point locations

# Reproject to WGS 84 lat/lon
twi_fd8_wgs84 <- project(twi_fd8, dem)

sample_points_raw <- st_read(here("data", "raw", "vector", "coarseScaleSample.geojson"))

sample_points_twi <- terra::extract(twi_fd8_wgs84, sample_points_raw)
head(sample_points_twi)

write_csv(sample_points_twi,
          here("data", "processed", "csv", "twi_sample.csv"))
