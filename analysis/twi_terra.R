## Topographic Wetness Index from DEM
## 20th March 2023
## Guy Lomax
## G.Lomax@exeter.ac.uk

library(terra)
library(sf)
library(tidyverse)
library(tmap)
library(dynatopmodel)
library(here)
library(whitebox)
library(raster)

tmap_mode("view")

# Load data and join

dem <- rast(here("data", "raw", "raster", "hydroSHEDS", "hydroshedsDem.tif"))

dem_reproj <- project(dem, "ESRI:54034")

# Try direct calculation of TWI
# Edit dynatopmodel::upslope.area() function to use terra rather than raster functions

upslope <- function (dem, log = TRUE, atb = FALSE, deg = 0.1, fill.sinks = TRUE) 
{
  if (!all.equal(xres(dem), yres(dem))) {
    stop("Raster has differing x and y cell resolutions. Check that it is in a projected coordinate system (e.g. UTM) and use raster::projectRaster to reproject to one if not. Otherwise consider using raster::resample")
  }
  if (fill.sinks) {
    capture.output(dem <- invisible(terra::setValues(dem, 
                                                      topmodel::sinkfill(terra::as.matrix(dem), res = xres(dem), 
                                                                         degree = deg))))
  }
  topidx <- topmodel::topidx(terra::as.matrix(dem), res = xres(dem))
  a <- terra::setValues(dem, topidx$area)
  if (log) {
    a <- log(a)
  }
  if (atb) {
    atb <- terra::setValues(dem, topidx$atb)
    a <- c(a, atb)
    names(a) <- c("a", "atb")
  }
  return(a)
}

# Run functions
Sys.time()
twi <- upslope(dem_reproj, atb = TRUE)
Sys.time()

plot(twi_agg$atb)

writeRaster(twi_agg, here("data", "processed", "raster", "twi_topmodel.tif"))

# Using whitebox and raster package
whitebox::wbt_init()

dem_r <- raster::raster(dem_agg)

raster::writeRaster(dem_r, here("data", "processed", "raster", "srtm", "dem_agg.tif"))

Sys.time()
wbt_breach_depressions_least_cost(
  dem = here("data", "processed", "raster", "srtm", "dem_agg.tif"),
  output = here("data", "processed", "raster", "srtm", "dem_agg_breached.tif"),
  dist = 10,
  fill = TRUE)
Sys.time()

wbt_fill_depressions(
  dem = here("data", "processed", "raster", "srtm", "dem_agg_breached.tif"),
  output = here("data", "processed", "raster", "srtm", "dem_agg_breached_filled.tif")
)

breached_dem <- raster(here("data", "processed", "raster", "srtm", "dem_agg_breached.tif"))

plot(dem_agg)
plot(breached_dem)

difference <- dem_r - breached_dem
plot(difference)

# Slope

wbt_slope(
  dem = here("data", "processed", "raster", "srtm", "dem_agg.tif"),
  output = here("data", "processed", "raster", "srtm", "dem_agg_slope.tif")
)

slope <- raster(here("data", "processed", "raster", "srtm", "dem_agg_slope.tif"))

# TWI with Freeman D8 flow accumulation algorithm
wbt_fd8_flow_accumulation(
  dem = here("data", "processed", "raster", "srtm", "dem_agg_breached.tif"),
  output = here("data", "processed", "raster", "srtm", "dem_agg_fa_fd8.tif"),
  out_type = "specific contributing area",
  threshold = 3000  # 
)

fa_fd8 <- raster(here("data", "processed", "raster", "srtm", "dem_agg_fa_fd8.tif"))

wbt_wetness_index(
  sca = here("data", "processed", "raster", "srtm", "dem_agg_fa_fd8.tif"),
  slope = here("data", "processed", "raster", "srtm", "dem_agg_slope.tif"),
  output = here("data", "processed", "raster", "srtm", "twi_fd8.tif")
)

twi_fd8 <- raster(here("data", "processed", "raster", "srtm", "twi_fd8.tif"))

plot(twi_fd8, xlim = c(30, 30.5), ylim = c(-5, -4.5))

# Infinite flow accumulation

wbt_d_inf_flow_accumulation(
  input = here("data", "processed", "raster", "srtm", "dem_agg_breached.tif"),
  output = here("data", "processed", "raster", "srtm", "dem_agg_fa_dinf.tif"),
  out_type = "specific contributing area",
  threshold = 3000
)

wbt_wetness_index(
  sca = here("data", "processed", "raster", "srtm", "dem_agg_fa_dinf.tif"),
  slope = here("data", "processed", "raster", "srtm", "dem_agg_slope.tif"),
  output = here("data", "processed", "raster", "srtm", "twi_dinf.tif")
)

twi_dinf <- raster(here("data", "processed", "raster", "srtm", "twi_dinf.tif"))

plot(twi_dinf, xlim = c(30, 30.5), ylim = c(-5, -4.5))

# Find stream channels and valleys

wbt_extract_streams(
  flow_accum = here("data", "processed", "raster", "srtm", "dem_agg_fa_dinf.tif"),
  output = here("data", "processed", "raster", "srtm", "dem_streams.tif"),
  threshold = 2
)

wbt_extract_valleys(
  dem = here("data", "processed", "raster", "srtm", "dem_agg_breached.tif"),
  output = here("data", "processed", "raster", "srtm", "dem_valleys.tif")
)
