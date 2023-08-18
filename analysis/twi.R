## Topographic Wetness Index from DEM
## 20th March 2023
## Guy Lomax
## G.Lomax@exeter.ac.uk

library(raster)
library(terra)
library(sf)
library(tidyverse)
library(tmap)
library(whitebox)
library(here)

whitebox::wbt_init()

tmap_mode("view")

# Load data and join

# dem_paths <- Sys.glob(here("data", "raw", "raster", "srtm", "*.tif"))
# 
# dem_segments <- lapply(dem_paths, raster)
# 
# dem <- do.call(merge, dem_segments)
# 
# writeRaster(dem, here("data", "processed", "raster", "srtm", "srtm_ke_tz.tif"))

dem <- raster(here("data", "processed", "raster", "srtm", "srtm_ke_tz.tif"))

# Fill depressions


Sys.time()
wbt_breach_depressions_least_cost(
  dem = here("data", "processed", "raster", "srtm", "srtm_ke_tz.tif"),
  output = here("data", "processed", "raster", "srtm", "srtm_ke_tz_breached.tif"),
  dist = 10,
  fill = FALSE)
Sys.time()


wbt_fill_depressions(
  dem = here("data", "processed", "raster", "srtm",
             "srtm_sa_breached.tif"),
  output = here("data", "processed", "raster", "srtm",
                "srtm_sa_breached_filled.tif")
)

breached_dem <- raster(here("data", "processed", "raster", "srtm",
                          "srtm_sa_breached.tif"))

breached_dem_zoom <- crop(breached_dem, bbox)

filled_dem <- raster(here("data", "processed", "raster", "srtm",
                          "srtm_sa_breached_filled.tif"))

filled_dem_zoom <- crop(filled_dem, bbox)


# Plot hillshades

wbt_hillshade(dem = here("data", "raw", "raster", "srtm",
                         "srtm_sa.tif"),
              output = here("data", "processed", "raster", "srtm",
                            "srtm_sa_hillshade.tif")
)

wbt_hillshade(dem = here("data", "processed", "raster", "srtm",
                         "srtm_sa_breached_filled.tif"),
              output = here("data", "processed", "raster", "srtm",
                            "srtm_sa_breached_filled_hillshade.tif")
)

tm_shape(dem_zoom) +
  tm_raster(style = "cont", palette = "-PuOr", breaks = seq(1000, 2600, 200)) +
  tm_shape(breached_dem_zoom) +
  tm_raster(style = "cont", palette = "-PuOr", breaks = seq(1000, 2600, 200), legend.show = F) +
  tm_shape(filled_dem_zoom) +
  tm_raster(style = "cont", palette = "-PuOr", breaks = seq(1000, 2600, 200), legend.show = F)

hillshade <- raster(here("data", "processed", "raster", "srtm",
                         "srtm_sa_hillshade.tif"))

hillshade_breached <- raster(here("data", "processed", "raster", "srtm",
                                "srtm_sa_breached_hillshade.tif"))


hillshade_zoom <- crop(hillshade, bbox)
hillshade_breached_zoom <- crop(hillshade_breached, bbox)

tm_shape(hillshade_zoom) + tm_raster(palette = "-Greys")
tm_shape(hillshade_filled_zoom) + tm_raster(palette = "-Greys")

difference <- dem - filled_dem
difference[difference == 0] <- NA

difference_zoom <- crop(difference, bbox)

tm_shape(hillshade_zoom) + tm_raster(palette = "-Greys") +
  tm_shape(difference_zoom) + tm_raster()

# Slope

wbt_slope(
  dem = here("data", "processed", "raster", "srtm",
             "srtm_sa_breached.tif"),
  output = here("data", "processed", "raster", "srtm",
              "srtm_slope.tif")
)

slope <- raster(here("data", "processed", "raster", "srtm",
                     "srtm_slope.tif"))

slope_zoom <- crop(slope, bbox)

# TWI with Freeman D8 flow accumulation algorithm
wbt_fd8_flow_accumulation(
  dem = here("data", "processed", "raster", "srtm",
             "srtm_sa_breached.tif"),
  output = here("data", "processed", "raster", "srtm",
                "fd8fa.tif"),
  out_type = "specific contributing area",
  threshold = 3000  # 
)


fd8fa <- raster(here("data", "processed", "raster", "srtm",
                     "fd8fa.tif"))

fd8fa_zoom <- crop(fd8fa, bbox)

wbt_wetness_index(
  sca = here("data", "processed", "raster", "srtm",
             "fd8fa.tif"),
  slope = here("data", "processed", "raster", "srtm",
               "srtm_slope.tif"),
  output = here("data", "processed", "raster", "srtm",
                "twi_fd8.tif")
)

twi_fd8 <- raster(here("data", "processed", "raster", "srtm",
                       "twi_fd8.tif"))

twi_fd8_zoom <- crop(twi_df8, bbox)

tm_shape(hillshade_zoom)+
  tm_raster(style = "cont",palette = "-Greys", legend.show = FALSE)+
  tm_shape(twi_fd8_zoom)+
  tm_raster(style = "cont", palette = "PuOr", legend.show = TRUE, alpha = 0.5)+
  tm_scale_bar()

# Clean and plot TWI
twi_fd8[twi_fd8 > 10] <- NA

twi_fd8_zoom <- crop(twi_fd8, bbox)

tm_shape(hillshade_zoom) + tm_raster(style = "cont", palette = "-Greys", legend.show = F) +
  tm_shape(log(fd8fa_zoom)) + tm_raster(style = "cont", palette = "RdBu", alpha= 0.6) +
  tm_shape(twi_fd8_zoom) + tm_raster(style = "cont", palette = "PuOr", alpha = 0.6,
                                     breaks = seq(-6, 0, 1))
