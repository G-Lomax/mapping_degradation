## Topographic Wetness Index from DEM
## 20th March 2023
## Guy Lomax
## G.Lomax@exeter.ac.uk

library(raster)
library(sf)
library(whitebox)
library(tmap)
library(here)

whitebox::wbt_init()

# Load data

dem <- raster(here("data", "raw", "raster", "srtm",
                   "srtm_sa.tif"))

# Fill depressions

wbt_breach_depressions_least_cost(
  dem = here("data", "raw", "raster", "srtm",
             "srtm_sa.tif"),
  output = here("data", "processed", "raster", "srtm",
                "srtm_sa_breached.tif"),
  dist = 5,
  fill = FALSE)

wbt_fill_depressions(
  dem = here("data", "processed", "raster", "srtm",
             "srtm_sa_breached.tif"),
  output = here("data", "processed", "raster", "srtm",
                "srtm_sa_breached_filled.tif"),
  max_depth = 10
)

# Slope

wbt_slope(
  dem = here("data", "processed", "raster", "srtm",
             "srtm_sa_breached_filled.tif"),
  output = here("data", "processed", "raster", "srtm",
              "srtm_slope.tif")
)

# TWI with Freeman D8 flow accumulation algorithm
wbt_fd8_flow_accumulation(
  dem = here("data", "processed", "raster", "srtm",
             "srtm_sa_breached_filled.tif"),
  output = here("data", "processed", "raster", "srtm",
                "fd8fa.tif"),
  out_type = "specific contributing area",
  threshold = 3000  # 
)

wbt_wetness_index(
  sca = here("data", "processed", "raster", "srtm",
             "fd8fa.tif"),
  slope = here("data", "processed", "raster", "srtm",
               "srtm_slope.tif"),
  output = here("data", "processed", "raster", "srtm",
                "twi_fd8.tif")
)


# View

# tmap_mode("view)")
# twi <- raster(here("data", "processed", "raster", "srtm",
#                    "twi_fd8.tif"))
# bbox <- st_bbox(c(xmin = 35.5, xmax = 36.5, ymin = -2, ymax = -1.5))
# 
# twi_zoom <- crop(twi, bbox)
# 
# tm_shape(twi_zoom) + tm_raster(style = "cont", palette = "RdBu", alpha = 0.7,
#                                breaks = seq(-6, 4, 1), midpoint = NA)
