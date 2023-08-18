# Script to clip global bioclim variables to extent of Africa

library(tidyverse)
library(terra)
library(sf)
library(here)

# Load bioclim variable

bio01 <- rast(here("data", "raw", "raster", "WorldClim", "wc2.1_30s_bio_1.tif"))

summary(bio01)

# Load country vector

countries <- st_read(here("data", "raw", "vector", "natural_earth", "ne_110m_admin_0_countries_fixed.shp"))

africa <- filter(countries, CONTINENT == "Africa")

africa_vct <- vect(africa)

# Crop bioclim layer to Africa

bio01_africa <- crop(bio01, africa_vct)

plot(bio01_africa)

# Crop bioclim layer to Kenya/Tanzania

ke_tz <- filter(countries, NAME %in% c("Kenya", "Tanzania"))

bio01_ke_tz <- crop(bio01, ke_tz)

plot(bio01_ke_tz)

writeRaster(bio01_africa,
            here("data", "processed", "raster", "WorldClim", "bio_01_africa.tif"))

writeRaster(bio01_ke_tz,
            here("data", "processed", "raster", "WorldClim", "bio_01_ke_tz.tif"))
