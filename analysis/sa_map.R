# Study area figure

# Setup

library(sf)
library(tidyverse)
library(tmap)
library(tmaptools)
library(rJava)
library(OpenStreetMap)
library(grid)
library(here)

# Load data


tz <- st_read(here("data", "raw", "vector", "natural_earth",
                   "ne_110m_admin_0_countries_fixed.shp")) %>%
  filter(NAME == "Tanzania") %>%
  select(NAME)

tpw <- st_read(here("data", "processed", "vector", "tpw", "tpw_locations.shp"))

tpw_bbox <- st_bbox(tpw)
bbox_buffered <- (tpw_bbox + c(-0.23, -0.23, 0.23, 0.23)) %>% st_as_sfc()

osm <- read_osm(bbox_buffered, type = "stamen-terrain", zoom = 9)

osm

tz_map <- tm_shape(tz) + tm_polygons(col = "#A2754E") + tm_text("NAME", size = 2) +
  tm_shape(bbox_buffered) + tm_polygons(col = "white", lwd = 1) +
  tm_shape(tpw) + tm_dots()

tz_map

map <- 
  tm_shape(osm) + tm_rgb() +
  tm_shape(tz) + tm_borders() +
  tm_shape(tpw) + tm_symbols(border.col = "#64643E", col = "#A2754E", size = 0.3, border.lwd = 2) +
  tm_shape(bbox_buffered, is.master = T) + tm_borders(col = "transparent") +
  tm_scale_bar(width = 0.2, position = c("right", "bottom"),
               text.size = 1, color.dark = "#64643E") +
  tm_compass(position = c("right", "top"), text.size = 1) +
  tm_layout (frame = FALSE, bg.color = "transparent")
  

map

print(tz_map, vp = viewport(0.3, 0.2, width = 0.25, height = 0.25))

tmap_save(map,
          insets_tm = tz_map, insets_vp = viewport(0.2, 0.3, width = 0.25, height = 0.25),
          filename = here("results", "figures", "tpw_map.png"),
          width = 9, height = 9, dpi = 600)
