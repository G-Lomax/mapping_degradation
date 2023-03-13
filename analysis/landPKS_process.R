###### LandPKS Extract ###############################################
###### Script to filter LandPKS data to a geographic region and ######
###### convert to a more streamlined data structure ##################
######################################################################

## Guy Lomax
## 26 September 2022
## G.Lomax@exeter.ac.uk

options(repos = c(CRAN = "https://cran.rstudio.org"))
.libPaths("C:/Program Files/Microsoft/R Open/R-4.0.2/library")

library(tidyverse)
library(lubridate)
library(sf)
library(tmap)

### Load data

landpks <- read_csv("raw_data/csv/landPKS/landCover.csv",
                    col_types = "ccddDTccccffflcccccddddddddddcddcddc")
  
ea_countries <- c("Kenya", "Tanzania")
ea <-
  st_read("raw_data/vector/natural_earth/ne_110m_admin_0_countries_fixed.shp") %>%
  select("NAME", "geometry") %>%
  filter(NAME %in% ea_countries) %>%
  rename(country = NAME)

#### Filter landPKS points to Kenya and Tanzania
landpks_ea <- landpks %>%
  st_as_sf(crs = 4326, coords = c("Longitude", "Latitude"), remove = FALSE) %>%
  st_join(ea, left = FALSE)

### Clean data

# Remove rows with data entry errors and drop unneeded columns
landpks_ea_clean <- landpks_ea %>%
  filter(is.na(Notes)) %>%
  select(-c(RecorderName, DateCreated_GMT, CanopyHeight, BasalGap,
            Species1, SegmentSpecies1Density, PlotSpecies1Density,
            Species2, SegmentSpecies2Density, PlotSpecies2Density,
            Notes))

# Summarise into plot-level data
landpks_plots <- landpks_ea_clean %>%
  group_by(Name, Latitude, Longitude) %>%
  mutate(id = cur_group_id()) %>%
  group_by(Name, Latitude, Longitude, id, ObservationDate_GMT) %>%
  summarise(
    n_points = n(),
    across(.cols = starts_with("Dominant"), first),
    across(.cols = ends_with("Count"), sum)
  )

sum(landpks_plots$n_points == 40)

# 26 entries have duplicate data (40 rows instead of 20)
# 3 entries have triplicate data (60 rows)
# 1 entry has quadruple-entered data (80 rows!)
# 81 entries have incomplete or ambiguous data (< 20 rows or > 20)

# Remove incomplete/ambiguous data and rescale duplicate data
landpks_plots_clean <- landpks_plots %>%
  filter((n_points %% 20) == 0) %>%
  mutate(across(ends_with("Count"),
                function(x) x * 20 / n_points))

#### Export key data as csv

landpks_plots_clean %>%
  rename_with(function(x) substring(x, 8), .cols = ends_with("Count")) %>%
  select(-c(PlantBaseCount, ousLitterCount, WoodyLitterCount, RockCount)) %>%
  st_write("processed_data/vector/landPKS/land_cover_plot.geojson")
