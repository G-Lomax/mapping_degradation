## Calculation of the Standardised Precipitation Evapotranspiration Index (SPEI)
## from covariate layers in the study region.

## 6th September 2023
## Guy Lomax
## G.Lomax@exeter.ac.uk

library(terra)
library(SPEI)
library(purrr)
library(parallel)
library(here)

nc <- 4

# Load

water_bal_paths <- Sys.glob(here("data", "raw", "raster", "spei", "speiBands*.tif"))

# Calculate SPEI in each pixel, looping through files

calc_spei <- function(raster, scale, na.rm = TRUE) {
  spei_object <- SPEI::spei(raster, scale, na.rm = na.rm, verbose = FALSE)
  spei_object$fitted
}

spei_rast_list <- list(length = length(water_bal_paths))

for (i in seq_len(length(water_bal_paths))) {
  cat("Processing file ", i, " of ", length(water_bal_paths), "...\n")
  
  water_bal_rast <- rast(water_bal_paths[i])
  
  spei_rast <- terra::app(water_bal_rast, calc_spei, scale = 12, cores = nc)
  
  names(spei_rast) <- month_names
  
  spei_rast_list[[i]] <- spei_rast
  
  cat("Processing file ", i, "complete\n")
}

# Rename months with sensible names
year_names <- seq(1991, 2020)

band_names <- map(year_names, function(y) paste0(month.abb, "_", y)) %>%
  unlist()

spei_rast_renamed <- map(spei_rast_list, function(rast) {
  names(rast) <- band_names
  rast
})

# Write interim rasters in case I need them later
for (i in 7:length(spei_rast_renamed)) {
  filename <- paste0("spei_full_raster_", i, ".tif")
  writeRaster(spei_rast_renamed[[i]],
              here("data", "processed", "raster", "spei", filename),
              overwrite = TRUE)
}

# Filter to August only and during study period only

study_bands <- band_names[grepl("Aug", band_names)][11:29]
study_bands

spei_rast_aug_study <- map(spei_rast_renamed, terra::subset, subset = study_bands)

# Merge to single raster and save to file

spei_merged <- do.call(terra::merge, spei_rast_aug_study)

writeRaster(spei_merged,
            here("data", "processed", "raster", "spei", "study_spei.tif"))

# Extract SPEI values for sample points

library(sf)
library(dplyr)

points <- st_read(here("data", "raw", "vector", "coarse", "coarseScaleSample.geojson")) %>%
  dplyr::select(id, geometry)

points_spei <- terra::extract(spei_merged, points) %>%
  bind_cols(points)

head(points_spei)

# Save to file
readr::write_csv(points_spei %>% st_drop_geometry(),
          here("data", "processed", "vector", "spei_sample.csv"))
