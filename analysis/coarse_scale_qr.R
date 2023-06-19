library(tidyverse)
library(sf)
library(here)
library(quantreg)
library(qgam)
library(parallel)
library(tmap)
library(gratia)
library(mgcViz)

nc <- 4
cl <- makeCluster(nc)

# Load data

countries <- st_read(here("data", "raw", "vector", "natural_earth",
                          "ne_110m_admin_0_countries_fixed.shp"))

ke_tz <- countries %>% filter(NAME %in% c("Kenya", "Tanzania"))

sample_points_raw <- st_read(here("data", "raw", "vector", "coarse",
                                  "coarseScaleSample.geojson"))


sample_points <- sample_points_raw %>%
  pivot_longer(cols = starts_with(c("GPP","precipitation")),
               names_to = "index", values_to = "value") %>%
  separate_wider_delim(index, "_", names = c("data", "year")) %>%
  pivot_wider(names_from = data, values_from = value)

head(sample_points, n = 20)

sample_points %>%
  filter(shrubland > 0.8 & year == 2008) %>%
  pivot_longer(cols = c(insolation, meanT, sand, slope, precipitation, landform),
               names_to = "var", values_to = "value") %>%
  ggplot(aes(x = value, y = GPP)) +
  geom_point(size = 0.5) +
  facet_wrap(~var, scales = "free")
# geom_line(lwd = 0.2, show.legend = F)

qr_gam1 <- qgam(GPP ~ s(precipitation, bs = "ts", k = 16), qu = 0.9, data = sample_points_model)

sample_points_model <- sample_points %>%
  mutate(precipitation = precipitation / 2000,
         meanT = meanT / 250,
         sand = sand / 100) %>%
  filter(GPP > 0)

qr_gam1 <- qgam(GPP ~ s(precipitation, bs = "ts", k = 16), qu = 0.9, data = sample_points_model)

qr_gam2 <- qgam(GPP ~ s(precipitation, bs = "ts", k = 12) +
                  s(meanT, bs = "ts", k = 12) +
                  s(sand, bs = "ts", k = 12),
                data = sample_points_model,
                qu = 0.9,
                multicore = TRUE,
                ncores = 4)

gam.check(qr_gam2)

plot(qr_gam2, rug = FALSE, seWithMean = TRUE, shift = coef(qr_gam2)[1], pages = 1)

sample_points_results <- sample_points_model %>%
  mutate(fitted = fitted(qr_gam2),
         residuals = residuals(qr_gam2))

ppt_smooth <- smooth_estimates(qr_gam1, smooth = "s(precipitation)",
                               overall_uncertainty = TRUE,
                               partial_match = TRUE) %>%
  mutate(est_with_mean = est + coef(qr_gam1)[1])

ppt_smooth

ggplot() +
  geom_point(aes(x = precipitation, y = GPP), data = sample_points_model, alpha = 0.1) +
  geom_line(aes(x = precipitation, y = est_with_mean), data = ppt_smooth, colour = "blue")

# Check zeroes

zeroes <- sample_points %>% 
  group_by(id) %>%
  summarise(nzero = sum(GPP == 0), geometry = first(geometry)) %>%
  filter(nzero > 0) %>%
  st_as_sf()

tm_shape(ke_tz) + tm_borders() +
  tm_shape(zeroes) + tm_symbols()
