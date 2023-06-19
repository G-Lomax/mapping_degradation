library(tidyverse)
library(sf)
library(here)
library(quantreg)
library(qgam)
library(tmap)

# Load data

sample_points_raw_fine <- st_read(here("data", "raw", "vector", "soralo",
                               "covariateSamplePointsDownscaled_southRift.geojson"))


sample_points_fine <- sample_points_raw_fine %>%
  pivot_longer(cols = starts_with("Y20"),
               names_to = "index", values_to = "value") %>%
  separate_wider_delim(index, "_", names = c("year", "data")) %>%
  pivot_wider(names_from = data, values_from = value)

head(sample_points_fine, n = 10)

sample_points_fine %>%
  filter(Map != 10) %>%
  ggplot(aes(x = PPT, y = GPP, colour = year)) +
  geom_point()



qr_mod1_fine <- rq(GPP ~ PPT, data = sample_points_fine %>% filter(Map == 30), tau = 0.9)

summary(qr_mod1_fine)

grass_lm <- sample_points %>%
  filter(Map == 30) %>%
  mutate(fitted = predict(qr_mod1))

grass_lm %>%
  ggplot(aes(x = PPT)) +
  geom_point(aes(y = GPP, colour = year), show.legend = FALSE) +
  geom_line(aes(y = fitted))

qr_gam1 <- qgam(GPP ~ s(PPT), qu = 0.9, data = sample_points %>% filter(Map == 30))

plot(qr_gam1, residuals = TRUE, rug = FALSE, ylim = c(0,2000))
