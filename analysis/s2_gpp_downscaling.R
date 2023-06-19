library(tidyverse)
library(lubridate)
library(modelr)
library(broom)
library(here)

s2_gpp <- read_csv(here("data", "raw", "csv", "s2GppCalibrationPointsPure.csv"))

table(s2_gpp$strata)

s2_gpp_na <- s2_gpp %>%
  mutate(across(.cols = c("NDVI", "EVI", "MSAVI", "probability"),
                .fns = ~ na_if(.x, -9999)))

s2_gpp_long <- s2_gpp_na %>%
  pivot_longer(cols = c("NDVI", "EVI", "MSAVI"),
               names_to = "index",
               values_to = "value") %>%
  filter(!is.na(value))


ggplot(s2_gpp_long, aes(x = value, y = GPP, colour = index)) +
  geom_point() +
  facet_wrap(~strata, nrow = 2, dir = "v") +
  theme_bw()

# Simple regression models for each land cover, strata and VI type

s2_gpp_nested <- s2_gpp_long %>%
  group_by(strata, index) %>%
  nest()

s2_gpp_mods <- s2_gpp_nested %>%
  mutate(lm = map(data, function(df) {
    lm(GPP ~ value, data = df)
  }))

s2_gpp_resids <- s2_gpp_mods %>%
  mutate(resids = map2(data, lm, add_residuals)) %>%
  unnest(cols = resids)

s2_gpp_resids %>%
  ggplot(aes(x = value, y = resid, colour = index)) +
  geom_point() +
  facet_wrap(~strata, nrow = 2, dir = "v") +
  theme_bw()

# Assess model quality for each ecoregion, land cover and index
s2_gpp_quality <- s2_gpp_mods %>%
  mutate(glance = map(lm, glance)) %>%
  unnest(glance)

s2_best_models <- s2_gpp_quality %>%
  group_by(strata) %>%
  filter(r.squared == max(r.squared))

head(s2_best_models)

# NDVI is the best linear predictor of GPP for all land cover types and
# both ecoregions. It performs best in grasslands (r-squared of 0.83 and 0.87)
# and worst in tree-covered pixels (r-squared of 0.61 and 0.62)

# I could probably do better with a glm modelling GPP with a gamma distribution
# or similar. It does look like the residuals are quite skewed in grasslands
# at low NDVI values

# Raw NDVI values only
ggplot(s2_gpp_long %>% filter(index == "NDVI"), aes(x = value, y = GPP)) +
  geom_point() +
  facet_wrap(~strata, nrow = 2, dir = "v") +
  theme_bw()

# Residuals
s2_gpp_resids %>%
  filter(index == "NDVI") %>%
  ggplot(aes(x = value, y = resid)) +
  geom_point() +
  facet_wrap(~strata, nrow = 2, dir = "v") +
  theme_bw()


# Test log/Gamma GLMs

s2_gpp_glms <- s2_gpp_nested %>%
  mutate(data = map(data, function(df) {
    min_nonzero_gpp <- min(filter(df, GPP > 0)$GPP)
    df %>% mutate(GPP = GPP + min_nonzero_gpp * 0.1)
  }),
  glm = map(data, function(df) {
    glm(GPP ~ value, data = df,
        family = Gamma(link = "log"))
  }))

s2_gpp_glm_resids <- s2_gpp_glms %>%
  mutate(resids = map2(data, glm, add_residuals)) %>%
  unnest(cols = resids)

s2_gpp_glm_fitted <- s2_gpp_glms %>%
  mutate(fitted = map2(data, glm, add_predictions)) %>%
  unnest(cols = fitted)

s2_gpp_glm_resids %>%
  ggplot(aes(x = value, y = resid, colour = index)) +
  geom_point() +
  facet_wrap(~strata, nrow = 2, dir = "v") +
  theme_bw()

s2_gpp_glm_quality <- s2_gpp_glms %>%
  mutate(glance = map(glm, glance)) %>%
  unnest(glance)

s2_gpp_glm_quality

# Extract coefficients for lms

s2_gpp_coefs <- s2_gpp_mods %>%
  filter(index == "NDVI") %>%
  mutate(constant = unlist(map(lm, function(mod) {mod$coefficients[[1]]})),
         slope = unlist(map(lm, function(mod) {mod$coefficients[[2]]})))

s2_gpp_coefs$constant[3]
