# Synthetic corn EONR dataset generator
# One row = site x year x topographic position
# Current crop is always corn
# Responses:
#   - eonr_kgha
#   - yield_eonr_Mg_ha

library(pacman)
p_load(dplyr, tidyr)

set.seed(6530)

# -----------------------------
# Design
# -----------------------------
n_sites <- 50
years <- 2001:2020

topo_levels <- c("high", "medium", "low")
prev_levels <- c("alfalfa", "soybean", "corn")
texture_levels <- c("coarse", "medium", "fine")
drain_levels <- c("well", "moderate", "poor")

base_df <- tidyr::crossing(
  site = factor(sprintf("S%02d", 1:n_sites)),
  year = years,
  topo_pos = factor(topo_levels, levels = topo_levels)
)

# -----------------------------
# Site-level baseline properties
# -----------------------------
site_tbl <- tibble(
  site = factor(sprintf("S%02d", 1:n_sites)),
  site_productivity = rnorm(n_sites, mean = 0, sd = 0.7),
  site_om_base = rnorm(n_sites, mean = 4.2, sd = 0.8),
  site_depth_base = rnorm(n_sites, mean = 105, sd = 18),
  site_whc_base = rnorm(n_sites, mean = 170, sd = 28),
  site_ph_base = rnorm(n_sites, mean = 6.5, sd = 0.35),
  site_slope_base = rlnorm(n_sites, meanlog = log(4.5), sdlog = 0.35),
  site_wetness = rnorm(n_sites, mean = 0, sd = 0.8),
  texture_code = sample(
    texture_levels,
    size = n_sites,
    replace = TRUE,
    prob = c(0.25, 0.45, 0.30)
  )
) %>%
  mutate(
    soil_texture = factor(texture_code, levels = texture_levels),
    drainage_base = case_when(
      soil_texture == "coarse" ~ sample(
        c("well", "moderate"), n(), replace = TRUE,
        prob = c(0.75, 0.25)
      ),
      soil_texture == "medium" ~ sample(
        c("well", "moderate", "poor"), n(), replace = TRUE,
        prob = c(0.20, 0.60, 0.20)
      ),
      soil_texture == "fine" ~ sample(
        c("moderate", "poor"), n(), replace = TRUE,
        prob = c(0.45, 0.55)
      )
    ),
    drainage_base = factor(drainage_base, levels = drain_levels)
  ) %>%
  select(-texture_code)

# -----------------------------
# Year-level weather patterns
# -----------------------------
year_tbl <- tibble(
  year = years,
  year_signal = rnorm(length(years), mean = 0, sd = 1.0),
  spring_rain_base = rnorm(length(years), mean = 220, sd = 55),
  summer_rain_base = rnorm(length(years), mean = 300, sd = 70),
  gdd_base = rnorm(length(years), mean = 1580, sd = 120),
  heat_days_base = rpois(length(years), lambda = 7)
) %>%
  mutate(
    weather_type = case_when(
      year_signal < -0.7 ~ "dry",
      year_signal > 0.7 ~ "wet",
      TRUE ~ "normal"
    )
  )

# -----------------------------
# Helper functions
# -----------------------------
clamp <- function(x, low, high) pmin(pmax(x, low), high)

# -----------------------------
# Build synthetic table
# -----------------------------
corn_eonr <- base_df %>%
  left_join(site_tbl, by = "site") %>%
  left_join(year_tbl, by = "year") %>%
  mutate(
    previous_crop = sample(
      prev_levels,
      size = n(),
      replace = TRUE,
      prob = c(0.12, 0.53, 0.35)
    ),
    previous_crop = factor(previous_crop, levels = prev_levels),
    
    topo_depth_adj = case_when(
      topo_pos == "high" ~ -12,
      topo_pos == "medium" ~ 0,
      topo_pos == "low" ~ 10
    ),
    topo_whc_adj = case_when(
      topo_pos == "high" ~ -18,
      topo_pos == "medium" ~ 0,
      topo_pos == "low" ~ 20
    ),
    topo_om_adj = case_when(
      topo_pos == "high" ~ -0.45,
      topo_pos == "medium" ~ 0,
      topo_pos == "low" ~ 0.55
    ),
    topo_slope_adj = case_when(
      topo_pos == "high" ~ 2.0,
      topo_pos == "medium" ~ 0,
      topo_pos == "low" ~ -1.2
    ),
    topo_wetness_adj = case_when(
      topo_pos == "high" ~ -0.7,
      topo_pos == "medium" ~ 0,
      topo_pos == "low" ~ 0.9
    ),
    
    soil_depth_cm = clamp(
      site_depth_base + topo_depth_adj + rnorm(n(), 0, 8),
      45, 160
    ),
    whc_mm = clamp(
      site_whc_base + topo_whc_adj + 0.55 * (soil_depth_cm - 100) + rnorm(n(), 0, 10),
      70, 280
    ),
    om_pct = clamp(site_om_base + topo_om_adj + rnorm(n(), 0, 0.35), 1.2, 8.5),
    ph = clamp(site_ph_base + rnorm(n(), 0, 0.18), 5.4, 7.8),
    slope_pct = clamp(site_slope_base + topo_slope_adj + rnorm(n(), 0, 0.8), 0.2, 14),
    
    drainage_class = case_when(
      drainage_base == "well" & topo_pos == "low" ~ sample(
        c("well", "moderate"), n(), replace = TRUE,
        prob = c(0.20, 0.80)
      ),
      drainage_base == "poor" & topo_pos == "high" ~ sample(
        c("moderate", "poor"), n(), replace = TRUE,
        prob = c(0.70, 0.30)
      ),
      TRUE ~ as.character(drainage_base)
    ),
    drainage_class = factor(drainage_class, levels = drain_levels),
    
    spring_rain_mm = clamp(
      spring_rain_base + 10 * site_wetness + 8 * topo_wetness_adj + rnorm(n(), 0, 18),
      80, 420
    ),
    summer_rain_mm = clamp(
      summer_rain_base + 10 * site_wetness + 10 * topo_wetness_adj + rnorm(n(), 0, 28),
      120, 520
    ),
    gdd = clamp(gdd_base - 4 * topo_wetness_adj + rnorm(n(), 0, 35), 1200, 1900),
    heat_stress_days = clamp(
      round(
        heat_days_base +
          if_else(weather_type == "dry", 3, 0) +
          if_else(weather_type == "wet", -1, 0) +
          if_else(topo_pos == "high", 1, 0) +
          rnorm(n(), 0, 1.5)
      ),
      0, 25
    ),
    
    planting_doy = round(clamp(
      118 +
        0.055 * (spring_rain_mm - 220) +
        if_else(drainage_class == "poor", 4, 0) +
        if_else(previous_crop == "alfalfa", 2, 0) +
        rnorm(n(), 0, 4.5),
      105, 155
    )),
    
    water_deficit_mm = clamp(
      165 - 0.52 * summer_rain_mm - 0.30 * whc_mm +
        0.030 * pmax(gdd - 1500, 0)^2 +
        8 * heat_stress_days +
        if_else(topo_pos == "high", 18, if_else(topo_pos == "low", -10, 0)) +
        rnorm(n(), 0, 18),
      0, 260
    ),
    
    excess_water_index = clamp(
      0.020 * spring_rain_mm +
        0.013 * summer_rain_mm +
        0.55 * if_else(drainage_class == "poor", 1, if_else(drainage_class == "moderate", 0.45, 0)) +
        0.40 * if_else(topo_pos == "low", 1, if_else(topo_pos == "medium", 0.35, 0)) +
        rnorm(n(), 0, 0.18),
      0, 3.2
    ),
    
    prev_nitrate_bonus = case_when(
      previous_crop == "alfalfa" ~ 10,
      previous_crop == "soybean" ~ 4,
      previous_crop == "corn" ~ 0
    ),
    prev_mineral_bonus = case_when(
      previous_crop == "alfalfa" ~ 1.1,
      previous_crop == "soybean" ~ 0.35,
      previous_crop == "corn" ~ 0
    ),
    
    spring_nitrate_ppm = clamp(
      6.5 + prev_nitrate_bonus + 0.85 * om_pct + 0.020 * soil_depth_cm -
        1.2 * excess_water_index - 0.020 * spring_rain_mm +
        if_else(drainage_class == "poor", -0.8, 0) +
        rnorm(n(), 0, 1.8),
      2, 28
    ),
    
    n_mineralization_index = clamp(
      2.0 + prev_mineral_bonus + 0.55 * om_pct + 0.010 * soil_depth_cm +
        0.0025 * whc_mm - 0.15 * excess_water_index +
        rnorm(n(), 0, 0.45),
      0.8, 9.5
    ),
    
    n_loss_risk_index = clamp(
      0.010 * spring_rain_mm +
        0.40 * if_else(drainage_class == "poor", 1, if_else(drainage_class == "moderate", 0.45, 0)) +
        0.35 * if_else(topo_pos == "low", 1, if_else(topo_pos == "medium", 0.2, 0)) +
        0.18 * if_else(soil_texture == "fine", 1, if_else(soil_texture == "medium", 0.4, 0)) +
        rnorm(n(), 0, 0.25),
      0, 5.5
    ),
    
    yield_potential =
      11.2 +
      0.55 * site_productivity +
      0.010 * (soil_depth_cm - 100) +
      0.008 * (whc_mm - 170) +
      0.12 * pmin(om_pct, 5.5) -
      0.09 * pmax(om_pct - 5.5, 0) -
      0.00011 * (gdd - 1580)^2 -
      0.018 * pmax(planting_doy - 122, 0) -
      0.022 * pmax(water_deficit_mm - 35, 0) -
      0.060 * pmax(heat_stress_days - 6, 0)^1.2 -
      0.40 * pmax(excess_water_index - 1.2, 0)^1.3 +
      0.65 * exp(-((ph - 6.6)^2) / 0.18) +
      if_else(topo_pos == "low" & weather_type == "dry", 0.45, 0) +
      if_else(topo_pos == "low" & weather_type == "wet", -0.35, 0) +
      if_else(topo_pos == "high" & weather_type == "dry", -0.55, 0) +
      if_else(previous_crop == "corn", -0.18, 0) +
      if_else(previous_crop == "alfalfa", 0.20, 0),
    
    yield_eonr_Mg_ha = clamp(yield_potential + rnorm(n(), 0, 0.45), 5.5, 17.5),
    
    eonr_raw =
      112 +
      8.5 * (yield_eonr_Mg_ha - 10) +
      8.0 * n_loss_risk_index -
      4.8 * spring_nitrate_ppm -
      7.0 * n_mineralization_index +
      0.10 * pmax(planting_doy - 125, 0)^1.2 +
      10 * if_else(water_deficit_mm > 120 & topo_pos == "high", 1, 0) +
      14 * if_else(spring_rain_mm > 260 & drainage_class == "poor", 1, 0) +
      10 * if_else(spring_rain_mm > 260 & topo_pos == "low", 1, 0) +
      if_else(previous_crop == "alfalfa", -78, 0) +
      if_else(previous_crop == "soybean", -24, 0) +
      if_else(previous_crop == "corn", 0, 0),
    
    eonr_kgha = clamp(eonr_raw + rnorm(n(), 0, 12), 0, 280),
    
    eonr_kgha = if_else(
      previous_crop == "alfalfa" & spring_nitrate_ppm > 16 & n_mineralization_index > 5.5,
      pmax(0, eonr_kgha - runif(n(), 18, 45)),
      eonr_kgha
    ),
    
    crop = factor("corn")
  ) %>%
  select(
    crop, site, year, topo_pos, previous_crop,
    soil_texture, soil_depth_cm, whc_mm, om_pct, ph, drainage_class, slope_pct,
    planting_doy, spring_rain_mm, summer_rain_mm, gdd, heat_stress_days,
    water_deficit_mm, excess_water_index,
    spring_nitrate_ppm, n_mineralization_index, n_loss_risk_index,
    yield_eonr_Mg_ha, eonr_kgha
  )

# -----------------------------
# Quick checks
# -----------------------------
summary_tbl <- corn_eonr %>%
  summarise(
    n = n(),
    yield_min = min(yield_eonr_Mg_ha),
    yield_mean = mean(yield_eonr_Mg_ha),
    yield_max = max(yield_eonr_Mg_ha),
    eonr_min = min(eonr_kgha),
    eonr_mean = mean(eonr_kgha),
    eonr_max = max(eonr_kgha)
  )

prev_crop_summary <- corn_eonr %>%
  group_by(previous_crop) %>%
  summarise(
    n = n(),
    mean_yield = mean(yield_eonr_Mg_ha),
    mean_eonr = mean(eonr_kgha),
    p10_eonr = quantile(eonr_kgha, 0.10),
    p50_eonr = quantile(eonr_kgha, 0.50),
    p90_eonr = quantile(eonr_kgha, 0.90),
    .groups = "drop"
  )

print(summary_tbl)
print(prev_crop_summary)

# Save file
# write.csv(corn_eonr, "synthetic_corn_eonr.csv", row.names = FALSE)
