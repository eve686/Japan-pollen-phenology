# Load required libraries
library(readr)
library(readxl)
library(dplyr)
library(tidyverse)
library(purrr)
library(lubridate)

# 1. Read the full dataset with StationID and year columns
pollen_all <- read_csv("~/Desktop/RAWjapan/pollen_all_years_with_latlon.csv")

# 2. Count number of unique years per StationID
station_year_counts <- pollen_all %>%
  group_by(StationID) %>%
  summarise(years_observed = n_distinct(year)) %>%
  filter(years_observed >= 5)

# 3. Filter the full dataset to keep only those StationIDs
pollen_filtered <- pollen_all %>%
  filter(StationID %in% station_year_counts$StationID)

# 4. Save the filtered dataset
write_csv(pollen_filtered, "~/Desktop/RAWjapan/pollen_filtered_min5years.csv")
pollen_final <- pollen_filtered %>%
  select(No., point, StationID, year, `Latitude and longitude`)
write_csv(pollen_final, "~/Desktop/RAWjapan/pollen_final_selected_columns.csv")
write_csv(station_year_counts, "~/Desktop/RAWjapan/station_year_counts.csv")

# Keep only one row per StationID (with point and lat/lon)
station_coords <- pollen_final %>%
  group_by(StationID) %>%
  slice(1) %>%  # keep only the first row per StationID
  ungroup() %>%
  select(StationID, point, `Latitude and longitude`)
write_csv(station_coords, "~/Desktop/RAWjapan/station_locations_unique.csv")

#Map of Study Sites
library(raster)
install.packages('raster')
install.packages('terra')
library(terra)
library(ggplot2)
library(sf)
install.packages('sf')
library(dplyr)
library(readxl)
install.packages("terra", repos = "https://cran.r-project.org")
install.packages("sf", repos = "https://cran.r-project.org")
library(viridis)
install.packages('viridis')

# ==== 1. Read station data ====
station_data <- read_excel("~/Desktop/station_locations_unique.xlsx")
station_data2 <- station_data %>%
  separate(`Latitude and longitude`, into = c("Latitude", "Longitude"), sep = ", ") %>%
  mutate(
    Latitude = as.numeric(Latitude),
    Longitude = as.numeric(Longitude)
  )

# Convert to SpatVector directly (no need for sf)
sites_vect <- vect(station_data2,
                   geom = c("Longitude", "Latitude"),
                   crs = "EPSG:4326")

# ==== 2. Convert to sf points (WGS84) ====
sites_sf <- st_as_sf(
  station_data2,
  coords = c("Longitude", "Latitude"),
  crs = 4326
)

# ==== 2. Read rasters ====
alan_rast <- rast("~/Desktop/RAWjapan/pollen_rs/ALANMap2023/ALANMap2023.tif")
#lc_rast1  <- rast("~/Desktop/RAWjapan/pollen_rs/LCMap2022/1.tif")
#lc_rast2  <- rast("~/Desktop/RAWjapan/pollen_rs/LCMap2022/2.tif")
#lc_rast3  <- rast("~/Desktop/RAWjapan/pollen_rs/LCMap2022/3.tif")
#lc_rast4  <- rast("~/Desktop/RAWjapan/pollen_rs/LCMap2022/4.tif")
lc_rast <- rast("~/Desktop/RAWjapan/pollen_rs/LCMap2022/merge.tif")

# ==== 3. Extract values ====
alan_vals <- terra::extract(alan_rast, sites_vect)
station_with_vals <- cbind(station_data2,
                           ALAN = alan_vals[,2],
                           LandCover = lc_vals[,2])
# ==== 4. Convert rasters for ggplot ====
alan_df <- as.data.frame(alan_rast, xy = TRUE)
colnames(alan_df)[3] <- "ALAN"

#Map sties
library(terra)
library(dplyr)
library(ggplot2)

# --- existing: read raster ---
alan_rast <- rast("~/Desktop/RAWjapan/pollen_rs/ALANMap2023/ALANMap2023.tif")

# (optional) downsample so plotting is fast
alan_small <- aggregate(alan_rast, fact = 5, fun = "mean")

# --- NEW: log-transform values ---
# use log10( value + 1 ) to avoid log(0)
alan_log <- log10(alan_small)

# convert to df for ggplot
alan_df <- as.data.frame(alan_log, xy = TRUE, na.rm = TRUE)
colnames(alan_df)[3] <- "logALAN"

p1 <- ggplot() +
  geom_raster(data = alan_df, aes(x = x, y = y, fill = logALAN)) +
  scale_fill_viridis_c(
    option = "magma",
    name   = "log10(ALAN)"
  ) +
  coord_quickmap(xlim = c(122, 150), ylim = c(24, 46)) +
  theme_minimal() +
  labs(
    title = "log-transformed ALAN Map with Study Sites",
    x = "Longitude", y = "Latitude"
  )

p1
ggsave(filename ='visuals/Plot ALAN.png' ,plot=p1,dpi = 300,width = 6,height = 6)

# Aggregate land cover and convert rester
lc_rast2 <- aggregate(lc_rast, fact = 200)  
lc_df <- as.data.frame(lc_rast2, xy = TRUE, na.rm = TRUE)
names(lc_df)[3] <- "LandCover"
lc_df$LandCover <- as.factor(as.integer(round(lc_df$LandCover)))

group_fun <- function(v) {
  dplyr::case_when(
    v %in% c(10,11,12,20) ~ 1L,  # Cropland
    v %in% 51:92          ~ 2L,  # Forest (51–92)
    v == 190              ~ 3L,  # Urban
    v == 210              ~ 4L,  # Water
    TRUE                  ~ 5L   # Other
  )
}

lc_group_rast <- app(lc_rast, group_fun)
levels(lc_group_rast) <- data.frame(
  ID       = 1:5,
  lc_group = c("Cropland","Forest","Urban","Water","Other")
)

# 5. Plot
lc_cols <- c(
  "Cropland" = "#FFD54F",
  "Forest"   = "#2E7D32",
  "Urban"    = "#C62828",
  "Water"    = "#1E88E5",
  "Other"    = "grey80"
)

ggplot(lc_group_rast, aes(x = x, y = y, fill = lc_group)) +
  scale_fill_manual(values = lc_cols, name = "Land cover") +
  coord_quickmap(xlim = c(122, 150), ylim = c(24, 46), expand = FALSE) +
  theme_minimal() +
  labs(title = "Simplified Land Cover Map of Japan")

# Build a lookup table
landcover_labels <- c(
  "10"  = "Rainfed cropland",
  "11"  = "Herbaceous cropland",
  "12"  = "Orchard cropland",
  "20"  = "Irrigated cropland",
  "51"  = "Open evergreen broadleaved forest",
  "52"  = "Closed evergreen broadleaved forest",
  "61"  = "Open deciduous broadleaved forest",
  "62"  = "Closed deciduous broadleaved forest",
  "71"  = "Open evergreen needle-leaved forest",
  "72"  = "Closed evergreen needle-leaved forest",
  "81"  = "Open deciduous needle-leaved forest",
  "82"  = "Closed deciduous needle-leaved forest",
  "91"  = "Open mixed forest",
  "92"  = "Closed mixed forest",
  "120" = "Shrubland",
  "121" = "Evergreen shrubland",
  "122" = "Deciduous shrubland",
  "130" = "Grassland",
  "140" = "Lichens and mosses",
  "150" = "Sparse vegetation",
  "152" = "Sparse shrubland",
  "153" = "Sparse herbaceous",
  "181" = "Swamp",
  "182" = "Marsh",
  "183" = "Flooded flat",
  "184" = "Saline",
  "185" = "Mangrove",
  "186" = "Salt marsh",
  "187" = "Tidal flat",
  "190" = "Impervious surfaces",
  "200" = "Bare areas",
  "201" = "Consolidated bare areas",
  "202" = "Unconsolidated bare areas",
  "210" = "Water body",
  "220" = "Permanent ice/snow"
)

landcover_labels <- case_when(
  
  # ----- Cropland -----
  lc_df %in% c("10","11","12","20") ~ "Cropland",
  
  # ----- Forests -----
  lc_df %in% c(
    "51","52",     # evergreen broadleaf: open/closed
    "61","62",     # deciduous broadleaf: open/closed
    "71","72",     # evergreen needleleaf: open/closed
    "81","82",     # deciduous needleleaf: open/closed
    "91","92"      # mixed forests
  ) ~ "Forest",
  
  # ----- Urban / Impervious -----
  lc_df == "190" ~ "Urban",
  
  # ----- Water -----
  lc_df == "210" ~ "Water",
  
  # ----- Everything else -----
  TRUE ~ "Other"
)

landcover_colors <- c(
  "Cropland" = "#FFD54F",
  "Forest"   = "#2E7D32",
  "Urban"    = "#C62828",
  "Water"    = "#1E88E5",
  "Other"    = "grey70"
)

landcover_colors <- c(
  "10"  = rgb(255,255,100, maxColorValue=255),
  "11"  = rgb(255,255,100, maxColorValue=255),
  "12"  = rgb(255,255,0,   maxColorValue=255),
  "20"  = rgb(170,240,240, maxColorValue=255),
  "51"  = rgb(76,115,0,    maxColorValue=255),
  "52"  = rgb(0,100,0,     maxColorValue=255),
  "61"  = rgb(170,200,0,   maxColorValue=255),
  "62"  = rgb(0,160,0,     maxColorValue=255),
  "71"  = rgb(0,80,0,      maxColorValue=255),
  "72"  = rgb(0,60,0,      maxColorValue=255),
  "81"  = rgb(40,100,0,    maxColorValue=255),
  "82"  = rgb(40,80,0,     maxColorValue=255),
  "91"  = rgb(160,180,50,  maxColorValue=255),
  "92"  = rgb(120,130,0,   maxColorValue=255),
  "120" = rgb(150,100,0,   maxColorValue=255),
  "121" = rgb(150,75,0,    maxColorValue=255),
  "122" = rgb(150,100,0,   maxColorValue=255),
  "130" = rgb(255,180,50,  maxColorValue=255),
  "140" = rgb(255,220,210, maxColorValue=255),
  "150" = rgb(255,235,175, maxColorValue=255),
  "152" = rgb(255,210,120, maxColorValue=255),
  "153" = rgb(255,235,175, maxColorValue=255),
  "181" = rgb(0,168,132,   maxColorValue=255),
  "182" = rgb(115,255,223, maxColorValue=255),
  "183" = rgb(158,187,215, maxColorValue=255),
  "184" = rgb(130,130,130, maxColorValue=255),
  "185" = rgb(245,122,182, maxColorValue=255),
  "186" = rgb(102,205,171, maxColorValue=255),
  "187" = rgb(68,79,137,   maxColorValue=255),
  "190" = rgb(195,20,0,    maxColorValue=255),
  "200" = rgb(255,245,215, maxColorValue=255),
  "201" = rgb(220,220,220, maxColorValue=255),
  "202" = rgb(255,245,215, maxColorValue=255),
  "210" = rgb(0,70,200,    maxColorValue=255),
  "220" = rgb(255,255,255, maxColorValue=255)
)

#  === 6. Plot Landcover ===
p2 <- ggplot() +
  geom_tile(data = lc_df, aes(x = x, y = y, fill = LandCover)) +
  scale_fill_manual(
    values = landcover_colors,
    labels = landcover_labels,
    na.value = "transparent"
  ) +
  coord_sf(xlim = c(122, 150), ylim = c(24, 46), expand = FALSE) +
  theme_minimal() +
  labs(title = "Japan Land Cover Map with Stations", fill = "Land Cover") +
  theme(legend.key.size = unit(0.5, "cm"))
p2
ggsave("visuals/Plot Land cover + sites.png", plot = p2, dpi = 300, width = 8, height = 8)

# Plot station locations
ggplot(station_data2, aes(x = Longitude, y = Latitude)) +
  borders("world", colour = "grey50", fill = "grey90") +   # world map background
  geom_point(aes(color = StationID), size = 3, alpha = 0.8) + # site points, colored by 'point'
  theme_minimal() +
  coord_fixed(1.3) +
  labs(
    title = "Map of Study Sites",
    x = "Longitude",
    y = "Latitude")+ coord_quickmap(xlim = c(122, 150), ylim = c(24, 46))

ggplot(station_data2, aes(x = Longitude, y = Latitude)) +
  borders("world", colour = "grey50", fill = "grey90") +
  geom_point(size = 1, color = "red", alpha = 0.8) +   # No color mapping
  coord_quickmap(xlim = c(122, 150), ylim = c(24, 46)) +
  theme_minimal() +
  labs(
    title = "Map of Study Sites",
    x = "Longitude",
    y = "Latitude")
ggsave(filename ='visuals/Site.png',dpi = 300,width = 8,height = 8)   

#Temporal Trends
pollen_filtered 
library(stringr)
install.packages("ggh4x")
library(ggh4x)

pollen_filtered %>%
  summarise(n_station = n_distinct(StationID))

df_pollen_long %>%
  filter(metric %in% c("c) Maximum daily pollen", "d) Total dispersal")) %>%
  group_by(species, metric) %>%
  summarise(
    n = n(),
    min_value = min(value_numeric, na.rm = TRUE),
    q01 = quantile(value_numeric, 0.01, na.rm = TRUE),
    q05 = quantile(value_numeric, 0.05, na.rm = TRUE),
    median = median(value_numeric, na.rm = TRUE),
    mean = mean(value_numeric, na.rm = TRUE),
    max_value = max(value_numeric, na.rm = TRUE),
    n_zero = sum(value_numeric == 0, na.rm = TRUE),
    n_le10 = sum(value_numeric <= 10, na.rm = TRUE),
    .groups = "drop"
  )

ggplot(
  df_pollen_long %>%
    filter(metric %in% c("c) Maximum daily pollen", "d) Total dispersal")),
  aes(x = value_numeric)
) +
  geom_histogram(bins = 50, fill = "grey70", color = "black") +
  scale_x_log10() +
  facet_grid(metric ~ species, scales = "free") +
  theme_classic(base_size = 12) +
  labs(
    x = "Pollen abundance, log10 scale",
    y = "Count"
  )

# 1. Prepare long-format dataset
# ============================================================
df_pollen_long <- pollen_filtered %>%
  dplyr::select(
    year, StationID,
    dis_sugi, maxdis_sugi, max_sugi, sum_sugi,
    dis_hinoki, maxdis_hinoki, max_hinoki, sum_hinoki
  ) %>%
  mutate(
    # Date columns: real date -> DOY
    across(
      c(dis_sugi, maxdis_sugi, dis_hinoki, maxdis_hinoki),
      ~ yday(as.Date(.x))
    ),
    
    # Pollen concentration / total dispersal columns: numeric
    across(
      c(max_sugi, sum_sugi, max_hinoki, sum_hinoki),
      ~ as.numeric(.x)
    )
  ) %>%
  pivot_longer(
    cols = -c(year, StationID),
    names_to = "variable",
    values_to = "value_numeric"
  ) %>%
  mutate(
    species = case_when(
      str_detect(variable, "sugi") ~ "Japanese cedar",
      str_detect(variable, "hinoki") ~ "Japanese cypress"
    ),
    
    metric = case_when(
      variable %in% c("dis_sugi", "dis_hinoki") ~ "Start of Pollen Season",
      variable %in% c("maxdis_sugi", "maxdis_hinoki") ~ "Peak of Pollen Season",
      variable %in% c("max_sugi", "max_hinoki") ~ "Maximum Daily Pollen",
      variable %in% c("sum_sugi", "sum_hinoki") ~ "Total Pollen Concentration"
    )
  ) %>%
  filter(
    !is.na(species),
    !is.na(metric),
    !is.na(value_numeric)
  ) %>%
  mutate(
    species = factor(species, levels = c("Japanese cedar", "Japanese cypress")),
    metric = factor(
      metric,
      levels = c(
        "Start of Pollen Season",
        "Peak of Pollen Season",
        "Maximum Daily Pollen",
        "Total Pollen Concentration"
      )
    )
  )

df_pollen_long <- df_pollen_long %>%
  filter(
    !(
      metric %in% c("Maximum Daily Pollen", "Total Pollen Concentration") &
        value_numeric == 0
    )
  ) %>%
  filter(year >= 1995)

# ============================================================
# 2. Annual mean for each species and metric
# ============================================================
df_mean <- df_pollen_long %>%
  group_by(species, metric, year) %>%
  summarise(
    mean_val = mean(value_numeric, na.rm = TRUE),
    .groups = "drop"
  )

# ============================================================
# 3. Linear regression for each panel
# ============================================================
lm_stats <- df_mean %>%
  group_by(species, metric) %>%
  do({
    model <- lm(mean_val ~ year, data = .)
    tidy_model <- broom::tidy(model)
    
    intercept <- tidy_model$estimate[tidy_model$term == "(Intercept)"]
    slope <- tidy_model$estimate[tidy_model$term == "year"]
    p_value <- tidy_model$p.value[tidy_model$term == "year"]
    
    data.frame(
      intercept = intercept,
      slope = slope,
      p_value = p_value,
      label = paste0(
        "y = ", round(intercept, 2), " + ", round(slope, 3), "x",
        "\nSlope = ", round(slope, 3),
        "\np = ", signif(p_value, 3)
      )
    )
  }) %>%
  ungroup()

# ============================================================
# 4. Label position for regression text
# ============================================================

lm_stats <- df_mean %>%
  group_by(species, metric) %>%
  do({
    model <- lm(mean_val ~ year, data = .)
    td <- broom::tidy(model)
    
    intercept <- td$estimate[td$term == "(Intercept)"]
    slope     <- td$estimate[td$term == "year"]
    p_value   <- td$p.value[td$term == "year"]
    
    data.frame(
      intercept = intercept,
      slope = slope,
      p_value = p_value,
      label = paste0(
        "y = ", round(intercept, 2),
        ifelse(slope >= 0, " + ", " - "),
        abs(round(slope, 3)), "x",
        "; slope = ", round(slope, 3),
        "; P = ", signif(p_value, 3)
      )
    )
  }) %>%
  ungroup()

# ============================================================
# 5. Plot temporal trends
# ============================================================

library(ggh4x)

# 1. Recalculate label positions for zoomed panels
label_pos <- df_pollen_long %>%
  filter(year >= 1995) %>%
  group_by(species, metric) %>%
  summarise(
    x_min = min(year, na.rm = TRUE),
    x_max = max(year, na.rm = TRUE),
    y_min = min(value_numeric, na.rm = TRUE),
    y_max = max(value_numeric, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    x_pos = x_min + 0.04 * (x_max - x_min),
    y_pos = case_when(
      metric == "Maximum Daily Pollen" ~ 5000 * 0.90,
      metric == "Total Pollen Concentration" ~ 20000 * 0.90,
      TRUE ~ y_max - 0.08 * (y_max - y_min)
    )
  )

lm_stats <- lm_stats %>%
  dplyr::select(-any_of(c("x_pos", "y_pos"))) %>%
  left_join(label_pos, by = c("species", "metric"))

p_temporal_nature <- ggplot() +
  geom_line(
    data = df_pollen_long,
    aes(x = year, y = value_numeric, group = StationID),
    color = "grey80",
    alpha = 0.45,
    linewidth = 0.25
  ) +
  geom_line(
    data = df_mean,
    aes(x = year, y = mean_val, group = 1),
    color = "black",
    linewidth = 1.2
  ) +
  geom_smooth(
    data = df_mean,
    aes(x = year, y = mean_val, group = 1),
    method = "lm",
    se = FALSE,
    color = "#D55E00",
    linewidth = 1.0
  ) +
  geom_text(
    data = lm_stats,
    aes(x = x_pos, y = y_pos, label = label),
    hjust = 0,
    vjust = 1,
    size = 4.2,
    color = "black"
  ) +
  ggh4x::facet_grid2(
    rows = vars(metric),
    cols = vars(species),
    scales = "free",
    independent = "all",
    switch = "y"
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0.04, 0.10))
  ) +
  scale_x_continuous(
    expand = expansion(mult = c(0.02, 0.02))
  ) +
  labs(
    x = "Year",
    y = ""
  ) +
  theme_classic(base_size = 9) +
  theme(
    
    # remove strip backgrounds
    strip.background = element_blank(),
    
    # top headers: Japanese cedar / Japanese cypress
    strip.text.x = element_text(
      face = "bold",
      size = 14,
      color = "black"
    ),
    
    # left-side metric labels
    strip.text.y.left = element_text(
      face = "bold",
      size = 12,
      color = "black",
      angle = 90
    ),
    
    # put row labels outside y axis
    strip.placement = "outside",
    
    axis.title = element_text(
      face = "bold",
      size = 10,
      color = "black"
    ),
    
    axis.text = element_text(
      size = 12,
      color = "black"
    ),
    
    axis.line = element_line(
      color = "black",
      linewidth = 0.35
    ),
    
    axis.ticks = element_line(
      color = "black",
      linewidth = 0.35
    ),
    
    panel.border = element_rect(
      color = "black",
      fill = NA,
      linewidth = 0.35
    ),
    
    panel.spacing = unit(1.1, "lines"),
    
    legend.position = "none"
  )+
  ggh4x::facetted_pos_scales(
    y = list(
      species == "Japanese cedar" & metric == "Maximum Daily Pollen" ~
        scale_y_continuous(limits = c(0, 5000)),
      species == "Japanese cypress" & metric == "Maximum Daily Pollen" ~
        scale_y_continuous(limits = c(0, 5000)),
      species == "Japanese cedar" & metric == "Total Pollen Concentration" ~
        scale_y_continuous(limits = c(0, 20000)),
      species == "Japanese cypress" & metric == "Total Pollen Concentration" ~
        scale_y_continuous(limits = c(0, 20000))
    )
  )

p_temporal_nature

ggsave(filename ='visuals/timetrend-pollen.png',dpi = 300,width =12,height = 11)

#quantile analysis
library(dplyr)

qs <- c(0.10, 0.25, 0.50, 0.75, 0.90)

year_quantile <- df_pollen_long %>%
  group_by(species, metric, year) %>%
  reframe(
    quantile_level = qs,
    value = quantile(
      value_numeric,
      probs = qs,
      na.rm = TRUE,
      names = FALSE
    )
  ) %>%
  mutate(
    quantile_level = factor(
      quantile_level,
      levels = qs,
      labels = c("10%", "25%", "50%", "75%", "90%")
    )
  )

trend_table <- year_quantile %>%
  group_by(species, metric, quantile_level) %>%
  do({
    fit <- lm(value ~ year, data = .)
    
    tibble(
      slope = coef(fit)[2],
      p_value = summary(fit)$coefficients[2, 4],
      r2 = summary(fit)$r.squared
    )
  }) %>%
  ungroup()

print(trend_table, n = Inf)

#Time series but for ALAN (2012-2023??) and temperature (1986-2023?)

library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(broom)
library(readr)

df_temp <- read_excel("~/Desktop/Merged_files_annual.xlsx")

df_temp <- df_temp %>%
  mutate(
    year = as.numeric(year),
    StationID = as.character(StationID),
    Longitude = as.numeric(Longitude),
    Latitude  = as.numeric(Latitude),
    
    ALAN = parse_number(as.character(ALAN)),
    log_alan = log10(ALAN + 1),
    
    annual_mean_Tmax_C = as.numeric(annual_mean_Tmax_C),
    annual_mean_Tmin = as.numeric(annual_mean_Tmin),
    annual_mean_monthly_Precip_mm = as.numeric(annual_mean_monthly_Precip_mm),
    
    annual_mean_temp_C = (annual_mean_Tmax_C + annual_mean_Tmin) / 2
  ) %>%
  filter(!is.na(Longitude), !is.na(Latitude))

df_env_long <- df_temp %>%
  dplyr::select(
    StationID, year,
    log_alan,
    annual_mean_temp_C
  ) %>%
  pivot_longer(
    cols = c(log_alan, annual_mean_temp_C),
    names_to = "variable",
    values_to = "value"
  ) %>%
  mutate(
    metric = case_when(
      variable == "log_alan" ~ "a) Artificial light at night",
      variable == "annual_mean_temp_C" ~ "b) Annual mean temperature"
    )
  ) %>%
  filter(!is.na(value)) %>%
  mutate(
    metric = factor(
      metric,
      levels = c(
        "a) Artificial light at night",
        "b) Annual mean temperature"
      )
    )
  )



# ============================================================
# 3. Annual mean across stations
# ============================================================

df_env_mean <- df_env_long %>%
  group_by(metric, year) %>%
  summarise(
    mean_val = mean(value, na.rm = TRUE),
    .groups = "drop"
  )

# ============================================================
# 4. Linear regression for each panel
# ============================================================

env_lm_stats <- df_env_mean %>%
  group_by(metric) %>%
  do({
    model <- lm(mean_val ~ year, data = .)
    td <- broom::tidy(model)
    
    intercept <- td$estimate[td$term == "(Intercept)"]
    slope     <- td$estimate[td$term == "year"]
    p_value   <- td$p.value[td$term == "year"]
    
    data.frame(
      intercept = intercept,
      slope = slope,
      p_value = p_value,
      label = paste0(
        "slope = ", round(slope, 3),
        "; P = ", signif(p_value, 3)
      )
    )
  }) %>%
  ungroup()

# ============================================================
# 5. Label position
# ============================================================

label_pos <- df_env_long %>%
  group_by(metric) %>%
  summarise(
    x_min = min(year, na.rm = TRUE),
    x_max = max(year, na.rm = TRUE),
    y_min = min(value, na.rm = TRUE),
    y_max = max(value, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    x_pos = x_min + 0.04 * (x_max - x_min),
    y_pos = y_max - 0.08 * (y_max - y_min)
  )

env_lm_stats <- env_lm_stats %>%
  dplyr::select(-any_of(c("x_pos", "y_pos"))) %>%
  left_join(label_pos, by = "metric")

# ============================================================
# 6. Plot
# ============================================================

p_env_trend <- ggplot() +
  
  geom_line(
    data = df_env_long,
    aes(
      x = year,
      y = value,
      group = StationID
    ),
    color = "grey80",
    alpha = 0.45,
    linewidth = 0.25
  ) +
  
  geom_line(
    data = df_env_mean,
    aes(
      x = year,
      y = mean_val,
      group = 1
    ),
    color = "black",
    linewidth = 0.9
  ) +
  
  geom_smooth(
    data = df_env_mean,
    aes(
      x = year,
      y = mean_val,
      group = 1
    ),
    method = "lm",
    se = FALSE,
    color = "#D55E00",
    linewidth = 0.75
  ) +
  
  geom_text(
    data = env_lm_stats,
    aes(
      x = x_pos,
      y = y_pos,
      label = label
    ),
    hjust = 0,
    vjust = 1,
    size = 3,
    color = "black"
  ) +
  
  facet_wrap(
    ~ metric,
    ncol = 2,
    scales = "free"
  ) +
  
  scale_x_continuous(
    expand = expansion(mult = c(0.02, 0.02))
  ) +
  
  scale_y_continuous(
    expand = expansion(mult = c(0.04, 0.10))
  ) +
  
  labs(
    x = "Year",
    y = ""
  ) +
  
  theme_classic(base_size = 10) +
  theme(
    strip.background = element_blank(),
    strip.text = element_text(
      face = "bold",
      size = 11,
      color = "black"
    ),
    axis.title = element_text(
      face = "bold",
      size = 11,
      color = "black"
    ),
    axis.text = element_text(
      size = 9,
      color = "black"
    ),
    axis.line = element_line(
      color = "black",
      linewidth = 0.35
    ),
    axis.ticks = element_line(
      color = "black",
      linewidth = 0.35
    ),
    panel.border = element_rect(
      color = "black",
      fill = NA,
      linewidth = 0.35
    ),
    panel.spacing = unit(1.2, "lines"),
    legend.position = "none"
  )

p_env_trend

ggsave(
  "visuals/timetrendalantem-logalan.png",
  p_env_trend,
  width = 10,
  height = 4,
  dpi = 300
)

#Barplot
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(readxl)
library(patchwork)

df <- read_excel("~/Desktop/Merged_files_copy.xlsx")

df %>%
  summarise(
    n_station = n_distinct(StationID)
  )

names(df)


df2 <- df %>%
  mutate(
    dis_hinoki_doy = yday(as.Date(as.numeric(dis_hinoki), origin = "1899-12-30")),
    maxdis_hinoki_doy = yday(as.Date(as.numeric(maxdis_hinoki), origin = "1899-12-30")),
    dis_sugi_doy = yday(as.Date(as.numeric(dis_sugi), origin = "1899-12-30")),
    maxdis_sugi_doy = yday(as.Date(as.numeric(maxdis_sugi), origin = "1899-12-30")),
    
    max_hinoki = as.numeric(max_hinoki),
    sum_hinoki = as.numeric(sum_hinoki),
    max_sugi = as.numeric(max_sugi),
    sum_sugi = as.numeric(sum_sugi),
    
    lc_chr = as.character(Landcover),
    lc_group = case_when(
      lc_chr %in% c("10", "11", "12", "20") ~ "Cropland",
      lc_chr %in% c("51", "52", "61", "62") ~ "Broadleaf forest",
      lc_chr %in% c("71", "72", "81", "82")~ "Needleleaf forest",
      lc_chr %in% c("91", "92") ~ "Mixed forest",
      lc_chr == "130" ~ "Grassland",
      lc_chr == "190" ~ "Urban",
      TRUE ~ "Other"
    )
  ) %>%
  filter(lc_group != "Other")

dat_long <- df2 %>%
  dplyr::select(
    StationID, year, lc_group,
    dis_sugi_doy, maxdis_sugi_doy, max_sugi, sum_sugi,
    dis_hinoki_doy, maxdis_hinoki_doy, max_hinoki, sum_hinoki
  ) %>%
  pivot_longer(
    cols = -c(StationID, year, lc_group),
    names_to = "variable",
    values_to = "value"
  ) %>%
  mutate(
    species = case_when(
      grepl("sugi", variable) ~ "Japanese cedar",
      grepl("hinoki", variable) ~ "Japanese cypress"
    ),
    metric = case_when(
      grepl("^dis", variable) ~ "Start of Pollen Season",
      grepl("^maxdis", variable) ~ "Peak of Pollen Season",
      grepl("^max_", variable) ~ "Maximum Daily Pollen",
      grepl("^sum_", variable) ~ "Total Pollen Concentration"
    )
  ) %>%
  filter(!is.na(value))

dat_long_all <- dat_long %>%
  mutate(
    lc_group = as.character(lc_group)
  ) %>%
  bind_rows(
    dat_long %>%
      mutate(lc_group = "All")
  ) %>%
  mutate(
    metric = factor(
      metric,
      levels = c(
        "Start of Pollen Season",
        "Peak of Pollen Season",
        "Maximum Daily Pollen",
        "Total Pollen Concentration"
      )
    ),
    lc_group = factor(
      lc_group,
      levels = c(
        "Broadleaf forest",
        "Needleleaf forest",
        "Mixed forest",
        "Cropland",
        "Grassland",
        "Urban",
        "All"
      )
    )
  )

lc_cols2 <- c(
  "Broadleaf forest"  = "#2E7D32",
  "Needleleaf forest" = "#1B5E20",
  "Mixed forest"      = "#66BB6A",
  "Cropland"          = "#FBC02D",
  "Grassland"         = "#C0CA33",
  "Urban"             = "#D32F2F",
  "All"               = "grey50"
)

mean_lc <- dat_long_all %>%
  group_by(species, metric, lc_group) %>%
  summarise(
    mean_value = mean(value, na.rm = TRUE),
    sd_value = sd(value, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )

station_slopes <- dat_long_all %>%
  group_by(species, metric, lc_group, StationID) %>%
  filter(n_distinct(year) >= 5) %>%
  summarise(
    slope = coef(lm(value ~ year))[2],
    .groups = "drop"
  )

p_mean <- ggplot(mean_lc, aes(x = lc_group, y = mean_value, fill = lc_group)) +
  geom_col(width = 0.7, color = "black", linewidth = 0.25) +
  geom_errorbar(
    aes(ymin = mean_value - sd_value, ymax = mean_value + sd_value),
    width = 0.2,
    linewidth = 0.4
  ) +
  facet_grid(metric ~ species, scales = "free_y") +
  theme_classic(base_size = 12) +
  labs(
    x = "Land-cover type",
    y = "Long-term mean pollen metric",
    fill = "Land cover"
  ) +
  theme(
    axis.text.x = element_text(angle = 35, hjust = 1),
    strip.text = element_text(face = "bold"),
    legend.position = "none"
  )+
  scale_fill_manual(values = lc_cols2)+
  theme(
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 13),
    axis.text.x = element_text(size = 13, angle = 35, hjust = 1),
    strip.text = element_text(size = 12, face = "bold"),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12)
  )

p_mean


slope_lc <- station_slopes %>%
  group_by(species, metric, lc_group) %>%
  summarise(
    mean_slope = mean(slope, na.rm = TRUE),
    sd_slope = sd(slope, na.rm = TRUE),
    n_station = n(),
    .groups = "drop"
  )

p_slope <- ggplot(slope_lc, aes(x = lc_group, y = mean_slope, fill = lc_group)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40", linewidth = 0.4) +
  geom_col(width = 0.7, color = "black", linewidth = 0.25) +
  geom_errorbar(
    aes(ymin = mean_slope - sd_slope, ymax = mean_slope + sd_slope),
    width = 0.2,
    linewidth = 0.4
  ) +
  facet_grid(metric ~ species, scales = "free_y") +
  theme_classic(base_size = 12) +
  labs(
    x = "Land-cover type",
    y = "Temporal trend slope per year",
    fill = "Land cover"
  ) +
  theme(
    axis.text.x = element_text(angle = 35, hjust = 1),
    strip.text = element_text(face = "bold"),
    legend.position = "none"
  )+
  scale_fill_manual(values = lc_cols2)+
  theme(
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 13),
    axis.text.x = element_text(size = 13, angle = 35, hjust = 1),
    strip.text = element_text(size = 12, face = "bold"),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12)
  )

p_slope

library(patchwork)

p_lc_combined <- (p_mean | p_slope) +
  plot_layout(guides = "collect") +
  plot_annotation(
    tag_levels = "a"
  )

p_lc_combined

ggsave(
  "visuals/landcover_mean_and_slope_barplots2.png",
  p_lc_combined,
  width = 18,
  height = 11,
  dpi = 300
)

# Map slope
library(dplyr)
library(tidyr)
library(ggplot2)
library(maps)
library(patchwork)
library(viridis)

japan_map <- map_data("world") %>%
  filter(region == "Japan")

library(purrr)

station_loc <- df2 %>%
  mutate(
    Longitude = as.numeric(Longitude),
    Latitude  = as.numeric(Latitude)
  ) %>%
  filter(!is.na(Longitude), !is.na(Latitude)) %>%
  group_by(StationID) %>%
  summarise(
    Longitude = mean(Longitude, na.rm = TRUE),
    Latitude  = mean(Latitude, na.rm = TRUE),
    .groups = "drop"
  )

station_slope_map <- dat_long %>%
  filter(!is.na(value), !is.na(year)) %>%
  group_by(StationID, species, metric) %>%
  filter(n_distinct(year) >= 5) %>%
  summarise(
    model = list(lm(value ~ year)),
    .groups = "drop"
  ) %>%
  mutate(
    slope = map_dbl(model, ~ coef(.x)[2]),
    p_value = map_dbl(model, ~ summary(.x)$coefficients["year", "Pr(>|t|)"])
  ) %>%
  dplyr::select(-model) %>%
  left_join(station_loc, by = "StationID") %>%
  mutate(
    metric = factor(
      metric,
      levels = c(
        "Start of Pollen Season",
        "Peak of Pollen Season",
        "Maximum Daily Pollen",
        "Total Pollen Concentration"
      )
    ),
    species = factor(species, levels = c("Japanese cedar", "Japanese cypress"))
  ) %>%
  filter(!is.na(Longitude), !is.na(Latitude), !is.na(slope))

metrics_order <- c(
  "Start of Pollen Season",
  "Peak of Pollen Season",
  "Maximum Daily Pollen",
  "Total Pollen Concentration"
)


make_slope_map <- function(dat, sp, met) {
  
  d <- dat %>%
    filter(species == sp, metric == met)
  
  n_all <- nrow(d)
  
  pct_neg <- round(100 * sum(d$slope < 0, na.rm = TRUE) / n_all)
  pct_pos <- round(100 * sum(d$slope > 0, na.rm = TRUE) / n_all)
  
  pct_neg_sig <- round(100 * sum(d$slope < 0 & d$p_value < 0.05, na.rm = TRUE) / n_all)
  pct_pos_sig <- round(100 * sum(d$slope > 0 & d$p_value < 0.05, na.rm = TRUE) / n_all)
  
  neg_text <- paste0(pct_neg, "% (", pct_neg_sig, "%*)")
  pos_text <- paste0(pct_pos, "% (", pct_pos_sig, "%*)")
  
  xr <- range(d$slope, na.rm = TRUE)
  
  text_df <- data.frame(
    x = c(
      xr[1] + diff(xr) * 0.20,   # blue: left
      xr[2] - diff(xr) * 0.20    # red: right
    ),
    y = c(Inf, Inf),
    lab = c(neg_text, pos_text),
    hjust = c(0, 1),
    col = c("#2166AC", "#B2182B")
  )
  
  hist_breaks <- case_when(
    met == "Maximum Daily Pollen" & sp == "Japanese cypress" ~
      list(c(-200, 0, 200)),
    
    met == "Total Pollen Concentration" ~
      list(c(-1000, 0, 1000)),
    
    TRUE ~ list(waiver())
  )[[1]]
  
  hist_grob <- ggplotGrob(
    ggplot(d, aes(x = slope)) +
      geom_histogram(
        bins = 20,
        fill = "grey70",
        color = "white",
        linewidth = 0.2
      ) +
      geom_vline(
        xintercept = 0,
        linetype = "dashed",
        color = "black",
        linewidth = 0.3
      ) +
      scale_x_continuous(
        breaks = hist_breaks
        )+
      geom_text(
        data = text_df,
        aes(
          x = x,
          y = y,
          label = lab
        ),
        vjust = -0.5,
        color = text_df$col,
        fontface = "bold",
        size = 2.5,
        inherit.aes = FALSE
      )+
      theme_classic(base_size = 9) +
      labs(x = NULL, y = "Count") +
      coord_cartesian(clip = "off") +
      theme(
        axis.text = element_text(size = 8, color = "black"),
        axis.title.y = element_text(size = 8),
        plot.margin = margin(18, 3, 2, 3)
      )
  )
  
  legend_title <- case_when(
    met %in% c(
      "Start of Pollen Season",
      "Peak of Pollen Season"
    ) ~ "Slope\n(days yr\u207B\u00B9)",
    
    met == "Maximum Daily Pollen" ~
      "Slope\n(grains cm\u207B\u00B2 yr\u207B\u00B9)",
    
    met == "Total Pollen Concentration" ~
      "Slope\n(grains cm\u207B\u00B2 yr\u207B\u00B9)"
  )

    color_limits <- case_when(
      
      met == "Start of Pollen Season" ~
        list(c(-2.5, 2)),
      
      met == "Peak of Pollen Season" & sp == "Japanese cedar" ~
        list(c(-3, 2)),
      
      met == "Peak of Pollen Season" & sp == "Japanese cypress" ~
        list(c(-1.5, 1.5)),
      
      met == "Maximum Daily Pollen" & sp == "Japanese cedar" ~
        list(c(-250, 100)),
      
      met == "Maximum Daily Pollen" & sp == "Japanese cypress" ~
        list(c(-100, 80)),
      
      met == "Total Pollen Concentration" & sp == "Japanese cedar" ~
        list(c(-750, 500)),
      
      met == "Total Pollen Concentration" & sp == "Japanese cypress" ~
        list(c(-250, 250))
      
    )[[1]]
  
  ggplot() +
    geom_polygon(
      data = japan_map,
      aes(x = long, y = lat, group = group),
      fill = "white",
      color = "grey70",
      linewidth = 0.25
    ) +
    geom_point(
      data = d,
      aes(x = Longitude, y = Latitude, fill = slope),
      shape = 21,
      size = 2.0,
      stroke = 0.25,
      color = "grey35",
      alpha = 0.95
    ) +
    scale_fill_gradient2(
      low = "#2166AC",
      mid = "white",
      high = "#B2182B",
      midpoint = 0,
      limits = color_limits,
      oob = scales::squish,
      name = legend_title
    ) +
    annotation_custom(
      hist_grob,
      xmin = 127.8,
      xmax = 136.8,
      ymin = 39.2,
      ymax = 46.2
    ) +
    coord_fixed(xlim = c(128, 146), ylim = c(30, 46)) +
    theme_classic(base_size = 13) +
    labs(
      title = NULL,
      x = "Longitude",
      y = "Latitude"
    ) +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5, size = 10),
      axis.text = element_text(size = 10, color = "black"),
      axis.title = element_text(size = 12),
      legend.title = element_text(size = 11),
      legend.text = element_text(size = 10),
      panel.border = element_blank(),
      axis.line = element_line(color = "black",linewidth = 0.5)
    )
}


library(grid)

# Create maps
p_list <- list()

for (m in metrics_order) {
  p_list[[paste("Japanese cedar", m)]] <-
    make_slope_map(station_slope_map, "Japanese cedar", m)
  
  p_list[[paste("Japanese cypress", m)]] <-
    make_slope_map(station_slope_map, "Japanese cypress", m)
}

# ===== Add panel labels =====

tags <- c("(a)", "(b)", "(c)", "(d)", "(e)", "(f)", "(g)", "(h)")

names <- c(
  "Japanese cedar Start of Pollen Season",
  "Japanese cypress Start of Pollen Season",
  "Japanese cedar Peak of Pollen Season",
  "Japanese cypress Peak of Pollen Season",
  "Japanese cedar Maximum Daily Pollen",
  "Japanese cypress Maximum Daily Pollen",
  "Japanese cedar Total Pollen Concentration",
  "Japanese cypress Total Pollen Concentration"
)

for(i in seq_along(names)){
  p_list[[names[i]]] <-
    p_list[[names[i]]] +
    labs(tag = tags[i]) +
    theme(
      plot.tag = element_text(
        face = "bold",
        size = 11
      ),
      plot.tag.position = c(0.02, 0.98)
    )
}

# ===== Then continue with headers =====

cedar_header <- wrap_elements(
  full = textGrob(
    "Japanese cedar",
    gp = gpar(fontsize = 16, fontface = "bold")
  )
)

cypress_header <- wrap_elements(
  full = textGrob(
    "Japanese cypress",
    gp = gpar(fontsize = 16, fontface = "bold")
  )
)

blank_header <- plot_spacer()


start_label <- wrap_elements(
  full = textGrob(
    "Start of Pollen Season",
    rot = 90,
    gp = gpar(fontsize = 14, fontface = "bold")
  )
)

peak_label <- wrap_elements(
  full = textGrob(
    "Peak of Pollen Season",
    rot = 90,
    gp = gpar(fontsize = 14, fontface = "bold")
  )
)

max_label <- wrap_elements(
  full = textGrob(
    "Maximum Daily Pollen",
    rot = 90,
    gp = gpar(fontsize = 14, fontface = "bold")
  )
)

total_label <- wrap_elements(
  full = textGrob(
    "Total Pollen Concentration",
    rot = 90,
    gp = gpar(fontsize = 14, fontface = "bold")
  )
)

# Column headers
header_row <-
  (
    plot_spacer() |
      cedar_header |
      cypress_header
  ) +
  plot_layout(widths = c(0.07, 1, 1))

# Start row
row_start <-
  (
    start_label |
      p_list[["Japanese cedar Start of Pollen Season"]] |
      p_list[["Japanese cypress Start of Pollen Season"]]
  ) +
  plot_layout(widths = c(0.07, 1, 1))

# Peak row
row_peak <-
  (
    peak_label |
      p_list[["Japanese cedar Peak of Pollen Season"]] |
      p_list[["Japanese cypress Peak of Pollen Season"]]
  ) +
  plot_layout(widths = c(0.07, 1, 1))

# Maximum daily pollen row
row_max <-
  (
    max_label |
      p_list[["Japanese cedar Maximum Daily Pollen"]] |
      p_list[["Japanese cypress Maximum Daily Pollen"]]
  ) +
  plot_layout(widths = c(0.07, 1, 1))

# Total pollen concentration row
row_total <-
  (
    total_label |
      p_list[["Japanese cedar Total Pollen Concentration"]] |
      p_list[["Japanese cypress Total Pollen Concentration"]]
  ) +
  plot_layout(widths = c(0.07, 1, 1))

p_slope_map_hist <-
  header_row /
  row_start /
  row_peak /
  row_max /
  row_total +
  plot_layout(
    heights = c(0.07, 1, 1, 1, 1)
  )

p_slope_map_hist

ggsave(
  "visuals/station_slope_maps_with_histograms4.png",
  p_slope_map_hist,
  width = 12,
  height = 14,
  dpi = 300
)

# Four Map
library(terra)
library(sf)
library(dplyr)
library(ggplot2)
library(patchwork)
library(viridis)
alan_rast <- rast("~/Desktop/RAWjapan/pollen_rs/ALANMap2023/ALANMap2023.tif")
lc_rast   <- rast("~/Desktop/RAWjapan/pollen_rs/LCMap2022/merge.tif")
plot(lc_rast)
jpn_ext <- ext(128, 146, 30, 46)

lc_crop <- crop(lc_rast, jpn_ext)
lc_group <- classify(
  lc_crop,
  rcl = matrix(
    c(
      10, 1, 11, 1, 12, 1, 20, 1,              # Cropland
      51, 2, 52, 2, 61, 2, 62, 2,              # Broadleaf forest
      71, 3, 72, 3,                            # Needleleaf forest
      91, 4, 92, 4,                            # Mixed forest
      120, 5, 130, 6,                          # Shrubland, Grassland
      190, 7,                                  # Urban
      200, 8,                                  # Bare areas
      210, 9                                   # Water
    ),
    ncol = 2,
    byrow = TRUE
  ),
  others = NA
)

lc_small <- aggregate(lc_group, fact = 5, fun = "modal", na.rm = TRUE)

lc_df <- as.data.frame(lc_small, xy = TRUE, na.rm = TRUE)
colnames(lc_df)[3] <- "lc_code"

lc_df <- lc_df %>%
  mutate(
    landcover = factor(
      lc_code,
      levels = 1:9,
      labels = c(
        "Cropland",
        "Broadleaf forest",
        "Needleleaf forest",
        "Mixed forest",
        "Shrubland",
        "Grassland",
        "Urban",
        "Bare areas",
        "Water"
      )
    )
  )

lc_cols <- c(
  "Cropland"          = "#FBC02D",
  "Broadleaf forest"  = "#2E7D32",
  "Needleleaf forest" = "#1B5E20",
  "Mixed forest"      = "#66BB6A",
  "Shrubland"         = "#8D6E63",
  "Grassland"         = "#C0CA33",
  "Urban"             = "#D32F2F",
  "Bare areas"        = "#A1887F",
  "Water"             = "#1E88E5"
)

p_lc <- ggplot() +
  geom_tile(
    data = lc_df,
    aes(x = x, y = y, fill = landcover)
  ) +
  scale_fill_manual(
    values = lc_cols,
    name = "Land cover",
    na.value = "transparent"
  ) +
  coord_fixed(
    xlim = c(128, 146),
    ylim = c(30, 46),
    expand = FALSE
  ) +
  labs(
    title = "b) Land-cover map of Japan",
    x = "Longitude",
    y = "Latitude"
  ) +
  map_theme

p_lc

# Downsample for faster plotting
alan_crop  <- crop(alan_rast, jpn_ext)
alan_small <- aggregate(alan_crop, fact = 5, fun = "mean")

# Log-transform ALAN
alan_log <- log10(alan_small + 1)

# Convert raster to dataframe for ggplot
alan_df <- as.data.frame(alan_log, xy = TRUE, na.rm = TRUE)
colnames(alan_df)[3] <- "logALAN"

map_theme <- theme_classic(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 13),
    axis.title = element_text(size = 11, color = "black"),
    axis.text = element_text(size = 9, color = "black"),
    axis.line = element_line(color = "black", linewidth = 0.35),
    axis.ticks = element_line(color = "black", linewidth = 0.35),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9)
  )

p_alan <- ggplot() +
  geom_raster(
    data = alan_df,
    aes(x = x, y = y, fill = logALAN)
  ) +
  scale_fill_viridis_c(
    option = "magma",
    name = expression(log[10](ALAN + 1))
  ) +
  coord_quickmap(
    xlim = c(128, 146),
    ylim = c(30, 46),
    expand = FALSE
  ) +
  theme_classic(base_size = 12) +
  labs(
    title = "a) Artificial light at night",
    x = "Longitude",
    y = "Latitude"
  )+map_theme

p_alan

df_station <- df_temp %>%
  mutate(
    Longitude = as.numeric(Longitude),
    Latitude  = as.numeric(Latitude)
  ) %>%
  group_by(StationID, Longitude, Latitude) %>%
  summarise(
    years_available = n_distinct(year),
    .groups = "drop"
  )

p_station <- ggplot() +
  geom_polygon(
    data = japan_map,
    aes(x = long, y = lat, group = group),
    fill = "gray92",
    color = "gray50",
    linewidth = 0.3
  ) +
  geom_point(
    data = df_station,
    aes(
      x = Longitude,
      y = Latitude,
      color = years_available
    ),
    size = 1.5,
    alpha = 0.9
  ) +
  scale_color_viridis_c(
    option = "plasma",
    name = "Number of years"
  ) +
  coord_fixed(xlim = c(128, 146), ylim = c(30, 46)) +
  labs(
    title = "c) Pollen monitoring stations",
    x = "Longitude",
    y = "Latitude"
  ) +
  map_theme

p_station

p_maps <- p_alan + p_lc + p_station
p_maps

ggsave(filename ='visuals/FigureS1 map.png',dpi = 300,width =14,height = 8) 

#Temperature+Station map
# -----------------------------
# 1) LOAD PACKAGES
# -----------------------------
library(ggplot2)
library(dplyr)
library(maps)

# -----------------------------
# 2) GET JAPAN MAP DATA
# -----------------------------
japan_map <- map_data("world") %>%
  filter(region == "Japan")

# -----------------------------
# 3) PLOT JAPAN MAP
# -----------------------------
p_japan <- ggplot() +
  geom_polygon(
    data = japan_map,
    aes(x = long, y = lat, group = group),
    fill = "gray92",
    color = "gray40",
    linewidth = 0.3
  ) +
  coord_fixed(xlim = c(128, 146), ylim = c(30, 46)) +
  theme_classic() +
  labs(
    title = "Map of Japan",
    x = "Longitude",
    y = "Latitude"
  )
p_japan

library(readxl)
library(dplyr)
library(ggplot2)
library(maps)
library(viridis)

df_temp <- read_excel("~/Desktop/Merged_files_annual.xlsx")

df_temp <- df_temp %>%
  mutate(
    Longitude = as.numeric(Longitude),
    Latitude  = as.numeric(Latitude),
    annual_mean_Tmax_C = as.numeric(annual_mean_Tmax_C),
    annual_mean_Tmin = as.numeric(annual_mean_Tmin),
    annual_mean_monthly_Precip_mm = as.numeric(annual_mean_monthly_Precip_mm)
  ) %>%
  filter(!is.na(Longitude), !is.na(Latitude))

p_tmax <- ggplot() +
  geom_polygon(
    data = japan_map,
    aes(x = long, y = lat, group = group),
    fill = "gray92",
    color = "gray50",
    linewidth = 0.3
  ) +
  geom_point(
    data = df_temp,
    aes(x = Longitude, y = Latitude, color = annual_mean_Tmax_C),
    size = 2,
    alpha = 0.85
  ) +
  scale_color_viridis_c(
    option = "plasma",
    name = expression(T[max]~degree*C)
  ) +
  coord_fixed(xlim = c(128, 146), ylim = c(30, 46)) +
  theme_classic(base_size = 12) +
  labs(
    title = "Annual mean maximum temperature",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "right"
  )

p_tmax

p_tmin <- ggplot() +
  geom_polygon(
    data = japan_map,
    aes(x = long, y = lat, group = group),
    fill = "gray92",
    color = "gray50",
    linewidth = 0.3
  ) +
  geom_point(
    data = df_temp,
    aes(x = Longitude, y = Latitude, color = annual_mean_Tmin),
    size = 2,
    alpha = 0.85
  ) +
  scale_color_viridis_c(
    option = "plasma",
    name = expression(T[min]~degree*C)
  ) +
  coord_fixed(xlim = c(128, 146), ylim = c(30, 46)) +
  theme_classic(base_size = 12) +
  labs(
    title = "Annual mean minimum temperature",
    x = "Longitude",
    y = "Latitude"
  )+
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "right"
  )

p_tmin

p_precip <- ggplot() +
  geom_polygon(
    data = japan_map,
    aes(x = long, y = lat, group = group),
    fill = "gray92",
    color = "gray50",
    linewidth = 0.3
  ) +
  geom_point(
    data = df_temp,
    aes(x = Longitude, y = Latitude, color = annual_mean_monthly_Precip_mm),
    size = 2,
    alpha = 0.85
  ) +
  scale_color_viridis_c(
    option = "viridis",
    name = "Precipitation\n(mm)"
  ) +
  coord_fixed(xlim = c(128, 146), ylim = c(30, 46)) +
  theme_classic(base_size = 12) +
  labs(
    title = "Annual mean monthly precipitation",
    x = "Longitude",
    y = "Latitude"
  )+theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "right"
  )

p_precip

library(patchwork)

p_climate <- p_tmax + p_tmin + p_precip +
  plot_layout(ncol = 3)

p_climate

ggsave(
  "visuals/japan_climate_station_maps.png",
  p_climate,
  width = 15,
  height = 5,
  dpi = 300
)

#ALAN+Temperature 
df <- read_excel("~/Desktop/Merged_files_copy.xlsx")

df <- df %>%
  mutate(
    dis_hinoki_doy = yday(as.Date(as.numeric(dis_hinoki),
                                  origin = "1899-12-30")),
    maxdis_hinoki_doy=yday(as.Date(as.numeric(maxdis_hinoki),
                                   origin = "1899-12-30")),
    end_hinoki_doy = yday(as.Date(as.numeric(end_hinoki),
                          origin = "1899-12-30")),
    dis_sugi_doy = yday(as.Date(as.numeric(dis_sugi),
                                 origin = "1899-12-30")),
    maxdis_sugi_doy=yday(as.Date(as.numeric(maxdis_sugi),
                                   origin = "1899-12-30")),
    end_sugi_doy = yday(as.Date(as.numeric(end_sugi),
                                  origin = "1899-12-30")),
    # Peak pollen concentration
    max_hinoki = as.numeric(max_hinoki),
    max_sugi   = as.numeric(max_sugi)
    )

df <- df %>%
  mutate(
    ALAN = as.numeric(ALAN),
    log_alan = log(ALAN+1),
    across(
      c(Start_mean_Tmax, Start_mean_Tmin,
        Peak_mean_Tmax, Peak_mean_Tmin,
        End_mean_Tmax, End_mean_Tmin),
      as.numeric
    )
  )

#calculate years
df %>%
  select(StationID, year, ALAN) %>%
  filter(!is.na(ALAN)) %>%
  group_by(year) %>%
  summarise(
    n_stations = n_distinct(StationID),
    mean_ALAN  = mean(ALAN, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(year)

ggplot(df, aes(x = log_alan, y = dis_hinoki_doy, color = Start_mean_Tmax)) +
  geom_point(alpha = 0.75, size = 2.2) +
  geom_smooth(
    method = "lm",
    se = TRUE,
    color = "black",
    fill = "grey80"
  ) +
  scale_color_viridis_c(
    name = expression(T[max]~degree*C)
  ) +
  theme_classic(base_size = 14) +
  labs(
    x = "log(ALAN + 1)",
    y = "Start of Hinoki pollen season (DOY)"
  )

dat_long <- df %>%
  dplyr::select(
    StationID, log_alan,
    dis_hinoki_doy, maxdis_hinoki_doy, max_hinoki,
    dis_sugi_doy, maxdis_sugi_doy, max_sugi,
    Start_mean_Tmax, Peak_mean_Tmax
  ) %>%
  pivot_longer(
    cols = c(
      dis_hinoki_doy, maxdis_hinoki_doy, max_hinoki,
      dis_sugi_doy, maxdis_sugi_doy, max_sugi
    ),
    names_to = "metric",
    values_to = "value"
  ) %>%
  mutate(
    species = case_when(
      grepl("hinoki", metric) ~ "Japanese cypress",
      grepl("sugi", metric) ~ "Japanese cedar"
    ),
    phase = case_when(
      grepl("^dis", metric) ~ "Start of Pollen Season",
      grepl("^maxdis", metric) ~ "Peak of Pollen Season",
      grepl("^max_", metric) ~ "Maximum Daily Pollen"
    ),
    temp = case_when(
      phase == "Start of Pollen Season" ~ Start_mean_Tmax,
      phase == "Peak of Pollen Season" ~ Peak_mean_Tmax,
      phase == "Maximum Daily Pollen" ~ Peak_mean_Tmax
    ),
    panel = paste(phase, species, sep = " | ")
  ) %>%
  filter(!is.na(log_alan), !is.na(value), !is.na(temp))

p <- ggplot(dat_long, aes(x = log_alan, y = value, color = temp)) +
  geom_point(alpha = 0.7, size = 1.8) +
  geom_smooth(
    method = "lm",
    se = TRUE,
    color = "black",
    fill = "grey80"
  ) +
  facet_wrap(~ panel, ncol = 2, scales = "free_y") +
  scale_color_viridis_c(
    name = expression(T[max]~degree*C)
  ) +
  theme_classic(base_size = 12) +
  labs(
    x = "log(ALAN + 1)",
    y = "Pollen timing / peak concentration"
  )

p

p2 <- ggplot(dat_long, aes(x = log_alan, y = value, color = temp)) +
  geom_point(alpha = 0.65, size = 1.8) +
  geom_smooth(
    method = "lm",
    se = TRUE,
    color = "black",
    fill = "grey80",
    linewidth = 0.8
  ) +
  facet_wrap(~ panel, ncol = 2, scales = "free_y") +
  scale_color_viridis_c(
    option = "plasma",
    name = expression(T[max]~degree*C)
  ) +
  theme_classic(base_size = 13) +
  theme(
    strip.background = element_blank(),
    strip.text = element_text(face = "bold", size = 12),
    axis.title = element_text(size = 13),
    axis.text = element_text(size = 11, color = "black"),
    axis.line = element_line(color = "black", linewidth = 0.5),
    axis.ticks = element_line(color = "black", linewidth = 0.5),
    legend.title = element_text(size = 11),
    legend.text = element_text(size = 10),
    panel.spacing = unit(1.2, "lines"),
    plot.title = element_blank()
  ) +
  labs(
    x = "log(ALAN + 1)",
    y = ""
  )

p2

ggsave(filename ='visuals/ALAN+T-pollen.png',dpi = 300,width =10,height = 8)

table(df$dis_hinoki_doy, useNA = "always")

m_start <- lm(dis_hinoki_DOY ~ log_alan + Start_mean_Tmax + Start_mean_Tmin, data = df)
m_peak  <- lm(maxdis_hinoki_DOY ~ log_alan + Peak_mean_Tmax + Peak_mean_Tmin, data = df)

summary(m_start)
summary(m_peak)


m_start_sugi <- lm(dis_sugi_DOY ~ log_alan + Start_mean_Tmax + Start_mean_Tmin, data = df)
m_peak_sugi  <- lm(maxdis_sugi_DOY ~ log_alan + Peak_mean_Tmax + Peak_mean_Tmin, data = df)

summary(m_start_sugi)
summary(m_peak_sugi)

# Number of stations
pollen <- read_csv("~/Desktop/RAWjapan/pollen_filtered_min5years.csv")  
station_data <- read_excel("~/Desktop/station_locations_unique.xlsx")
pollen_fixed <- pollen %>%
  dplyr::select(-`Latitude and longitude`) %>%  # remove old version
  left_join(
    station_data %>% dplyr::select(StationID, `Latitude and longitude`),
    by = "StationID"
  )
alan   <- read_csv("~/Desktop/RAWjapan/pollen_rs/ALAN/ALAN.csv") %>%
  dplyr::select(Latitude, Longitude, StationID, avg_rad,year)                  
land   <- read_csv("~/Desktop/RAWjapan/pollen_rs/LC/LC.csv") %>% 
  dplyr::select(Latitude, Longitude, StationID, lc,year) 
settle <- read_csv("~/Desktop/RAWjapan/pollen_rs/GHS/GHS.csv") %>% 
  dplyr::select(Latitude, Longitude, StationID, b1,year) 

# 2. Start merging step-by-step by StationID (and Year if applicable)
merged_env <- alan %>%
  full_join(land,   by = c("StationID", "year", "Latitude", "Longitude")) %>%
  full_join(settle, by = c("StationID", "year", "Latitude", "Longitude"))

pollen_env <- pollen %>%
  left_join(merged_env, by = c("StationID", "year"))

# Analysis

landcover_labels <- c(
  "10"  = "Rainfed cropland",
  "11"  = "Herbaceous cropland",
  "20"  = "Irrigated cropland",
  "51"  = "Open evergreen broadleaved forest",
  "61"  = "Open deciduous broadleaved forest",
  "62"  = "Closed deciduous broadleaved forest",
  "72"  = "Closed evergreen needle-leaved forest",
  "190" = "Impervious surfaces",
  "210" = "Water body"
)

landcover_colors <- c(
  "10"  = rgb(255,255,100, maxColorValue=255),
  "11"  = rgb(255,255,100, maxColorValue=255),
  "20"  = rgb(170,240,240, maxColorValue=255),
  "51"  = rgb(76,115,0,    maxColorValue=255),
  "61"  = rgb(170,200,0,   maxColorValue=255),
  "62"  = rgb(0,160,0,     maxColorValue=255),
  "72"  = rgb(0,60,0,      maxColorValue=255),
  "190" = rgb(195,20,0,    maxColorValue=255),
  "210" = rgb(0,70,200,    maxColorValue=255)
)

# Line plot
library(dplyr)
library(ggplot2)
library(tidyr)

# Map detailed LC codes → broad groups
pollen_env_grp <- pollen_env %>%
  filter(!is.na(lc)) %>%
  mutate(
    lc_chr   = as.character(lc),
    lc_group = case_when(
      lc_chr %in% c("10","11","20")            ~ "Cropland",
      lc_chr %in% c("51","61","62","72")       ~ "Forest",
      lc_chr == "190"                          ~ "Urban (Impervious)",
      lc_chr == "210"                          ~ "Water",
      TRUE                                     ~ "Other"
    )
  )

# A simple palette for the 4 groups
lc_group_cols <- c(
  "Cropland"           = "#FFD54F",  # yellow-orange
  "Forest"             = "#2E7D32",  # green
  "Urban (Impervious)" = "#C62828",  # red
  "Water"              = "#1E88E5"   # blue
)

station_count_grp <- pollen_env_grp %>%
  group_by(year, lc_group) %>%
  summarise(n_stations = n_distinct(StationID), .groups = "drop")

ggplot(station_count_grp,
       aes(x = year, y = n_stations, color = lc_group)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  scale_color_manual(values = lc_group_cols, name = "Land cover group") +
  theme_minimal() +
  labs(title = "Number of pollen monitoring stations by land cover group",
       x = "Year", y = "Number of stations")

# Replace dis_hinoki / dis_sugi with your metric names if different
station_means_grp <- pollen_env_grp %>%
  group_by(StationID, lc_group) %>%
  summarise(
    mean_hinoki = mean(dis_hinoki, na.rm = TRUE),
    mean_sugi   = mean(dis_sugi,   na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(c(mean_hinoki, mean_sugi),
               names_to = "Species", values_to = "Mean_DOY")

ggsave(filename ='visuals/number of pollen station.png',dpi = 300,width =6,height = 4) 

#ALAN and land cover regression

library(dplyr)
library(ggplot2)
library(broom)

# ==== 1. Define Urban vs Rural ====
pollen_urban_rural <- pollen_env %>%
  filter(lc %in% c("10","11","20","51","61","62","72","190")) %>%  # keep main terrestrial types
  mutate(
    lc_chr = as.character(lc),
    lc_group = case_when(
      lc_chr == "190" ~ "Urban",
      TRUE ~ "Rural"
    )
  )

# ==== 2. Fit interaction model: ALAN × Region ====
# Hinoki start
m_hinoki_ur <- lm(dis_hinoki_doy ~ log_alan * lc_group, data = pollen_urban_rural)
summary(m_hinoki_ur)

# Sugi start
m_sugi_ur <- lm(dis_sugi_doy ~ log_alan * lc_group, data = pollen_urban_rural)
summary(m_sugi_ur)

# ==== 3. Compare slopes for each region ====
hinoki_slopes <- pollen_urban_rural %>%
  group_by(lc_group) %>%
  do(tidy(lm(dis_hinoki_doy ~ log_alan, data = .))) %>%
  filter(term == "log_alan") %>%
  mutate(Species = "Hinoki") %>%
  select(Species, lc_group, estimate, p.value)

sugi_slopes <- pollen_urban_rural %>%
  group_by(lc_group) %>%
  do(tidy(lm(dis_sugi_doy ~ log_alan, data = .))) %>%
  filter(term == "log_alan") %>%
  mutate(Species = "Sugi") %>%
  select(Species, lc_group, estimate, p.value)

slopes_ur <- bind_rows(hinoki_slopes, sugi_slopes)
print(slopes_ur)

# ==== 4. Plot ====
ur_colors <- c("Urban" = "#C62828", "Rural" = "#2E7D32")

ggplot(pollen_urban_rural, aes(x = log_alan, y = dis_hinoki_doy, color = lc_group)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE) +
  scale_color_manual(values = ur_colors) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Urban vs Rural Sensitivity to ALAN — Hinoki Pollen Start",
    x = "log(ALAN + 1, W/m²·sr)",
    y = "DOY (First Hinoki Pollen Detection)",
    color = "Region"
  )
ggsave(filename ='visuals/hinoki_urban_rural.png',dpi = 300,width =6,height = 4)

ggplot(pollen_urban_rural, aes(x = log_alan, y = dis_sugi_doy, color = lc_group)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE) +
  scale_color_manual(values = ur_colors) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Urban vs Rural Sensitivity to ALAN — Sugi Pollen Start",
    x = "log(ALAN + 1, W/m²·sr)",
    y = "DOY (First Sugi Pollen Detection)",
    color = "Region"
  )
ggsave(filename ='visuals/sugi_urban_rural.png',dpi = 300,width =6,height = 4)
