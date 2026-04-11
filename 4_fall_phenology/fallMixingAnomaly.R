library(tidyverse)
library(lubridate)
library(patchwork)
library(NTLlakeloads)

ntl_phys <- loadLTERtemp() %>%
  rename_with(tolower) %>%
  mutate(sampledate = as.Date(sampledate)) %>%
  filter(!is.na(wtemp), !is.na(o2)) # Remove rows with missing data

# 1. Identify the 'Fall Mixing' date for every lake/year
fall_mixing_data <- ntl_phys %>%
  filter(!(lakeid == 'CR' & year4 %in% c(2012,2013))) |> 
  filter(year4 >= 1982) |> 
  filter(lakeid %in% c('TR','TB','SP','CR','CB','BM','AL')) |> 
  mutate(
    year = year(sampledate),
    month = month(sampledate),
    doy = yday(sampledate),
    mix_year = if_else(month < 7, year - 1, year),   # 🔑 key fix
    doy_water = if_else(doy < 182, doy + 365, doy)   # keeps DOY continuous
  ) %>%
  filter(month >= 9) %>%  # fall window
  group_by(lakeid, mix_year, sampledate) %>%
  summarise(
    temp_gap = max(wtemp, na.rm = TRUE) - min(wtemp, na.rm = TRUE),
    doy_water = first(doy_water),
    .groups = "drop"
  ) %>%
  filter(temp_gap <= 2.0) %>%
  group_by(lakeid, mix_year) %>%
  slice_min(sampledate, n = 1) %>%
  rename(year = mix_year) %>%   # keep downstream code unchanged
  mutate(day_of_year = doy_water)

# 1. Calculate Anomalies
fall_anomalies <- fall_mixing_data %>%
  filter(lakeid %in% c('AL','TR')) |> 
  group_by(lakeid) %>%
  mutate(mean_mix_day = mean(day_of_year, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(anomaly = day_of_year - mean_mix_day) |> 
  mutate(lake_name = case_match(lakeid,
                                "TR" ~ "Trout",
                                "AL" ~ "Allequash",
                                "ME" ~ 'Mendota',
                                .default = lakeid)) # Keeps the original ID if not matched
  

# 2. The Anomaly Heatmap
p.fall = ggplot(fall_anomalies, aes(x = year, y = lake_name, fill = anomaly)) +
  geom_tile(color = "white", linewidth = 0.2) +
  # Diverging scale: Blue (Early/Cold), White (Average), Red (Late/Warm)
  scale_fill_gradient2(
    low = "#0571b0",      # Deep Blue
    mid = "grey90",        # Neutral
    high = "#ca0020",     # Deep Red
    midpoint = 0,
    name = "Days") +
  # Remove gaps on X and Y axes
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  theme_minimal(base_size = 9) +
  theme(panel.grid = element_blank(),
        legend.title = element_text(size = 8),
        legend.key.width = unit(0.3,'cm'),
        legend.key.height = unit(0.3,'cm'),
        axis.title = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_text(angle = 90, hjust = 0.5, vjust = 0.5))

# ggsave(p.fall, filename = 'fallMixingAnomaly.png', width = 4, height = 1.5, dpi = 500)  


