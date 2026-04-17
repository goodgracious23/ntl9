library(dataRetrieval)
library(sf)
library(tidyverse)

# Yahara at 113 annual discharge
N.y113 <- read_waterdata_daily(
  monitoring_location_id = "USGS-05427850", 
  parameter_code = c("00060")) |> 
  mutate(year = year(time)) |> 
  # mutate( water_year = if_else(month(time) >= 10, year(time) + 1, year(time))) |> 
  group_by(year) |> 
  summarise(annualQ = sum(value * 0.0283168 * 86400))
mean(N.y113$annualQ)

p <- read_waterdata_monitoring_location("USGS-05427718")
codes = read_waterdata_combined_meta(monitoring_location_id = "USGS-05427718")
codes |> dplyr::select(parameter_code, parameter_name) |> arrange(parameter_code) |> distinct()

N.yw <- read_waterdata_daily(
  monitoring_location_id = "USGS-05427718", 
  parameter_code = c("00060", "00631", "00625"))  |> 
  select(gage = monitoring_location_id, parameter_code, time, value) |> 
  pivot_wider(names_from = parameter_code, values_from = value) |> 
  st_drop_geometry() |> 
  mutate(flow_m3s = `00060` * 0.0283168) |> 
  rename(no3 = `00631`, NH3_orgN = `00625`, date = time)

N.pb <- read_waterdata_daily(
  monitoring_location_id = "USGS-05427948", 
  parameter_code = c("00060", "00631", "00625")) |> 
  select(gage = monitoring_location_id, parameter_code, time, value) |> 
  pivot_wider(names_from = parameter_code, values_from = value) |> 
  st_drop_geometry() |> 
  mutate(flow_m3s = `00060` * 0.0283168) |> 
  rename(no3 = `00631`, NH3_orgN = `00625`, date = time)

N.2 <- N.pb %>% bind_rows(N.yw) |> 
  mutate(year = year(date))

valid_years <- N.2 %>%
  group_by(gage, year) %>%
  summarise(n_days = sum(!is.na(no3)), .groups = "drop") %>%
  filter(n_days > 300)

N.2 = N.2 |> semi_join(valid_years, by = c("gage", "year")) |> 
    # water_year = if_else(month(date) >= 10, year(date) + 1, year(date)),  # Oct–Sep water year
  mutate(
    load_no3_kg_d = flow_m3s * no3 * 86400 * 1e-3,
    load_no4_kg_d = flow_m3s * NH3_orgN * 86400 * 1e-3) %>%
  group_by(gage, year) %>%
  filter(sum(!is.na(no3)) > 0) %>%
  arrange(date) %>%
  group_by(gage, year) %>%
  mutate(
    cumQ = cumsum(flow_m3s * 86400),   # cumulative volume (m3)
    no3_Load = cumsum(load_no3_kg_d),
    nh4_orgN_Load = cumsum(load_no4_kg_d)) %>%
  ungroup()

# write_csv(N.2, 'NitrogenFig/usgsTribN.csv')

ggplot(N.2, aes(x = cumQ, y = nh4_orgN_Load, color = gage)) +
  geom_path(alpha = 0.8, linewidth = 1) +
  labs(
    x = "Cumulative Discharge (m³)",
    y = "Cumulative NO3 Load (kg)",
    color = "Water Year") +
  theme_minimal() +
  facet_wrap(~gage, scales = 'free')

N.2_yearly <- N.2 %>%
  group_by(gage, year) %>%
  summarise(total_Q = max(cumQ, na.rm = TRUE),
    NO3 = max(no3_Load, na.rm = TRUE),
    NH4_orgN = max(nh4_orgN_Load, na.rm = TRUE),
    .groups = "drop") |> 
  pivot_longer(cols = 4:5) |> 
  mutate(gage_name = recode(
      gage,
      "USGS-05427718" = "Trib: Yahara @ Windsor",
      "USGS-05427948" = "Trib: Pheasant Branch"))

# Windsor discharge is 4.5x than PB
N.2_yearly |> group_by(gage) |> 
  summarise(mean(total_Q))
6546304 # PB
30048053 # Windsor
73963745 # 113

p.tribs = ggplot(N.2_yearly) +
  geom_point(aes(x = total_Q/1e6, y = value/1e4, fill = name, shape = name), size = 2, alpha = 0.8) +
  facet_wrap(~ gage_name, scales = "free") +
  labs(x = expression(Annual~Discharge~(10^6*~m^3)),
    y = expression(Load~(10^4*~kg))) +
  scale_shape_manual(values = c(23,22), name = '') +
  scale_fill_manual(values = c('#7f1147', '#c98c3d'), name = '') +
  scale_x_continuous(n.breaks = 3) +
  theme_bw(base_size = 9) +
  theme(
    # panel.border = element_blank(),
    strip.background = element_blank(),
    strip.text = element_text(color = "black")); p.tribs

p.tribs.l = p.tribs +
  theme(
    legend.title =  element_blank(),
    legend.key.size = unit(12, "pt"),
    legend.margin = margin(0.5, 3, 0.5, 0.5),
    legend.position = c(0,1),   # x, y (0–1 coordinates)
    legend.justification = c(0, 1),    # anchor top-left of legend box
    legend.background = element_rect(fill = "white", color = "black", linewidth = 0.2)
  ); p.tribs.l


p.yahara = ggplot(N.2_yearly |> filter(gage == 'USGS-05427718')) +
  geom_point(aes(x = total_Q/1e6, y = value/1e4, fill = name, shape = name), size = 2, alpha = 0.8) +
  labs(x = expression(Annual~Q~(10^6*~m^3)),
       y = expression(Load~(10^4*~kg))) +
  scale_shape_manual(values = c(23,22), name = '') +
  scale_fill_manual(values = c('#7f1147', '#c98c3d'), name = '') +
  scale_x_continuous(n.breaks = 4) +
  ylim(0,35) +
  theme_bw(base_size = 9) +
  theme(
    # panel.border = element_blank(),
    legend.key.spacing = unit(0.04, "cm"),
    strip.background = element_blank(),
    strip.text = element_text(color = "black"),
    legend.title =  element_blank(),
    legend.key.size = unit(12, "pt"),
    legend.margin = margin(0.5, 3, 0.5, 0.5),
    legend.position = c(0,1),   # x, y (0–1 coordinates)
    legend.justification = c(0, 1),    # anchor top-left of legend box
    legend.background = element_rect(fill = "white", color = "black", linewidth = 0.2)
  ); p.yahara

#7f1147
#c98c3d

# N.2_yearly <- N.2_yearly %>%
#   group_by(gage) %>%
#   mutate(
#     Q_rel = total_Q / max(total_Q),
#     load_rel = total_load / max(total_load)
#   ) %>%
#   ungroup()
# 
# ggplot(N.2_yearly, aes(Q_rel, load_rel, color = gage)) +
#   geom_point(size = 2) +
#   geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
#   theme_bw(base_size = 9) +
#   labs(y = 'Normalized annual nitrate load', x = 'Normalized annual discharge', 
#        caption = 'Water-year cumulative nitrate load (load_rel) versus cumulative discharge (Q_rel), 
#        normalized to the maximum annual values within each gage. Each point represents a single water 
#        year, allowing comparison of export efficiency across years and sites.')

