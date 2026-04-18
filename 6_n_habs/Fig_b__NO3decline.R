library(MetBrewer)
library(scales)
library(tidyverse)
library(patchwork)
library(cowplot)
################## Source tributary plot ##################
# source('6_n_habs/1_getUSGS_N.R')

################## load MEMO aphan data ##################
aphan = read_csv('6_n_habs/aphanizomenon_APHAN134_first_bloom.csv')

################## load NTL nutrient data ##################
nuts = loadLTERnutrients() |> filter(lakeid == 'ME', depth == 0) |> 
  filter(!is.na(no3no2_WSLH)) |> 
  filter(yday(sampledate) >= 100 & yday(sampledate) <= 365) |> 
  select(sampledate, depth, no3no2_WSLH, nh4_WSLH, totnuf_WSLH)

# Day of first 0 nitrate in surface water of Mendota 
zerono3 = nuts %>%
  mutate(year = year(sampledate)) %>%
  group_by(year) %>%
  filter(no3no2_WSLH == 0) %>%
  slice_min(sampledate, n = 1, with_ties = FALSE) %>%
  ungroup()

p.no3 = ggplot(zerono3) +
  geom_path(aes(x = year, y = yday(sampledate)), color = '#c98c3d') +
  geom_point(aes(x = year, y = yday(sampledate)), shape = 21, fill = '#c98c3d', size = 2) +
  geom_path(data = aphan, aes(x = year, y = yday(date)),color = '#10adad') +
  geom_point(data = aphan, aes(x = year, y = yday(date)), shape = 21, fill = '#10adad', size = 2) +
  annotate("text", x = 2016, y = Inf,
           label = "date~NO[3]~'hits zero'",
           parse = TRUE, vjust = 2, size = 2.5, color = '#c98c3d') +
  annotate("text", x = 2017, y = Inf,
           label = "Aphanizomenon\nfirst appears",
           vjust = 5.6, size = 2.5, color = '#10adad') +
  scale_y_continuous(
    labels = function(x) format(as.Date(x, origin = "2025-01-01"), "%b")) +
  labs(y = 'Month') +
  theme_bw(base_size = 9) +
  theme(axis.title.x = element_blank()); p.no3

## Cowplot
plot_grid(p.tribs.l, p.no3,
  ncol = 2, rel_widths = c(1.5,1),
  labels = c("a)", "b)"), label_size = 8, label_fontface = "plain",
  align = "v",      # align vertically
  axis = "lr"       # align left/right axes
)
# ggsave(filename = '6_n_habs/Nfig_v1.png', width = 6.5, height = 1.8, dpi = 500, bg = 'white')  


zeronh4 = nuts %>%
  mutate(year = year(sampledate)) %>%
  group_by(year) %>%
  # filter(month(sampledate) %in% c(4,5)) |> 
  filter(yday(sampledate) > 100) |>
  filter(yday(sampledate) < 150) |>
  summarise(nh4_WSLH = mean(nh4_WSLH, na.rm = T),
            no3no2_WSLH = mean(no3no2_WSLH, na.rm = T)) %>%
  # slice_min(sampledate, n = 1, with_ties = FALSE) %>%
  ungroup()

scale_factor <- 365 / 0.5
doy_min <- 122
doy_max <- 176

nh4_min <- 0
nh4_max <- 0.3
scale_nh4_to_doy <- function(x) {
  doy_min + (x - nh4_min) * (doy_max - doy_min) / (nh4_max - nh4_min)
}

p.nh4 = ggplot() +
  geom_path(data = zeronh4, aes(x = year, y = scale_nh4_to_doy(nh4_WSLH)),
            color = '#c98c3d', linewidth = 0.3) +
  geom_path(data = aphan, aes(x = year, y = yday(date)),
            color = '#10adad', linewidth = 0.3) +
  geom_point(data = zeronh4, aes(x = year, y = scale_nh4_to_doy(nh4_WSLH)),
             shape = 21, fill = '#c98c3d', size = 2) +
  geom_point(data = aphan, aes(x = year, y = yday(date)),
             shape = 21, fill = '#10adad', size = 2) +
  annotate("text", x = 1999, y = Inf,
           label = "spring~NH[4]",
           parse = TRUE, vjust = 12.5, size = 2.5, color = '#c98c3d') +
  annotate("text", x = 2025, y = Inf, hjust = 1, lineheight = 0.8,
           label = "Aphanizomenon\nfirst appears",
           vjust = 1.2, size = 2.5, color = '#10adad') +
  scale_y_continuous(
    name = "Month",
    breaks = yday(seq.Date(as.Date("2025-01-01"), as.Date("2025-12-01"),by = "1 month")),
    # limits = c(doy_min, doy_max),
    labels = function(x) format(as.Date(x, origin = "2025-01-01"), "%b"),
    sec.axis = sec_axis(
      ~ (.-doy_min) * (nh4_max - nh4_min) / (doy_max - doy_min) + nh4_min,
      name = expression(NH[4]~"(mg/L)"))) +
  theme_bw(base_size = 9) +
  theme(axis.title.x = element_blank()); p.nh4

# p.nh4 = ggplot(zeronh4) +
#   geom_path(data = zeronh4, aes(x = year, y = nh4_WSLH * scale_factor),
#             color = '#c98c3d') +
#   geom_point(data = zeronh4, aes(x = year, y = nh4_WSLH * scale_factor),
#              shape = 21, fill = '#c98c3d', size = 2) +
#   geom_path(data = aphan, aes(x = year, y = yday(date)),color = '#10adad') +
#   geom_point(data = aphan, aes(x = year, y = yday(date)), shape = 21, fill = '#10adad', size = 2) +
#   annotate("text", x = 1999, y = Inf,
#            label = "spring~NH[4]",
#            parse = TRUE, vjust = 12, size = 2.5, color = '#c98c3d') +
#   annotate("text", x = 2017, y = Inf,
#            label = "Aphanizomenon\nfirst appears",
#            vjust = 1.2, size = 2.5, color = '#10adad') +
#   scale_y_continuous(
#     name = "Day", 
#     labels = function(x) format(as.Date(x, origin = "2025-01-01"), "%b"),
#     sec.axis = sec_axis(
#       ~ . / scale_factor,
#       name = expression(NH[4]~"(mg/L)"))) +
#   # scale_y_continuous(
#   #   labels = function(x) format(as.Date(x, origin = "2025-01-01"), "%b")) +
#   labs(y = 'Month') +
#   theme_bw(base_size = 9) +
#   theme(axis.title.x = element_blank()); p.nh4

