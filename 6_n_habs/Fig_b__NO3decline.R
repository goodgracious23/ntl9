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

zeronh4 = nuts %>%
  mutate(year = year(sampledate)) %>%
  group_by(year) %>%
  filter(nh4_WSLH == 0) %>%
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

