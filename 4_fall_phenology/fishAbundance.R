library(tidyverse)
library(lubridate)
library(patchwork)
library(cowplot)

source('4_fall_phenology/fallMixingAnomaly.R')

# Package ID: knb-lter-ntl.7.46 Cataloging System:https://pasta.edirepository.org.
# Data set title: North Temperate Lakes LTER: Fish Abundance 1981 - current.
inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-ntl/7/46/a61e4e3d72606b70535d79d5c4f3474a" 
infile1 <- tempfile()
try(download.file(inUrl1,infile1,method="curl",extra=paste0(' -A "',getOption("HTTPUserAgent"),'"')))
if (is.na(file.size(infile1))) download.file(inUrl1,infile1,method="auto")

fish <- read_csv(infile1) |> 
  filter(lakeid == 'TR') |> 
  # keep only VGN gear
  filter(str_starts(gearid, "VGN"))

species_keep <- c("LAKETROUT", "CISCO", "LAKEWHITEFISH")

fish_clean <- fish %>%
  # keep only relevant columns (optional but cleaner)
  select(lakeid, year4, gearid, spname, effort, total_caught) %>%
  # restrict to species of interest
  filter(spname %in% species_keep) %>%
  # aggregate just in case duplicates exist
  # group_by(lakeid, year4, gearid, spname) %>%
  # summarise(
  #   total_caught = sum(total_caught, na.rm = TRUE),
  #   effort = sum(effort, na.rm = TRUE),
  #   .groups = "drop") %>%
  
  #complete within each gear × year
  complete(lakeid, year4, gearid, spname = species_keep,
    fill = list(total_caught = 0, effort = 0)) |>
  group_by(lakeid, year4, spname) |> 
  summarise(total_caught = sum(total_caught), effort = max(effort)) |> 
  group_by(lakeid, year4) |> 
  mutate(effort = max(effort), cpue = total_caught/effort) |> 
  mutate(species = tolower(spname)) |> 
  mutate(panel = if_else(species %in% c("laketrout", "lakewhitefish"),
                         "lake trout & lake whitefish", "cisco"),
         panel = factor(panel, levels = c("lake trout & lake whitefish", "cisco"))) 

##################### PLOTTING ########################
labels <- fish_clean %>%
  group_by(species) %>%
  filter(year4 == min(year4, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(label = recode(species, laketrout = "lake trout", 
                        lakewhitefish = "lake whitefish", cisco = "cisco")) |> 
  mutate(cpue = c(750, 11.4, 8.5), year4 = 1980)

p.fish = fish_clean %>%
  ggplot(aes(x = year4, y = cpue, color = species)) +
  geom_line(linewidth = 0.7) +
  geom_text(data = labels, aes(label = label),
    hjust = -0, size = 2.7, show.legend = FALSE) +
  facet_wrap(~panel, scales = "free_y", ncol = 1) +
  scale_color_manual(
    values = c( "laketrout" = "#1b7837", "lakewhitefish" = "#80cdc1", "cisco" = "#2166ac"),
    labels = c("laketrout" = "lake trout", "lakewhitefish" = "lake whitefish", "cisco" = "cisco")) +
  theme_bw(base_size = 9) +
  labs(y = "CPUE", x = NULL) +
  theme(axis.title = element_text(size = 9),
    legend.position = "none",   # remove legend
    strip.text = element_blank())

caption_text <- "Fig. X a) Interannual anomalies in fall mixing in NTL lakes, 
defined as the first fall sampling date when the vertical temperature difference 
was ≤2 °C; (right) fish time series from the NTL lakes. b) Interannual 
catch-per-unit-effort (CPUE) for lake trout, lake whitefish, and cisco in Trout 
Lake. CPUE was calculated as total 
fish caught divided by total effort for vertical gill net sampling."

p.fall + p.fish  +
  plot_annotation(tag_levels = "a", tag_suffix = ')',
                  # caption = str_wrap(caption_text, width = 100)  # wrap at ~80 characters
  ) &
  theme(plot.tag = element_text(size = 8),  plot.caption = element_text(size = 10, hjust = 0))

# ggsave(filename = 'fallMixing_fish.png', width = 6.5, height = 1.8, dpi = 500)  

# Join with cowplot instead ot decrease borders

plot_grid(
  p.fall, p.fish,
  labels = c("a)", "b)"), label_size = 8, label_fontface = "plain",
  label_x = 0, label_y = 1,
  hjust = -0.2, vjust = 1.2, ncol = 2, axis = "tblr")

ggsave(filename = '4_fall_phenology/fallMixing_fish_2.png', width = 6.5, height = 1.2, dpi = 500, bg = 'white')  
