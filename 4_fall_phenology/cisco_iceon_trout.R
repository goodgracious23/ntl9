library(tidyverse)
library(cowplot)

####### Get fall mixing plot for later join 
source('4_fall_phenology/fallMixingAnomaly.R')
########

# Package ID: knb-lter-ntl.32.31 Cataloging System:https://pasta.edirepository.org.
# Data set title: North Temperate Lakes LTER: Ice Duration - Trout Lake Area 1981 - current.
inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-ntl/32/31/22dfd43d5e56e7d828c13317c7c7a9d1" 
infile1 <- tempfile()
try(download.file(inUrl1,infile1,method="curl",extra=paste0(' -A "',getOption("HTTPUserAgent"),'"')))
if (is.na(file.size(infile1))) download.file(inUrl1,infile1,method="auto")

ice <- read_csv(infile1)

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
  #complete within each gear × year
  complete(lakeid, year4, gearid, spname = species_keep,
           fill = list(total_caught = 0, effort = 0)) %>%
  group_by(lakeid, year4, spname) %>% 
  summarise(total_caught = sum(total_caught), effort = max(effort)) %>%
  group_by(lakeid, year4) %>% 
  mutate(effort = max(effort), cpue = total_caught/effort) %>% 
  mutate(species = tolower(spname)) %>% 
  filter(lakeid == "TR") %>%
  ungroup()


ice_tr = ice %>% filter(lakeid == "TR") %>% rename(year4 = year) |> 
  select(lakeid, year4, ice_on, ice_off) |> 
  mutate(doy_iceon = yday(ice_on),
         doy_iceon = case_when(doy_iceon < 100 ~ doy_iceon + 365, TRUE ~ doy_iceon),
         doy_iceoff = yday(ice_off))

fish_ice = left_join(fish_clean, ice_tr, by = "year4") %>%
  group_by(spname) |> 
  mutate(iceon_lag2 = lag(doy_iceon, 2), 
         iceoff_lag2 = lag(doy_iceoff, 1)) |> 
  mutate(ice_lag2_date = as.Date(iceon_lag2 - 1, origin = paste0(2024, "-01-01")))


p.fishice = ggplot(fish_ice |> filter(spname %in% c('CISCO','LAKEWHITEFISH')), 
       aes(x = ice_lag2_date, 
           y = cpue, fill = species)) + 
  geom_point(size = 1.5, shape = 21, stroke = 0.2) + 
  facet_wrap(~spname, scales = 'free_y', ncol = 1) +
  scale_fill_manual(
    values = c( "laketrout" = "#1b7837", "lakewhitefish" = "#80cdc1", "cisco" = "#2166ac"),
    labels = c("laketrout" = "lake trout", "lakewhitefish" = "lake whitefish", "cisco" = "cisco")) +
  theme_bw(base_size = 9) +
  xlab("Ice On Date, Two Years Prior") + ylab("CPUE") + 
  theme(axis.title = element_text(size = 8), 
        legend.position = c(0.85, 0.9),
        legend.title = element_blank(),
        strip.text = element_blank(), 
        legend.key.size = unit(9, "pt"),
        legend.margin = margin(0.5, 3, 0.5, 0.5),
        # legend.justification = c(0, 1),    # anchor top-left of legend box
        legend.background = element_rect(fill = "white", color = "black", linewidth = 0.2)); p.fishice


# Join with cowplot 
plot_grid(
  p.fall, p.fishice,
  labels = c("a)", "b)"), label_size = 8, label_fontface = "plain",
  label_x = 0, label_y = 0.97,
  hjust = -0.2, vjust = 1.2, ncol = 2, axis = "tblr")

ggsave(filename = '4_fall_phenology/fallMixing_fish_3.png', width = 6.5, height = 1.4, dpi = 500, bg = 'white')  

