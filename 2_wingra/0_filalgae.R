library(tidyverse)
library(ggpattern)
library(sf)

# Download data from EDI
# North Temperate Lakes LTER: Macrophyte Biomass - Madison Lakes Area 1995 - current
inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-ntl/24/32/a03d18be68db4cf2280846afe2643d5e" 

infile1 <- tempfile()
try(download.file(inUrl1,infile1,method="curl",extra=paste0(' -A "',getOption("HTTPUserAgent"),'"')))
if (is.na(file.size(infile1))) download.file(inUrl1,infile1,method="auto")

macrophyte_ntl <- read_csv(infile1) |> filter(lakeid == "WI") %>% 
  mutate(fil_algae_wt = if_else(year4 == 2008, fil_algae_wt * 10, fil_algae_wt)) %>% 
  mutate(plant_wt_hand = if_else(year4 == 2008, plant_wt_hand * 10, plant_wt_hand))

#Filamentous algae
# fil_algae_timeseries <- macrophyte_ntl %>%
#   mutate(fil_algae_wt = replace_na(fil_algae_wt, 0)) %>%
#   filter(transect %in% c(2,5,7,9,11)) |> 
#   group_by(year4) %>%
#   summarize(fil_algae_mean = mean(fil_algae_wt, na.rm = T), 
#             plant_wt_mean = mean(plant_wt_hand, na.rm = T)) %>%
#   mutate(removal = ifelse(year4 < 2008, '< 2008', '≥ 2008'))

# Read in shapefile 
bathy = st_read('2_wingra/maps/Wingra_bathymetry_2025.shp') %>% 
  mutate(area = st_area(.)) %>% 
  group_by(Depth_m) %>% 
  summarise(area = sum(area)) %>% 
  st_drop_geometry() %>% 
  arrange(area) %>% 
  filter(Depth_m %in% c(0,1,2,3)) %>% 
  mutate(area_diff = c(area[1], diff(area))) %>% 
  mutate(area_per = as.numeric(area/sum(area))) %>% 
  rename(depth = Depth_m) 

# Create all possible combinations
transect <- c(2, 5, 7, 9, 11)
depth <- c(1, 1.5, 2, 2.5, 3, 3.5, 4)
year4 = c(1995:2019, 2022:2024)
rep = 1:4
combinations <- expand.grid(year4 = year4, transect = transect, depth = depth, rep = rep)


# Convert data to area depth
benthic_spatial = macrophyte_ntl %>% 
  full_join(combinations) %>%
  mutate(fil_algae_wt = replace_na(fil_algae_wt, 0)) %>%
  mutate(plant_wt_hand = replace_na(plant_wt_hand, 0)) %>%
  filter(transect %in% c(2,5,7,9,11)) %>% 
  mutate(depth = floor(depth)) %>%
  group_by(year4, depth) %>% 
  summarise(
    across(where(~ is.numeric(.) | inherits(., "Date")), 
      ~ mean(., na.rm = TRUE))) %>% 
  left_join(bathy) %>% 
  mutate(fil_algae_spatial = fil_algae_wt*area_per, plant_wt_spatial = plant_wt_hand*area_per) %>% 
  mutate(depth = factor(depth, levels = c(4,3.5,3,2.5,2,1.5,1,0.5,0)))

benthic_spatial_year = benthic_spatial %>% 
  group_by(year4) %>%
  summarise(fil_algae_spatial = sum(fil_algae_spatial, na.rm = T), 
            plant_wt_spatial = sum(plant_wt_spatial, na.rm = T)) %>% 
  mutate(removal = ifelse(year4 < 2008, '< 2008', '≥ 2008'))

# Check plot  
p1 = ggplot(benthic_spatial) +
  geom_col(aes(x = year4, y = fil_algae_wt, fill = as.factor(depth)))

p2 = ggplot(benthic_spatial, aes(x = year4, y = plant_wt_hand,
                                 fill = as.factor(depth))) +
  geom_col()

p1/p2 + plot_layout(guides = 'collect')

# Stats for paper 
benthic_spatial |> filter(year4 > 2008) |> 
  summarise(min(fil_algae_wt), max(fil_algae_wt), sd(fil_algae_wt),
            min(plant_wt_hand), max(plant_wt_hand), sd(plant_wt_hand))

