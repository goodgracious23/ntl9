library(tidyverse)
library(viridis)
library(stringr)
library(MetBrewer)

## Physical
# North Temperate Lakes LTER: Physical Limnology of Primary Study Lakes 1981 – current
p1 = 129701*4
# North Temperate Lakes LTER: Physical Limnology of Lake Kegonsa and Lake Waubesa 1995 – current
p2 = 9268*13
# North Temperate Lakes LTER: Snow and Ice Depth 1982 – current
p3 = 958*5
# North Temperate Lakes LTER: Ice Duration – Trout Lake Area 1981 – current
p4 = 301*2
# North Temperate Lakes LTER: Ice Duration – Madison Lakes Area 1853 – current
p5 = 443*2
# North Temperate Lakes LTER: Lake Levels 1981 – current
p6 = 3688*1
# North Temperate Lakes LTER: Light Extinction 1981 – current
p7 = 5661*1
# North Temperate Lakes LTER: Secchi Disk Depth; Other Auxiliary Base Crew Sample Data 1981 – current
p8 = 8777*8
# North Temperate Lakes LTER: Color – Trout Lake Area 1989 – current
p9 = 705334*1
# North Temperate Lakes LTER: Sediment Deposition – Trout Lake Area 1986 – current
p10 = 3699*8
# Lake Mendota Multiparameter Sonde Profiles: 2017 – current
p11 = 2118*9
p.all = p1 + p2 + p3 + p4 + p5 + p6 + p7 + p8 + p9 + p10 + p11

## Groundwater
# North Temperate Lakes LTER: Groundwater Chemistry 1984 – current
g1 = 431*19
# North Temperate Lakes LTER: Groundwater Levels 1984 – current
g2 = 6445*1
g.all = g1+ g2


##Chemical
# North Temperate Lakes LTER: Chemical Limnology of Primary Study Lakes: Major Ions 1981 – current
c1 = 11540*9
# North Temperate Lakes LTER: Chemical Limnology of Primary Study Lakes: Nutrients, pH and Carbon 1981 – current
c2 = 19874*24
# North Temperate Lakes LTER: Physical and Chemical Limnology of Lake Kegonsa and Lake Waubesa 1994 – current
c3 = 9268*13
c.all = c1 + c2 + c3
  
##Biological
# North Temperate Lakes LTER: Chlorophyll – Madison Lakes Area 1995 – current
b1 = 5917 * 4
# North Temperate Lakes LTER: Chlorophyll – Trout Lake Area 1981 – current
b2 = 33953 * 2
# North Temperate Lakes LTER: Phytoplankton – Madison Lakes Area 1995 – current
b3 = 18654 * 4
# North Temperate Lakes LTER: Phytoplankton – Trout Lake Area 1984 – current
b4 = 2552 * 4
# North Temperate Lakes LTER: Zooplankton – Trout Lake Area 1982 – current
b5 = 49467 * 3
# North Temperate Lakes LTER: Zooplankton – Madison Lakes Area 1997 – current
b6 = 9603 * 3
b.all = b1 + b2 + b3 + b4 + b5 + b6 

##Macroinvertebrates
# North Temperate Lakes LTER: Benthic Macroinvertebrates 1981 – current
m1 = 24700 * 2
# North Temperate Lakes LTER: Crayfish Abundance 1981 – current
m2 = 475 * 2
# North Temperate Lakes LTER Pelagic Macroinvertebrate Abundance 1983 – current
m3 = 6934 * 2
# North Temperate Lakes LTER: Pelagic Macroinvertebrate Summary 1983 – current
m4 = 1651 * 2
m.all = m1 + m2 + m3 + m4

##Macrophytes
# North Temperate Lakes LTER: Macrophyte Transects – Trout Lake 1982 – current
mp1 = 16534 * 1
# North Temperate Lakes LTER: Macrophyte Biomass – Trout Lake 1983 – current
mp2 = 7940 * 2
# North Temperate Lakes LTER: Macrophyte Richness – Trout Lake 1993 – current
mp3 = 17173 * 1
# North Temperate Lakes LTER: Macrophyte Biomass in Trout Lake Summary 1983 – current
mp4 = 1588 * 2
# North Temperate Lakes LTER: Macrophyte Species at Quadrat Level – Trout Lake 1993 – current
mp5 = 17737 * 1
# North Temperate Lakes LTER: Macrophyte Rating – Madison Lakes Area 1995 – current
mp6 = 54561 * 2
# North Temperate Lakes LTER: Macrophyte Biomass – Madison Lakes Area 1995 – current
mp7 = 23774 * 3
mp.all = mp1 + mp2 + mp3 + mp4 + mp5 + mp6 + mp7

##Fish
# North Temperate Lakes LTER: Fish Abundance 1981 – current
f1 = 14982 * 3
# Lake Wingra: Fish Abundance 1995 – current
f2 = 1366 * 3
# Lake Wingra: Fish Lengths and Weights 1995 – current
f3 = 46350 * 3
# North Temperate Lakes LTER: Fish Species Richness 1981 – current
f4 = 432 * 1
# North Temperate Lakes LTER: Fish Length Frequency 1981 – current
f5 = 25261 * 1
# North Temperate Lakes LTER: Fish Lengths and Weights 1981 – current
f6 = 363698 * 3
# North Temperate Lakes LTER: Pelagic Prey – Sonar Data 2001 – current
f7 = 192 * 5
f.all = f1 + f2 + f3 + f4 + f5 + f6 + f7

# Uses hardcode values
ntl <- tibble(
  Category = c("Physical", "Chemical", "Biological",
               "Macroinvertebrates", "Macrophytes", "Fish", "Groundwater", "High-frequency"),
  
  DatasetCount = c(10, 6, 8, 5, 6, 4, 3, 11),
  Entries = c(p.all, c.all, b.all, m.all, mp.all, f.all, g.all, NA),
  
  StartYear = c(1981, 1981, 1981, 1981, 1982, 1981, 1984, 2003),
  EndYear   = c(2024, 2024, 2024, 2024, 2024, 2024, 2024, 2025),
  
  Topics = c(
    "ice, light, Secchi, temp, oxygen, color, lake level",
    "ions, nutrients, carbon",
    "plankton, chlorophyll",
    "benthic, pelagic, crayfish",
    "richness, biomass",
    "richness, species",
    "level, chemistry",
    "temperature, oxygen, light, fluorometry, metabolism"
  )) |> 
  mutate(Category = factor(Category, levels =  c("Physical", "Chemical", "Biological",
                                                 "Macroinvertebrates", "Macrophytes", "Fish", 
                                                 "Groundwater", "High-frequency")))
  # 
  # mutate(Category = factor(Category, levels = c("Groundwater", "Fish", "Macrophytes",
  #                                               "Macroinvertebrates", "Biological",
  #                                               "Chemical", "Physical")))
# Compute depth and wrap text for plotting
ntl <- ntl %>%
  mutate(DepthYears = EndYear - StartYear + 1,
    TopicsWrapped = str_wrap(Topics, width = 58))
    # Category = fct_reorder(Category, DatasetCount)

# Plot
ggplot(ntl, aes(Category, DatasetCount, fill = Entries)) +
  geom_col(width = 0.75, color = "black") +
  # Topic labels inside bars
  geom_text(aes(y = 0.2, label = TopicsWrapped),
    hjust = 0, vjust = 0.5, color = "white", size = 2, fontface = "bold") +
  coord_flip() +
  scale_fill_met_c('Homer2') +
  labs(title = "NTL Core Datasets", x = "", y = "Number of Datasets") +
  theme_minimal(base_size = 8) +
  theme(panel.grid.major.y = element_blank(),
    legend.position = "right")

# ggsave('ntlData.png', width = 4, height = 2, dpi = 500)

# Take 2 
ggplot(ntl, aes(Category, DatasetCount, fill = Entries)) +
  geom_col(width = 0.75, color = "black") +
  # topic labels inside bars
  geom_text(aes(y = 0.2, label = TopicsWrapped),
            hjust = 0, vjust = 0.5,
            color = "white", size = 2, fontface = "bold") +
  # entries labels to the right
  geom_text(aes(y = DatasetCount + 0.35,
                label = scales::comma(Entries)),
            hjust = 0, size = 2.5) +
  # coord_flip(clip = "off") +
  scale_y_continuous(breaks = c(0,2,4,6,8,10)) +
  scale_fill_met_c("Homer2") +
  labs(title = "NTL Core Datasets", x = "", y = "Number of Datasets") +
  theme_minimal(base_size = 8) +
  theme(panel.grid.major.y = element_blank(),
    legend.position = "none",
    plot.margin = margin(5.5, 40, 5.5, 5.5)) # space for labels)
# ggsave('ntlData2.png', width = 4, height = 2, dpi = 500)

# Take 3
ggplot(ntl, aes(Category, DatasetCount, fill = Entries)) +
  geom_col(width = 0.75, color = "black") +
  # topic labels inside bars
  geom_text(aes(y = 0.1, label = TopicsWrapped),
            angle = 90,
            hjust = 0, vjust = 0.5,
            color = "white", size = 2) + #fontface = "bold"
  # entries above bars
  geom_text(aes(y = DatasetCount + 1.07,
                label = scales::comma(Entries)),
            vjust = 0.6, size = 2.3, angle = 65) +
  scale_fill_met_c("Homer2") +
  scale_y_continuous(position = "right", limits = c(0,11.5), breaks = c(0,2,4,6,8,10)) +
  labs(y = "Number of Core Datasets") +
  theme_minimal(base_size = 8) +
  theme(axis.title.x = element_blank(),
    panel.grid.major.x = element_blank(),
    legend.position = "none",
    # axis.text.x = element_text(angle =  315, hjust = 0, vjust = -1),
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1.2),
    plot.margin = margin(10, 10, 25, 10))
ggsave('1_datasets/ntlData3.png', width = 2, height = 3.6, dpi = 700)

