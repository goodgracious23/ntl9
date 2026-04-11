library(tidyverse)

# Package ID: knb-lter-ntl.32.31 Cataloging System:https://pasta.edirepository.org.
# Data set title: North Temperate Lakes LTER: Ice Duration - Trout Lake Area 1981 - current.
options(HTTPUserAgent="EDI_CodeGen")
inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-ntl/32/31/22dfd43d5e56e7d828c13317c7c7a9d1" 
infile1 <- tempfile()
try(download.file(inUrl1,infile1,method="curl",extra=paste0(' -A "',getOption("HTTPUserAgent"),'"')))
if (is.na(file.size(infile1))) download.file(inUrl1,infile1,method="auto")

ice <-read.csv(infile1,header=F 
               ,skip=1
               ,sep=","  
               ,quot='"' 
               , col.names=c(
                 "lakeid",     
                 "year",     
                 "lastopen",     
                 "ice_on",     
                 "lastice",     
                 "ice_off",     
                 "duration",     
                 "comments"    ), check.names=TRUE)
unlink(infile1)


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
  mutate(panel = if_else(species %in% c("laketrout", "lakewhitefish"),
                         "lake trout & lake whitefish", "cisco"),
         panel = factor(panel, levels = c("lake trout & lake whitefish", "cisco"))) %>%
  filter(lakeid == "TR", spname == "CISCO") %>%
  ungroup()


ice_tr = ice %>% filter(lakeid == "TR") %>% rename(year4 = year)

fish_ice = left_join(fish_clean, ice_tr, by = "year4") %>%
  select(year4, cpue, ice_on) %>%
  mutate(ice_on = ymd(ice_on), 
         doy_iceon = yday(ice_on),
         doy_iceon = case_when(doy_iceon < 100 ~ doy_iceon + 365, TRUE ~ doy_iceon),
         iceon_lag2 = lag(doy_iceon, 2))


ggplot(fish_ice, 
       aes(x = iceon_lag2, 
           y = cpue, 
           color = year4)) + 
  geom_point(size = 3) + 
  scale_color_viridis_c(option = "rainbow", name = "Year") +
  # scale_y_continuous(transform = 'log10') +
  # geom_path() +
  # geom_smooth(method = "lm") +
  theme_bw() + 
  xlab("Ice On Date, Two Years Prior") + ylab("Catch Per Unit Effort") + 
  ggtitle("Cisco, Trout Lake") + 
  theme(legend.position = c(0.88, 0.8))
