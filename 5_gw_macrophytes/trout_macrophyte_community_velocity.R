library(tidyverse)
library(vegan)
library(ggpubr)

# Package ID: knb-lter-ntl.25.26 Cataloging System:https://pasta.edirepository.org.
# Data set title: North Temperate Lakes LTER: Macrophyte Biomass in Trout Lake Summary 1983 - current
options(HTTPUserAgent="EDI_CodeGen")
inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-ntl/25/26/a984d0bfc36497b8cc1b3f4692412b5d" 
infile1 <- tempfile()
try(download.file(inUrl1,infile1,method="curl",extra=paste0(' -A "',getOption("HTTPUserAgent"),'"')))
if (is.na(file.size(infile1))) download.file(inUrl1,infile1,method="auto")
macrophytes <-read.csv(infile1,header=F,skip=1,sep=",",quot='"', col.names=c("lakeid","year4","site","depth","species_name","nreps","avg_dryweight_g", "stdev_dryweight_g"), check.names=TRUE)
unlink(infile1)

macrophyte = macrophytes %>%
  mutate(species_name = 
           case_when(species_name == "SAGITTARIA SP." ~ "SAGITTARIA SP",
                     species_name == "P. AMPLIFOLIUS" ~ "POTAMOGETON AMPLIFOLIUS",
                     species_name == "P. ROBBINSII" ~  "POTAMOGETON ROBBINSII",
                     species_name == "MYRIO. ALT." ~ "MYRIOPHYLLUM ALTERNIFLORUM",
                     species_name == "MYRIO. VERT." ~ "MYRIOPHYLLUM VERTICILLATUM",
                     species_name == "MYRIO.TENELLUM" ~ "MYRIOPHYLLUM TENELLUM",
                     species_name == "P. ZOSTERIFORMIS" ~ "POTAMOGETON ZOSTERIFORMIS",
                     species_name ==  "MYRIO. SIB." ~ "MYRIOPHYLLUM SIBIRICUM",
                     species_name == "VAL." ~ "VALLISNERIA AMERICANA",
                     TRUE ~ species_name)) %>%
  separate(species_name, into = c("genus", "species"), sep = " ") %>%
  filter(!genus == "LOBELIA", !genus == "JUNCUS",
         !genus == "SAJ.", !genus == "NITELLA",
         !genus == "SPARG", !genus == "STUCKENIA PECTINATA") %>%
  group_by(year4, depth, genus) %>%
  summarize(biomass = sum(avg_dryweight_g, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(biomass = case_when(biomass < 0 ~ 0, TRUE ~ biomass)) %>%
  pivot_wider(names_from = genus, values_from = biomass, values_fill = 0)

# PCA of Macrophyte Communities ==================
meta <- macrophyte %>% select(year4, depth)
comm_mat <- macrophyte %>% select(-year4, -depth)
comm_hel <- decostand(comm_mat, method = "hellinger", 
                      na.rm = TRUE, zap = TRUE)
pca <- rda(comm_hel)

scores_depths <- scores(pca, display = "sites") %>%
  as.data.frame() %>%
  bind_cols(meta)

scores_species <- scores(pca, display = "species") %>%
  as.data.frame() %>%
  tibble::rownames_to_column("genus")

# Compute velocity (step-wise change)
traj <- scores_depths %>%
  arrange(depth, year4) %>%
  group_by(depth) %>%
  mutate(dPC1 = PC1 - lag(PC1),
         dPC2 = PC2 - lag(PC2),
         dt = year4 - lag(year4),
         step_length = sqrt(dPC1^2 + dPC2^2),
         velocity = step_length / dt)

traj_summary <- traj %>%
  group_by(depth) %>%
  summarize(trajectory_length = sum(step_length, na.rm = TRUE),
            mean_velocity = mean(velocity, na.rm = TRUE),
            max_velocity = max(velocity, na.rm = TRUE),
            .groups = "drop")

# Compute net displacement as a measure of directional change vs wandering
net_change <- scores_depths %>%
  arrange(depth, year4) %>%
  group_by(depth) %>%
  summarize(net_displacement = sqrt(
    (last(PC1) - first(PC1))^2 + (last(PC2) - first(PC2))^2), 
    .groups = "drop")

# Compute directionality (1 = straight directional shift, 0 = oscillations/variability without directional shift)
traj_summary <- traj_summary %>%
  left_join(net_change, by = "depth") %>%
  mutate(directionality = net_displacement / trajectory_length)


# Lake Level Data =====================
options(HTTPUserAgent="EDI_CodeGen")
inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-ntl/30/29/9d2f8f3b9587e17d44cab87795d38cb2" 
infile1 <- tempfile()
try(download.file(inUrl1,infile1,method="curl",extra=paste0(' -A "',getOption("HTTPUserAgent"),'"')))
if (is.na(file.size(infile1))) download.file(inUrl1,infile1,method="auto")
lakelevel <-read.csv(infile1,header=F,skip=1,sep=",",quot='"' 
                     , col.names=c("lakeid", "year4","daynum",
"sampledate","sta", "llevel_elevation"), check.names=TRUE)
unlink(infile1)

TR_lakelevel = lakelevel %>%
  mutate(sampledate = ymd(sampledate),
         month = month(sampledate)) %>%
  filter(lakeid == "TR",
         month < 10 | month > 4) %>%
  group_by(year4) %>%
  summarize(mean_llevel = mean(llevel_elevation, 
                               na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(dlakelevel = (mean_llevel-491.82) - lag(mean_llevel-491.82),
         dt = year4 - lag(year4))

traj_lakelevel = left_join(traj, TR_lakelevel, by = "year4")


# Package ID: knb-lter-ntl.1.63 Cataloging System:https://pasta.edirepository.org.
# Data set title: North Temperate Lakes LTER: Chemical Limnology of Primary Study Lakes: Nutrients, pH and Carbon 1981 - current.
options(HTTPUserAgent="EDI_CodeGen")
inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-ntl/1/63/0ff1fd13116d6097376e3745194cdc5f" 
infile1 <- tempfile()
try(download.file(inUrl1,infile1,method="curl",extra=paste0(' -A "',getOption("HTTPUserAgent"),'"')))
if (is.na(file.size(infile1))) download.file(inUrl1,infile1,method="auto")
chem <-read.csv(infile1,header=F 
               ,skip=1
               ,sep=","  
               ,quot='"' 
               , col.names=c(
                 "lakeid",     
                 "year4",     
                 "daynum",     
                 "sampledate",     
                 "depth",     
                 "rep",     
                 "sta",     
                 "event",     
                 "ph",     
                 "phair",     
                 "alk",     
                 "dic",     
                 "tic",     
                 "doc",     
                 "toc",     
                 "no3no2",     
                 "no2",     
                 "nh4",     
                 "totnf",     
                 "totnuf",     
                 "totpf",     
                 "totpuf",     
                 "drsif",     
                 "brsif",     
                 "brsiuf",     
                 "tpm",     
                 "totnuf_wslh",     
                 "no3no2_wslh",     
                 "nh4_wslh",     
                 "kjdl_n_wslh",     
                 "totpuf_wslh",     
                 "drp_wslh",     
                 "drsif_wslh",     
                 "flagdepth",     
                 "flagph",     
                 "flagphair",     
                 "flagalk",     
                 "flagdic",     
                 "flagtic",     
                 "flagdoc",     
                 "flagtoc",     
                 "flagno3no2",     
                 "flagno2",     
                 "flagnh4",     
                 "flagtotnf",     
                 "flagtotnuf",     
                 "flagtotpf",     
                 "flagtotpuf",     
                 "flagdrsif",     
                 "flagbrsif",     
                 "flagbrsiuf",     
                 "flagtpm",     
                 "flagtotnuf_wslh",     
                 "flagno3no2_wslh",     
                 "flagnh4_wslh",     
                 "flagkjdl_n_wslh",     
                 "flagtotpuf_wslh",     
                 "flagdrp_wslh",     
                 "flagdrsif_wslh"    ), check.names=TRUE)

unlink(infile1)

alk = chem %>% 
  filter(lakeid == "TR", !is.na(alk), depth <= 4) %>%
  select(year4, depth, sampledate, alk, flagalk) %>%
  mutate(sampledate = ymd(sampledate),
         month = month(sampledate)) %>%
  filter(month > 4 | month < 10,
         !flagalk == "K") %>%
  group_by(year4) %>%
  summarize(mean_alk = mean(alk, na.rm = TRUE)) %>%
  ungroup()

traj_lakelevel_alk = left_join(traj_lakelevel, alk, by = "year4") %>%
  filter(!year4 == 1983)

# Plot it up up up =================
trout_mac_pca = 
  ggplot(scores_depths, 
         aes(x = PC1, y = PC2,
             color = factor(depth), size = year4)) +
  geom_point(alpha = 0.3) +
  scale_color_manual(values = c("#ffb703", "#219ebc", "#023047")) +
  # geom_path(aes(group = depth), linewidth = 0.8, alpha = 0.5) +  
  theme_minimal() +
  labs(x = "PC1", y = "PC2", color = "Site") + 
  theme(legend.position = 'top')

trout_mac_velocity = 
  ggplot(traj_lakelevel_alk, 
         aes(x = year4, y = velocity, color = factor(depth))) +
  scale_color_manual(values = c("#ffb703", "#219ebc", "#023047")) +
  geom_point(size = 2, alpha = 0.7) +
  geom_line(linewidth = 1, alpha = 0.7) +
  theme_minimal() +
  labs(y = "Community velocity", color = "Depth")+ 
  theme(legend.position = 'top')

trout_mac_lakelevel =
  ggplot(traj_lakelevel_alk, 
       aes(x = mean_alk, y = velocity, 
           color = factor(depth))) + 
  geom_point(alpha = 0.75) +  
  geom_smooth(method = "lm", aes(fill = factor(depth))) +
  scale_color_manual(values = c("#ffb703", "#219ebc", "#023047")) +
  scale_fill_manual(values = c("#ffb703", "#219ebc", "#023047")) +
  theme_minimal() + 
  theme(legend.position = 'top')

# ggplot(traj_lakelevel, aes(x = year4, y = mean_llevel - 491.82)) + 
#   geom_point()

ggarrange(trout_mac_pca, 
          trout_mac_velocity, 
          trout_mac_lakelevel,
          nrow = 1)
