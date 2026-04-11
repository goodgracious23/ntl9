# Figure 4 - Littoral Zone
library(tidyverse)

# Panel A - Sparkling Lake data from Lodge et al. 1989 ==========
lodge = read_csv("C:/Users/grace/OneDrive/Desktop/lodge data.csv")

ggplot(lodge, aes(x = gw_velocity, y = biomass_depth_residual)) + 
  geom_point(color = "#023047", size = 3, alpha = 0.7) +
  theme_bw() +
  scale_x_continuous(transform = "log10") +
  geom_smooth(method = "lm", color = "black") + 
  xlab("Groundwater Velocity (um s-1)") + 
  ylab("Residuals of Biomass ~ Depth") 

summary(lm(lodge$biomass_depth_residual ~ log10(lodge$gw_velocity)))

# Panel B - lake level and ice depth ============================
# Package ID: knb-lter-ntl.30.29 Cataloging System:https://pasta.edirepository.org.
# Data set title: North Temperate Lakes LTER: Lake Levels 1981 - current.
options(HTTPUserAgent="EDI_CodeGen")
inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-ntl/30/29/9d2f8f3b9587e17d44cab87795d38cb2" 
infile1 <- tempfile()
try(download.file(inUrl1,infile1,method="curl",extra=paste0(' -A "',getOption("HTTPUserAgent"),'"')))
if (is.na(file.size(infile1))) download.file(inUrl1,infile1,method="auto")

all_lakelevel <-read.csv(infile1,header=F,skip=1,sep=",",quot='"' 
                     , col.names=c("lakeid", "year4","daynum",     
                                   "sampledate","sta", "llevel_elevation"    ), check.names=TRUE)
unlink(infile1)

# Package ID: knb-lter-ntl.34.34 Cataloging System:https://pasta.edirepository.org.
options(HTTPUserAgent="EDI_CodeGen")
inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-ntl/34/34/9be297624fc843fbd41f29b161150946" 
infile1 <- tempfile()
try(download.file(inUrl1,infile1,method="curl",extra=paste0(' -A "',getOption("HTTPUserAgent"),'"')))
if (is.na(file.size(infile1))) download.file(inUrl1,infile1,method="auto")

all_ice <-read.csv(infile1,header=F,skip=1,sep=",",quot='"', col.names=c("lakeid","year4", "daynum","sampledate","sta","nsnow","avsnow","sdsnow","wlevel","totice","nice","whiteice","blueice"), check.names=TRUE)
unlink(infile1)


lake_level = all_lakelevel %>%         
  mutate(sampledate = ymd(sampledate),
         month = month(sampledate)) %>%
  filter(lakeid == "AL",
         month > 10 | month < 4) %>% 
  group_by(year4) %>%
  summarize(mean_llevel = mean(llevel_elevation, na.rm = TRUE)) %>%
  ungroup()
# Store the mean to calc the anomaly
mean_lakelevel = mean(lake_level$mean_llevel)

black_ice = all_ice %>%
  filter(lakeid == "AL") %>%
  group_by(year4) %>%
  summarize(mean_blackice = mean(totice, na.rm = TRUE),
            max_blackice = max(totice, na.rm = TRUE)) %>%
  ungroup()
# Store the mean to calc the anomaly
mean_maxblackice = mean(black_ice$max_blackice)


ggplot(lake_level, aes(x = year4, y = mean_llevel - mean_lakelevel)) + 
  geom_col(fill = "#1D6996", alpha = 0.6) +
  theme_minimal() + 
  ylab("Winter Lake Level Anomalay") + xlab("") + 
  # plot black ice anomaly (53.7 = avg of the data record), convert to meters
  geom_line(data = black_ice, aes(x = year4, y = (max_blackice - mean_maxblackice)/100), 
            color = "#94346E", linewidth = 0.8) + 
  geom_point(data = black_ice, aes(x = year4, y = (max_blackice - mean_maxblackice)/100), 
             color ="#94346E", size = 2) +
  scale_y_continuous(name = "Lake Level Anomaly",
                     sec.axis = sec_axis(~ ., name = "Max Ice Depth Anomaly")) +
  theme(axis.title.y = element_text(color = "#1D6996"),
        axis.title.y.right = element_text(color = "#94346E")) +
  geom_hline(yintercept = 0, linetype = "dashed")

