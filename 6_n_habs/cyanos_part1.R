# Package ID: knb-lter-ntl.1.63 Cataloging System:https://pasta.edirepository.org.
# Data set title: North Temperate Lakes LTER: Chemical Limnology of Primary Study Lakes: Nutrients, pH and Carbon 1981 - current.
inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-ntl/1/63/0ff1fd13116d6097376e3745194cdc5f" 
infile1 <- tempfile()
try(download.file(inUrl1,infile1,method="curl",extra=paste0(' -A "',getOption("HTTPUserAgent"),'"')))
if (is.na(file.size(infile1))) download.file(inUrl1,infile1,method="auto")

chem <- read_csv(infile1) %>%
  filter(lakeid == "ME", depth <= 0) %>%
  group_by(sampledate) %>%
  summarize(mean_nh4 = mean(nh4_WSLH, na.rm = TRUE),
            mean_no3 = mean(no3no2_WSLH, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(year = year(sampledate),
         doy = yday(sampledate))

spring_n = chem %>%
  filter(doy > 50, doy <140) %>%
  group_by(year) %>%
  summarize(spring_nh4 = mean(mean_nh4, na.rm = TRUE),
            spring_no3 = median(mean_no3, na.rm = TRUE)) %>%
  ungroup()

aphan = read_csv("C:/Users/grace/Box/1 - NTL9 Proposal/aphanizomenon_APHAN134_first_bloom.csv") %>%
  rename(mcmahon_appears = doy)

taxa = read_csv("C:/Users/grace/Box/1 - NTL9 Proposal/unique_cyano_taxa.csv")
cyanos = read_csv("C:/Users/grace/Box/1 - NTL9 Proposal/cyanos_mendota_2000_to_2021_relabundance.csv")

aphanizomenon = left_join(cyanos, taxa, by = "tax_abr") %>%
  filter(`remane` == "Aphanizomenon") %>%
  filter(abund.rel.to.cyanos >0) %>%
  mutate(daynum = yday(date)) %>%
  filter(daynum > 50) %>%
  group_by(year) %>%
  summarize(aphan_appears = min(daynum, na.rm = TRUE)) %>%
  ungroup() 

aphan_springN = left_join(spring_n, aphan, by = "year") %>%
  left_join(., aphanizomenon, by = "year")

ggplot(aphan_springN %>% filter(!is.na(date)), aes(x = spring_nh4, y = aphan_appears, color = year)) + 
  geom_point(size = 3) + 
  # ylim(120, 160) +
  # scale_x_continuous(transform = "log10") +
  geom_smooth(method = "lm") +
  theme_bw()

data = aphan_springN %>% filter(!is.na(date), !mcmahon_appears > 160)
summary(lm(mcmahon_appears ~ spring_nh4, data = data))


don = dt1 %>%
  filter(lakeid == "ME", !is.na(totnf), !is.na(no3no2_wslh),
         !is.na(nh4_wslh), depth == 0) %>%
  mutate(don = (totnf/1000) - no3no2_wslh - nh4_wslh,
         sampledate = date(sampledate)) %>%
  filter(year4 > 2000, !don < 0, !don > 2,
         daynum > 80, daynum < 200, depth == 0) %>%
  group_by(year4) %>%
  summarize(mean_don = median(don, na.rm = TRUE)) %>%
  ungroup()

nh4 = dt1 %>%
  filter(lakeid == "ME", !is.na(nh4_wslh), depth == 0) %>%
  mutate(sampledate = date(sampledate)) %>%
  filter(year4 >= 2000, daynum > 70, daynum < 190) %>%
  group_by(year4) %>%
  summarize(mean_nh4 = mean(nh4_wslh, na.rm = TRUE)) %>%
  ungroup()

no3 = dt1 %>%
  filter(lakeid == "ME",  depth == 0) %>%
  mutate(sampledate = date(sampledate)) %>%
  filter(year4 >= 2000, daynum > 70, daynum < 160) %>%
  group_by(year4) %>%
  summarize(mean_no3 = mean(no3no2_wslh, na.rm = TRUE)) %>%
  ungroup()

nform = left_join(don, nh4, by = "year4") %>%
  left_join(., no3, by = "year4")
