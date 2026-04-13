library(tidyverse)
library(here)

cyanos = read_csv(here("6_n_habs", "cyanos_mendota_2000_to_2021_relabundance.csv"))
taxa = read_csv(here("6_n_habs", "unique_cyano_taxa.csv"))

cyanos_combo = left_join(cyanos, taxa, by = "tax_abr") %>%
  filter(!year == 2019) %>%
  mutate(daynum = yday(date),
         diazotrophy = 
           case_when(diazotrophy == NA ~ "unknown",
                     diazotrophy == "yes" ~ "diazotrophic",
                     diazotrophy == "no" ~ "non-diazotrophic")) %>%
  group_by(date, diazotrophy) %>%
  summarize(abundance = sum(abund.rel.to.cyanos)) %>%
  ungroup() %>%
  mutate(year = year(date))

# Boxplot of diaz vs non-diaz by year
ggplot(cyanos_combo %>% filter(!diazotrophy == "unknown", !year == 2019), 
       aes(x = year, y = abundance, fill = diazotrophy, 
           group = interaction(year, diazotrophy))) + 
  geom_boxplot(stat = "boxplot", outliers = F) + 
  theme_bw() + 
  scale_fill_manual(values = c("#94346E", "#ffb703"), name = "")

annual_cyanos_combo = cyanos_combo %>%
  group_by(year, diazotrophy) %>%
  summarize(mean_annual_abundance = mean(abundance, na.rm = TRUE)) %>%
  ungroup()

ggplot(annual_cyanos_combo %>% 
         filter(!diazotrophy == "unknown", !year == 2019), 
       aes(x = year, y = mean_annual_abundance, color = diazotrophy)) + 
  geom_point(size = 3) + geom_line(linewidth = 1) + 
  theme_bw() + 
  scale_color_manual(values = c("#94346E", "#ffb703"), name = "")
