library(tidyverse)
library(here)
library(cowplot)
################## Source tributary plot ##################
source('6_n_habs/Fig_a_getUSGS_N.R')
################## Source NO# plot ##################
source('6_n_habs/Fig_b__NO3decline.R')


cyanos = read_csv(here("6_n_habs", "cyanos_mendota_2000_to_2021_relabundance.csv"))
taxa = read_csv(here("6_n_habs", "unique_cyano_taxa.csv"))

cyanos_combo = left_join(cyanos, taxa, by = "tax_abr") %>%
  filter(!year == 2019) %>%
  mutate(daynum = yday(date),
         diazotrophy = 
           case_when(diazatroph == NA ~ "unknown",
                     diazatroph == "yes" ~ "diazotrophic",
                     diazatroph == "no" ~ "non-diazotrophic")) %>%
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

p.diazo = ggplot(annual_cyanos_combo %>% 
         filter(!diazotrophy == "unknown", !year == 2019), 
  aes(x = year, y = mean_annual_abundance, fill = diazotrophy)) + 
  geom_line(linewidth = 0.3) + 
  geom_point(size = 2, shape = 21) + 
  scale_fill_manual(values = c("#94346E", "#ffb703"), name = "") +
  ylab('Relative Abundance') +
  theme_bw(base_size = 9) + 
  theme(
    axis.title.x = element_blank(),
    legend.title =  element_blank(),
    legend.key.size = unit(12, "pt"),
    legend.margin = margin(0.5, 1, 0.5, 0.5),
    legend.position = c(1,1),   # x, y (0–1 coordinates)
    legend.justification = c(1, 1),    # anchor top-left of legend box
    legend.background = element_rect(fill = "white", color = "black", linewidth = 0.2)
  ); p.diazo

## Cowplot
plot_grid(p.yahara, p.no3, p.diazo,
          ncol = 3, rel_widths = c(1,1,1),
          labels = c("a)", "b)", "c)"), label_size = 8, label_fontface = "plain",
          align = "v",      # align vertically
          axis = "lr"       # align left/right axes
)

ggsave(filename = '6_n_habs/2_Nfig_v2.png', width = 6.5, height = 1.8, dpi = 500, bg = 'white')  

