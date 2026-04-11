taxa = read_csv("C:/Users/grace/Box/1 - NTL9 Proposal/unique_cyano_taxa.csv")
cyanos = read_csv("C:/Users/grace/Box/1 - NTL9 Proposal/cyanos_mendota_2000_to_2021_relabundance.csv")
tribs = read_csv("C:/Users/grace/Box/1 - NTL9 Proposal/MEtribs_annuals_long.csv") %>% filter(site == "yah") %>% pivot_wider(., names_from = load, values_from = value) %>% rename(year4 = year)

cyanos_better = left_join(cyanos, taxa, by = "tax_abr") %>%
  mutate(diazotrophy = case_when(diazatroph == "yes" ~ "diazotrophic",
                                 diazatroph == "no" ~ "non-diazotrophic",
                                 is.na(diazatroph) ~ "unknown")) %>%
  select(-diazatroph) %>%
  group_by(date, diazotrophy) %>%
  summarize(abundance = sum(abund.rel.to.cyanos)) %>%
  ungroup() %>%
  mutate(year = year(date),
         yday = yday(date))


ggplot(cyanos_better %>% 
         filter(!diazotrophy == "unknown", !year == 2019), 
       aes(x = year, y = abundance, 
           fill = diazotrophy, 
           # color = diazotrophy, 
           group = interaction(year, diazotrophy))) + 
  geom_boxplot(stat = 'boxplot', outliers = F) + 
  # geom_point(size = 3) + geom_line(linewidth = 1) + 
  theme_bw() + 
  scale_fill_manual(values = c("#94346E", "#ffb703"), name = "") + 
  # scale_color_manual(values = c("#ffb703", "#023047")) +
  theme(legend.position = c(0.2, 0.8))

cyano_year = cyanos_better %>%
  group_by(year, diazotrophy) %>%
  summarize(annual_abundance = mean(abundance)) %>%
  ungroup() %>%
  mutate(year4 = year)

cyano_annual = cyanos %>%
  group_by(date) %>%
  summarize(total_cyanos = sum(abund.rel.to.total, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(year4 = year(date)) %>%
  group_by(year4) %>%
  summarize(mean_cyanos = mean(total_cyanos, na.rm = TRUE)) %>%
  ungroup()


aphanizomenon = left_join(cyanos, taxa, by = "tax_abr") %>%
  filter(`remane` == "Aphanizomenon") %>%
  filter(abund.rel.to.cyanos >0) %>%
  mutate(daynum = yday(date)) %>%
  filter(daynum > 85) %>%
  group_by(year) %>%
  summarize(aphan_appears = min(daynum, na.rm = TRUE)) %>%
  ungroup() %>%
  rename(year4 = year)

cyano_don = left_join(cyano_year, nform, by = "year4") %>%
  # pivot_longer(., cols = mean_don:mean_nh4, 
  #              names_to = "N_form", 
  #              values_to = "concentration") %>%
  # mutate(N_form = case_when(N_form == "mean_don" ~ "DON",
  #                           N_form == "mean_no3" ~ "NO3",
  #                           N_form == "mean_nh4" ~ "NH4")) %>%
  left_join(., tribs, by = "year4") %>% 
  left_join(., aphanizomenon, by = "year4")

ggplot(cyano_don %>% filter(!diazotrophy == "unknown"), 
       aes(x = totalNO3, y = aphan_appears)) + 
  geom_point(size = 3) + 
  geom_smooth(
    method = 'lm') + 
  # scale_color_manual(values = c("#94346E", "#ffb703"), name = "") +
  theme_bw() + 
  xlab("Avg Springtime NH4") + ylab("Day Aphanizomenon First Appears")

ggplot(cyano_don %>% filter(!diazotrophy == "unknown"), 
       aes(x = totalNO3, y = mean_no3)) + 
  geom_point(size = 3) + 
  geom_smooth(
    method = 'lm') + 
  # scale_color_manual(values = c("#94346E", "#ffb703"), name = "") +
  theme_bw() + 
  xlab("Avg Springtime NH4") + ylab("Day Aphanizomenon First Appears")

diazotrophs = cyano_don %>% 
  filter(diazotrophy == "non-diazotrophic")
cor.test(diazotrophs$aphan_appears, 
         diazotrophs$mean_nh4, 
         method = "pearson")

ggplot(cyano_don %>% filter(!diazotrophy == "unknown",
                            N_form == "DON"), 
       aes(x = concentration, y = annual_abundance, 
           color = diazotrophy)) + 
  geom_point(size = 3) + 
  # facet_wrap(~N_form, scale = "free_x") +
  geom_smooth(
    data = cyano_don %>%
                filter(diazotrophy == "diazotrophic", 
                       N_form == "DON"),
              method = 'lm') + 
  scale_color_manual(values = c("#94346E", "#ffb703"), name = "") +
  theme_bw() + 
  xlab("Concentration of N (mg/L)") + 
  ylab("Mean Relative Abundance") + 
  theme(legend.position = 'top')
  # theme(legend.position = c(0.8, 0.2))
  # scale_y_continuous(transform = "log10")

diazotrophs = cyano_don %>% 
  filter(diazotrophy == "non-diazotrophic",
         N_form == "NO3")
cor.test(diazotrophs$annual_abundance, 
         diazotrophs$concentration, 
         method = "pearson")
  

ggplot(cyano_don %>% filter(!diazotrophy == "unknown"), 
       aes(y = mean_nh4, x = totalNH4/totalTN)) + 
  geom_point(size = 3) + 
  geom_smooth(
    method = 'lm') + 
  # scale_color_manual(values = c("#94346E", "#ffb703"), name = "") +
  theme_bw() + 
  xlab("NH4 Concentration") + ylab("Day of Year Aphanizomenon Appears") + 
  theme(legend.position = 'top')


ggplot(cyano_year %>% 
         filter(!diazotrophy == "unknown", !year == 2019), 
       aes(x = year, y = annual_abundance, 
           # fill = diazotrophy, 
           color = diazotrophy)) + 
  geom_point(size = 3) + geom_line(linewidth = 1) +
  theme_bw() + 
  scale_color_manual(values = c("#94346E", "#ffb703")) +
  theme(legend.position = c(0.7, 0.8)) + 
  ylab("Mean Relative Abundance") + xlab("")


ggplot(aphanizomenon, aes(x = year, y = aphan_appears)) + geom_point()
ggplot(aphanizomenon %>% 
         mutate(yday = yday(date)) %>%
         filter(!year == 2019, 
                yday >100, yday < 250), 
       aes(x = yday, y = abund.rel.to.cyanos, 
           color = factor(year))) + 
  geom_point() + facet_wrap(~year)
