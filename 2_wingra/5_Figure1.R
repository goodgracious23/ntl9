
library(patchwork)
library(scales)

################### Timeseries ###################
theme_timeseries <- function() {
  list(
    scale_x_date(
      limits = as.Date(c("1995-01-01", "2025-01-01")),
      breaks = seq(as.Date("1995-01-01"), as.Date("2025-01-01"), by = "5 years"),
      labels = date_format("%Y")
    ),
    xlab(""),
    scale_fill_manual(values = c("#f0c44d", "#406691")),
    theme_minimal(base_size = 9),
    theme(legend.position = "none", 
          panel.grid.major = element_line(color = "grey70", linewidth = 0.3),
          panel.grid.minor = element_blank(),
          axis.title.x = element_blank())
  )
}


secchi_timeseries = ggplot() +
  geom_point(data = secchi, aes(x = sampledate, y = secnview, fill = removal), size = 1.4, 
             shape = 21, alpha = 0.8, stroke = 0.3) +
  geom_line(data = summary_means, aes(x = as.Date(paste0(year4, "-07-01")), y = mean_secchi), 
            linewidth = 0.5) +
  ylab("Secchi\ndepth (m)") +
  theme_timeseries() +
  geom_vline(aes(xintercept = as.Date('2008-01-15')), linewidth = 0.3, linetype = 2)

tp_timeseries <- ggplot() +
  geom_point(data = tp,aes(x = sampledate, y = totpuf, fill = removal), size = 1.1, shape = 21, alpha = 0.5) +
  geom_line(data = summary_means, aes(x = as.Date(paste0(year4, "-07-01")), y = mean_totpuf), linewidth = 1) +
  ylab(bquote(atop("Total phosphorus", "(" * mu * "g " * L^{-1} * ")"))) +
  theme_timeseries() +
  geom_vline(aes(xintercept = as.Date('2008-01-15')), linewidth = 0.3, linetype = 2)


macro_timeseries <- ggplot(macrophyte_timeseries) +
  geom_point(data = secchi.may, aes(x = year4, y = secnview)) + 
  geom_line(data = secchi.may, aes(x = year4, y = secnview)) + 
  geom_point(aes(x = Year, y = `Maximum depth of plants (ft)`/3.281, fill = removal), size = 1.3, shape = 21) +
  geom_line(aes(x = Year, y = `Maximum depth of plants (ft)`/3.281)) +
  geom_rect(aes(xmin = 1995, xmax = 2004.5, ymin = 2.59, ymax = 4), fill = "grey") +
  annotate('text', x = 2000, y = 2.7, label = 'not sampled', angle = 90, col = 'grey30', hjust = 0, size = 2) +
  ylab("Macrophyte\ncolonization \nmax depth (m)") +
  scale_fill_manual(values = c( "white", "black")) +
  scale_x_continuous(breaks = seq(1995,2020, by = 5), limits = c(1995,2025)) +
  theme_bw(base_size = 9) + 
  theme(legend.position = "none", 
        axis.title.x = element_blank()) +
  geom_vline(aes(xintercept = 2008), linewidth = 0.3, linetype = 2); macro_timeseries

bethic_plant_timeseries = ggplot(benthic_spatial |> filter(depth == 3)) +
  geom_col(aes(x = year4, y = plant_wt_spatial), fill = '#6c8e67') + 
  geom_col(data = data.frame(x = c(2020,2021), y = 135),
            aes(x = x, y = y), fill = "grey") +
  # annotate('text', x = 2020.5, y = 1, label = 'not sampled', angle = 90, col = 'grey30', hjust = 0, size = 2) +
  # annotate('text', x = 2021, y = 1, label = 'not sampled', angle = 90, col = 'grey30', hjust = 0, size = 2) +
  # scale_fill_manual(values = c('#08c248', '#248c48', '#295e3b', '#203d2a'), name = 'Depth (m)') +
  ylab("Macrophyte\nwet mass (g)") +
  scale_x_continuous(breaks = seq(1995,2025, by = 5), limits = c(1995,2025.1)) +
  theme_minimal(base_size = 9) + 
  theme(legend.position = 'none', 
        panel.grid.major = element_line(color = "grey70", linewidth = 0.3),
        panel.grid.minor = element_blank(),
        axis.title.x = element_blank()) +
  geom_vline(aes(xintercept = 2008), linewidth = 0.3, linetype = 2); bethic_plant_timeseries

bethic_algae_timeseries = ggplot(benthic_spatial) +
  geom_col(aes(x = year4, y = fil_algae_spatial), fill = '#43a364') + 
  geom_col( data = data.frame(x = c(2020,2021), y = max(benthic_spatial_year$fil_algae_spatial)),
            aes(x = x, y = y), fill = "grey") +
  annotate('text', x = 2020, y = 1, label = 'not sampled', angle = 90, col = 'grey30', hjust = 0, size = 2) +
  annotate('text', x = 2021, y = 1, label = 'not sampled', angle = 90, col = 'grey30', hjust = 0, size = 2) +
  # scale_fill_manual(values = c('#08c248', '#248c48', '#295e3b', '#203d2a'), name = 'Depth (m)') +
  ylab("Filamentous\nalgae mass (g)") +
  scale_x_continuous(breaks = seq(1995,2020, by = 5), limits = c(1995,2025)) +
  theme_bw(base_size = 9) + 
  theme(legend.position = 'bottom', 
        axis.title.x = element_blank(), 
        legend.key.size = unit(0.3,'cm')) +
  geom_vline(aes(xintercept = 2008), linewidth = 0.3, linetype = 2); bethic_algae_timeseries


################### Boxplots ###################
theme_boxplot <- function() {
  list(
    scale_x_discrete(labels = c(expression("< 2008"), expression(phantom(x) >= 2008))),
    theme_bw(base_size = 9),
    scale_fill_manual(values = c( "white", "black")),
    theme(legend.position = "none",
      panel.border = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.title = element_blank(),
      axis.line = element_line(colour = "black")))
}

secchi_boxplot <- ggplot(summary_means, aes(x = removal, y = mean_secchi)) +
  geom_boxplot(outlier.color = NA) +
  geom_jitter(aes(fill = removal), shape = 21, alpha = 0.5) +
  annotate("text", x=1, y=Inf, vjust = 2, label= "p < 0.01", size = 2) +
  theme_boxplot()

tp_boxplot <- ggplot(summary_means, aes(x = removal, y = mean_totpuf)) +
  geom_boxplot(outlier.color = NA) +
  geom_jitter(aes(fill = removal), shape = 21, alpha = 0.5) +
  annotate("text", x=2, y=Inf, vjust = 1, label= "p < 0.01", size = 2) +
  theme_boxplot()

benthic_plant_box <- ggplot(benthic_spatial_year, aes(x = removal, y = plant_wt_spatial)) +
  geom_boxplot(outlier.color = NA) +
  geom_jitter(aes(fill = removal), shape = 21, alpha = 0.5) +
  annotate("text", x=1, y=Inf, vjust = 2, label= "p < 0.01", size = 2) +
  theme_boxplot()

benthic_algae_box <- ggplot(benthic_spatial_year, aes(x = removal, y = fil_algae_spatial)) +
  geom_boxplot(outlier.color = NA) +
  geom_jitter(aes(fill = removal), shape = 21, alpha = 0.5) +
  annotate("text", x=1, y=Inf, vjust = 2, label= "p < 0.01", size = 2) +
  theme_boxplot()


# Combine using design layout
## Cowplot
# plot_grid(secchi_timeseries, bethic_plant_timeseries,precip_tp
#           ncol = 2, rel_widths = c(1,1,1),
#           labels = c("a)", "b)", "c)"), label_size = 8, label_fontface = "plain",
#           align = "v",      # align vertically
#           axis = "lr"       # align left/right axes
# )

secchi_timeseries
ggsave("2_wingra/Figure_secchi_timeseries.png", width = 2.5, height = 0.8, units = 'in', dpi = 1000)
ggsave("2_wingra/Figure_secchi_timeseries.pdf", width = 2.5, height = 0.8, units = 'in', dpi = 1000)


left <- plot_grid(secchi_timeseries, bethic_plant_timeseries, ncol = 1, 
                  labels = c("a)", "b)", "c)"), label_y = c(1.0, 1.3),  # move "b" higher
                  rel_heights = c(1.2,1),
                  label_size = 8, label_fontface = "plain")   # stack 1 over 2

ggsave("2_wingra/Figure_timeseries.png", width = 2.5, height = 1.6, units = 'in', dpi = 500)


plot_grid(
  left, precip_tp,
  ncol = 2, labels = c("", "c)"), label_size = 8, label_fontface = "plain",
  rel_widths = c(1, 1)  # adjust if needed
)


ggsave("2_wingra/Figure2.png", width = 4.5, height = 1.8, units = 'in', dpi = 500, bg = 'transparent')

