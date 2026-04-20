library(tidyverse)
library(patchwork)

# get model significance
get_sig_models <- function(data, x, y, group_var = removal) {
  x_var <- as_label(enquo(x))
  y_var <- as_label(enquo(y))
  group_var <- enquo(group_var)
  
  data %>%
    group_by(!!group_var) %>%
    do({
      model <- lm(reformulate(x_var, y_var), data = .)
      tidy_model <- tidy(model)
      slope_p <- tidy_model %>%
        filter(term == x_var) %>%
        pull(p.value)
      if (length(slope_p) == 0) slope_p <- NA
      mutate(., sig_line = slope_p < 0.05, p = slope_p)
    }) %>%
    ungroup()
}

get_sig_models(summary_means, x = arb.precip, y = mean_secchi)

precip_tp <- ggplot(summary_means |> filter(year4 < 2008)) +
  # geom_smooth(
  #   data = filter(get_sig_models(summary_means, x = arb.precip, y = mean_totpuf), sig_line),
  #   aes(x = arb.precip, y = mean_totpuf, group = removal),
  #   method = "lm",
  #   color = "black", size = 0.4
  # ) +
  # geom_smooth(aes(x = arb.precip, y = mean_totpuf, col = removal), method = 'lm') +
  geom_point(aes(x = arb.precip, y = mean_totpuf, fill = removal), shape = 21, size = 1.4, stroke = 0.3)+
  scale_fill_manual(values = c("#9cbb3c", "#406691"))+
  scale_color_manual(values = c( "black", "black"), guide = "none")+
  scale_y_continuous(expand = expansion(mult = 0.1), breaks = c(30,50), limits = c(20,65)) +
  scale_x_continuous(breaks = c(700,1000), limits = c(300,1150)) +
  ylab(expression(paste("TP", " (µg ", L^-1,")")))+
  xlab("Precip (mm)")+
  theme_minimal(base_size = 8) +
  theme(
    panel.grid.major = element_line(color = "grey70", linewidth = 0.3),
    panel.grid.minor = element_blank(),
    axis.title = element_text(size = 7),
    axis.title.x = element_blank(),
    legend.title =  element_blank(),
    legend.key.size = unit(12, "pt"),
    legend.margin = margin(0.5, 3, 0.5, 0.5),
    legend.position = 'none',
    # legend.position = c(1,0),   # x, y (0–1 coordinates)
    legend.justification = c(1, 0),    # anchor top-left of legend box
    legend.background = element_rect(fill = "white", color = "black", linewidth = 0.2)
  ); precip_tp

# ggsave("2_wingra/Figure_rain.png", width = 1.8, height = 1.6, units = 'in', dpi = 500, bg = 'transparent')
ggsave("2_wingra/Figure_rain_v3a.png", width = 1.1, height = 0.9, units = 'in', dpi = 1000, bg = 'transparent')


precip_tp <- ggplot(summary_means |> filter(year4 >= 2008)) +
  geom_smooth(
    data = filter(get_sig_models(summary_means, x = arb.precip, y = mean_totpuf), sig_line),
    aes(x = arb.precip, y = mean_totpuf, group = removal),
    method = "lm",
    color = "black", size = 0.4
  ) +
  # geom_smooth(aes(x = arb.precip, y = mean_totpuf, col = removal), method = 'lm') +
  geom_point(aes(x = arb.precip, y = mean_totpuf, fill = removal), shape = 21, size = 1.4, stroke = 0.3)+
  scale_fill_manual(values = c("#406691"))+
  scale_color_manual(values = c( "black", "black"), guide = "none")+
  scale_y_continuous(expand = expansion(mult = 0.1), breaks = c(30,50), limits = c(20,65)) +
  scale_x_continuous(breaks = c(700,1000), limits = c(300,1150)) +
  ylab(expression(paste("TP", " (µg ", L^-1,")")))+
  xlab("Precip (mm)")+
  theme_minimal(base_size = 8) +
  theme(
    panel.grid.major = element_line(color = "grey70", linewidth = 0.3),
    panel.grid.minor = element_blank(),
    axis.title = element_text(size = 7),
    axis.title.x = element_blank(),
    legend.title =  element_blank(),
    legend.key.size = unit(12, "pt"),
    legend.margin = margin(0.5, 3, 0.5, 0.5),
    legend.position = 'none',
    # legend.position = c(1,0),   # x, y (0–1 coordinates)
    legend.justification = c(1, 0),    # anchor top-left of legend box
    legend.background = element_rect(fill = "white", color = "black", linewidth = 0.2)
  ); precip_tp

ggsave("2_wingra/Figure_rain_v3b.png", width = 1.1, height = 0.9, units = 'in', dpi = 1000, bg = 'transparent')

