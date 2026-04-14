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

precip_tp <- ggplot(summary_means) +
  geom_smooth(
    data = filter(get_sig_models(summary_means, x = arb.precip, y = mean_totpuf), sig_line),
    aes(x = arb.precip, y = mean_totpuf, group = removal),
    method = "lm",
    color = "black", size = 0.4
  ) +
  # geom_smooth(aes(x = arb.precip, y = mean_totpuf, col = removal), method = 'lm') +
  geom_point(aes(x = arb.precip, y = mean_totpuf, fill = removal), shape = 21, size = 2.5)+
  scale_fill_manual(values = c("#f0c44d", "#406691"))+
  scale_color_manual(values = c( "black", "black"), guide = "none")+
  ylab(expression(paste("TP", " (µg ", L^-1,")")))+
  xlab("Precipitation (mm)")+
  theme_bw(base_size = 9) +
  theme(
    legend.title =  element_blank(),
    legend.key.size = unit(12, "pt"),
    legend.margin = margin(0.5, 3, 0.5, 0.5),
    legend.position = c(1,0),   # x, y (0–1 coordinates)
    legend.justification = c(1, 0),    # anchor top-left of legend box
    legend.background = element_rect(fill = "white", color = "black", linewidth = 0.2)
  ); precip_tp

# ggsave("figures/Figure3.png", width = 6.5, height = 5, units = 'in', dpi = 500)
