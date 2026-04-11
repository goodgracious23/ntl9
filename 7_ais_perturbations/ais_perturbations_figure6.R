library(tidyverse)

perturbs = read_csv("C:/Users/grace/Box/1 - NTL9 Proposal/ntl_perturbation_timeseries.csv") %>%
  mutate(areal_value = case_when(
           lake == "Trout" & perturbation == "Rusty" ~ value/1565.1,
           lake == "Trout" & perturbation == "Trout" ~ value/1565.1,
           lake == "Sparkling" & perturbation == "Rusty" ~ value/63.7,
           lake == "Sparkling" & perturbation == "Cisco" ~ value/63.7,
           lake == "Sparkling" & perturbation == "Smelt" ~ value/63.7,
           lake == "Crystal" & perturbation == "Cisco" ~ value/37.5,
           lake == "Crystal" & perturbation == "Smelt" ~ value/37.5,
           TRUE ~ value)) %>%
  group_by(perturbation) %>%
  mutate(value_scaled = (areal_value - min(areal_value, na.rm = TRUE)) /
           (max(areal_value, na.rm = TRUE) - min(areal_value, na.rm = TRUE))) %>%
  ungroup() %>%
  mutate(perturbation = factor(perturbation, levels = c("Spiny WF", "Smelt", "Cisco",
                                                        "Carp", "Rusty", "EWM", "Zebra M",
                                                        "Trout", "Predators" 
                                                        )),
         category = case_when(perturbation == "Spiny WF" ~ "Planktivore",
                              perturbation == "Smelt" ~ "Planktivore",
                              perturbation == "Cisco" ~ "Planktivore",
                              perturbation == "Carp" ~ "Nearshore",
                              perturbation == "Rusty" ~ "Nearshore",
                              perturbation == "EWM" ~ "Nearshore",
                              perturbation == "Zebra M" ~ "Nearshore",
                              perturbation == "Trout" ~ "Cascade",
                              perturbation == "Predators" ~ "Cascade"
                              ),
         category = factor(category, levels = c("Planktivore", "Nearshore", "Cascade"))) %>%
  mutate(row_id = paste(category, perturbation, lake, sep = "_")) %>%
  arrange(category, perturbation, lake) %>%
  mutate(row_id = factor(row_id, levels = unique(row_id)))

ggplot(perturbs, 
       aes(x = year, y = row_id, 
           size = value_scaled,
           color = perturbation, alpha = value_scaled, group = category)) + 
  # facet_wrap(~category) +
  geom_point(shape = 19) + 
  scale_color_manual(values = c("#0F8554", "#99cc33","#ccee66","#EDAD08","#E17C05", "#CC503E", "#94346E", "#1D6996", "#38A6A5")) +
  scale_alpha_continuous(range = c(0.5, 1)) +
  # Zero values (force gray)
  geom_point(data = perturbs %>% filter(areal_value == 0),
             aes(x = year, y = row_id, size = value_scaled),
             color = "gray90", shape = 19, alpha = 1) +
  theme_minimal() +
  theme(legend.position = 'bottom') + guides(size = "none", alpha = "none") +
  ggtitle("") +
  ylab("") + xlab("") +
  scale_y_discrete(labels = function(x) {
    sapply(strsplit(x, "_"), function(parts) parts[3])  # extract lake
  })
