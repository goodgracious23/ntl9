library(tidyverse)
# devtools::install_github('hdugan/NTLlakeloads')
library(NTLlakeloads)
library(patchwork)
library(broom)

# Filter to data from June through August
usemonths = c(6,7,8)

#################### Secchi ####################
secchi = loadLTERsecchi() |> filter(lakeid == 'WI') |> 
  dplyr::select(sampledate, year4, secnview) |>
  filter(month(sampledate) %in% usemonths) |>
  mutate(removal = ifelse(year(sampledate) < 2008, "<2008", ">=2008"))

secchi |> group_by(year4) |> filter(n() <= 2)

secchi.may = loadLTERsecchi() |> filter(lakeid == 'WI') |> 
  dplyr::select(sampledate, year4, secnview) |>
  filter(month(sampledate) %in% 5) %>% 
  group_by(year4) %>% 
  summarise(secnview = max(secnview, na.rm = T)) %>% 
  mutate(removal = ifelse(year4 < 2008, "<2008", ">=2008"))

#################### Nutrients ####################
nuts = loadLTERnutrients() |> filter(lakeid == 'WI')


# Total unfiltered phosphorus 
tp <- nuts |> 
  filter(month(sampledate) %in% usemonths) |> 
  filter(depth == 0) |> 
  filter(is.na(flagtotpuf_WSLH) | !str_detect(flagtotpuf_WSLH, "[AKLHU]")) |> 
  filter(is.na(flagtotpuf) | !str_detect(flagtotpuf, "[AKLHU]")) |> 
  dplyr::select(sampledate, year4, lakeid, totpuf_WSLH, totpuf) |> 
  mutate(totpuf_WSLH = totpuf_WSLH * 1000) |> 
  pivot_longer(cols = c(totpuf_WSLH, totpuf)) |> 
  filter(value < 400) %>%
  group_by(sampledate) |>
  summarise(totpuf = mean(value, na.rm = T)) |> 
  mutate(year4 = year(sampledate))|>
  mutate(removal = ifelse(year(sampledate) < 2008, "<2008", ">=2008"))
  # filter(year4 != 2020)

tp |> group_by(year4) |> filter(n() <= 2)
ggplot(tp) +
  geom_point(aes(x = sampledate, y = totpuf))


#################### DNR Macrophyte ####################
macrophyte_dnr <- read_csv("2_wingra/dnr_macrophyte_sum.csv")

macrophyte_timeseries<- macrophyte_dnr|>mutate(removal = ifelse(Year < 2008, "<2008", ">=2008"))

colonization <- macrophyte_dnr %>%
  dplyr::select(Year, `Maximum depth of plants (ft)`) %>%
  rename(colonization_ft = `Maximum depth of plants (ft)`) %>%
  rename(year4 = Year) %>%
  mutate(colonization_m = colonization_ft*0.3048)

#################### Filamentous algae ####################
source('2_wingra/0_filalgae.R')

#################### Precipitation ####################
# Get arboretum .csv from github repository
arb.precip = read_csv('https://raw.githubusercontent.com/hdugan/Wingra_SaltTrajectory/refs/heads/main/data_input/Climate/3944435.csv') |> 
  mutate(year4 = year(DATE))

arb.spring <- arb.precip |>
  filter(NAME == 'UW ARBORETUM MADISON, WI US') |> 
  filter(year4 >= 1995) |> 
  filter(month(DATE) %in% c(1,2,3,4,5,6,7,8)) |> 
  group_by(year4, NAME) |>
  summarise(arb.precip = sum(PRCP, na.rm = T), num_na = sum(is.na(PRCP))) |> 
  dplyr::select(year4, arb.precip)


#################### Summary means ####################

secchi_mean = secchi |> 
  group_by(year4) |> 
  summarise(mean_secchi = mean(secnview, na.rm = TRUE))

tp_mean = tp |> 
  group_by(year4) |> 
  summarise(mean_totpuf = mean(totpuf, na.rm = TRUE))


summary_means <- secchi_mean %>%
  left_join(tp_mean, by = "year4") |>
  left_join(colonization, by = "year4")|>
  left_join(benthic_spatial_year |> dplyr::select(year4, plant_wt_spatial), by = "year4")|>
  left_join(benthic_spatial_year |> dplyr::select(year4, fil_algae_spatial), by = "year4")|>
  left_join(arb.spring, by = "year4") |> 
  mutate(across(-year4, ~ ifelse(year4 == 2020, NA, .))) |> # Remove 2020 (lack of data)
  mutate(removal = ifelse(year4 < 2008, "<2008", ">=2008"))

# For Table S1 
# Identify the years in the dataset
all_years <- sort(unique(summary_means$year4))
# Select numeric columns (or all columns you want to check)
cols_to_check <- summary_means %>% dplyr::select(-year4) %>% names()

# Function to get start, end, and missing years
year_summary <- function(col_name) {
  df <- summary_means %>%
    dplyr::select(year4, all_of(col_name)) %>%
    filter(!is.na(.data[[col_name]]))
  
  present_years <- sort(unique(df$year4))
  missing_years <- setdiff(all_years, present_years)
  
  tibble(
    column = col_name,
    start_year = min(present_years, na.rm = TRUE),
    end_year = max(present_years, na.rm = TRUE),
    missing_years = paste(missing_years, collapse = ", ")
  )
}

# Apply function to each column
year_summaries <- map_dfr(cols_to_check, year_summary)

print(year_summaries)

