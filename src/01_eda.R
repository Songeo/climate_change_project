

# LIBS ----
library(tidyverse)
library(ggplot2)
theme_set(theme_bw())

# DATA ----
data_panel_final <- read_csv("data/processed/panel_data_final.csv")


# EDA ---- 

# frecuencias -----
data_panel_final |> head()

# no missing values
(data_panel_final |> nrow()) - (data_panel_final |> na.omit() |> nrow())

# summary of variables # no nas
data_panel_final |> summary()


# treatment ----
data_panel_final |> 
  group_by(treatment) |> 
  count() |> 
  ungroup() |> 
  mutate(p = n/sum(n))

gg <- 
  data_panel_final |> 
  mutate_at(c("treatment", "year"), factor) |> 
  mutate(iso_initial = iso3 > "kg") |> 
  ggplot(aes(x = year, 
             y = iso3, 
             fill = treatment)) + 
  geom_tile(color = "white") + 
  scale_fill_manual(values = c("gray90", "#6987bb")) + 
  facet_wrap(~iso_initial, scales = "free_y") + 
  theme(axis.text.x = element_text(size = 7),
        strip.text = element_blank()) +
  labs(title = "Treatments by Year and ISO3",
       x = "Year",
       y = "Country",
       Fill = "Treatment") 
gg
ggsave(plot = gg, 
       filename = "results/figures/tmt_year.png", 
       width = 15, 
       height = 11)


# outcome ----

# no missing values
data_panel_final |> 
  group_by(is.na(outcome)) |> 
  count() |> 
  ungroup() |> 
  mutate(p = n/sum(n))

gg <- 
  data_panel_final |> 
  mutate(iso_initial = iso3 > "kg") |> 
  ggplot(aes(x = year, 
             y = iso3, 
             fill = log(outcome + 100)) ) + 
  geom_tile(color = "white") + 
  scale_fill_viridis_c() + 
  facet_wrap(~iso_initial, scales = "free_y") + 
  theme(axis.text.x = element_text(size = 7),
        strip.text = element_blank(), 
        legend.position = "bottom") +
  labs(title = "Log Outcome by Year and ISO3",
       x = "Year",
       y = "Country") 
gg
ggsave(plot = gg, 
       filename = "results/figures/outcome_year.png", 
       width = 16, 
       height = 12)



