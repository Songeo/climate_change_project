

# LIBS ----
library(tidyverse)
library(ggplot2)
theme_set(theme_bw())

# DATA ----

data_panel <- read_csv("data/processed/panel_data.csv")
data_panel_final <- read_csv("data/processed/panel_data_final.csv")


# EDA ---- 

# frecuencias -----
data_panel_final |> head()
(data_panel_final |> nrow()) - (data_panel_final |> na.omit() |> nrow())
data_panel_final |> summary()





# faltantes ----
tab <- 
  data_panel_final |> 
  pivot_longer(cols = population:renewable_pct) |> 
  mutate(nas = is.na(value)) |> 
  group_by(iso3, name) |> 
  reframe(sum_nas = sum(nas)) 

gg <- 
  ggplot(tab, 
       aes(x = iso3, y = name, fill = sum_nas)
       ) +
  geom_tile(color = "white") + 
  scale_fill_viridis_c() + 
  theme(axis.text.x = element_text(size = 5, 
                                   angle = 90), 
        legend.position = 'bottom') 

gg
ggsave(plot = gg, 
       filename = "results/figures/nas_outcome.png", 
       width = 9, 
       height = 5)


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

# treatment ----
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




# covariates
covars_long <-
  data_panel_final %>%
  pivot_longer(cols = population:renewable_pct, 
               names_to = "variable",
               values_to = "value") |> 
  group_by(iso3, variable) |> 
  mutate(sum_nas = sum(is.na(value)))

covars_long  |> 
  mutate(missing_values = sum_nas > 0) |> 
  ggplot(aes(x = year,
             y = value, 
             group = iso3, 
             color = iso3)) +
  geom_line(size = 1, alpha = .5) +
  facet_grid(variable ~ missing_values, 
             scales = "free") +
  labs(title = "Trends of Various Variables by Year and ISO3",
       x = "Year",
       y = "Value") +
  theme(axis.text.x = element_text(size = 7),
        strip.text = element_text(size = 10), 
        legend.position = "None")

sapply(unique(covars_long$variable), 
       function(var){
  gg <- covars_long  |> 
    filter(variable == var) |> 
    mutate(iso_initial = iso3 > "kg") |> 
    ggplot(aes(x = year,
               y = iso3, 
               fill = log(value + 100) )) +
    geom_tile() + 
    facet_wrap(~iso_initial, scales = "free") + 
    scale_fill_gradient(low = "#9eb1cf", 
                        high = "#5083a0", 
                        na.value = "white") + 
    labs(title = glue::glue("Missing values in variable {var}"),
         x = "Country",
         y = "Year") +
    theme(axis.text.x = element_text(size = 7),
          strip.text = element_blank(), 
          legend.position = "Bottom")
  
  var_lower <- str_squish(tolower(var))
  ggsave(plot = gg, 
         filename = glue::glue("results/figures/covariate_year_{var_lower}.png"), 
         width = 16, 
         height = 12)
  
})




# missing data
gg <- data_panel_final |> 
  select(iso3:year, carbon_dioxide:nitrous_oxide) |> 
  pivot_longer(carbon_dioxide:nitrous_oxide, 
               names_to = "gas_type", 
               values_to = "value") |> 
  filter(!is.na(value)) |> 
  mutate(no_nas = (!is.na(value))) |> 
  ggplot(aes(x = year, y = iso3, fill = no_nas) ) + 
  geom_tile(color = "white") + 
  facet_wrap(~gas_type, ncol = 1) + 
  theme(axis.text.x = element_text(size = 5, 
                                   angle = 90), 
        legend.position = "None") + 
  coord_flip()
gg
ggsave(plot = gg, filename = "results/figures/nas_outcome.png", width = )

data_panel_final |> 
  filter(year >= 1990) |> 
  ggplot(aes(x = year, y = iso3, fill = treatment) ) + 
  geom_tile(color = "white") + 
  facet_wrap(~tax_type, nrow = 1) + 
  theme(axis.text.y = element_text(size = 4), 
        axis.text.x = element_text(angle = 90))

# pollution metrics summary
data_panel_final |> 
  select(iso3:year, carbon_dioxide:nitrous_oxide) |> 
  pivot_longer(carbon_dioxide:nitrous_oxide, 
               names_to = "gas_type", 
               values_to = "value") |> 
  filter(!is.na(value)) |> 
  group_by(gas_type) |> 
  mutate(estand = (value - mean(value))/sd(value)) |> 
  ggplot(aes(x = year, 
             y = estand, 
             group = iso3,
             color = iso3) ) + 
  geom_line() + 
  facet_wrap(~gas_type) + 
  theme(legend.position = "None") +
  theme(axis.text.x = element_text(angle = 90))

data_panel_final |> 
  select(iso3:year, carbon_dioxide:nitrous_oxide) |> 
  pivot_longer(carbon_dioxide:nitrous_oxide, 
               names_to = "gas_type", 
               values_to = "value") |> 
  filter(!is.na(value)) |> 
  group_by(iso3, gas_type) |> 
  summarise(med = median(value)) |> 
  group_by(gas_type) |> 
  mutate(estand = (med - mean(med))/sd(med)) |> 
  mutate(grp = ifelse(iso3 > "k", 1, 0)) |> 
  ggplot(aes(x = gas_type, 
             y = iso3,
             fill = estand)) + 
  geom_tile() + 
  theme(axis.text.y = element_text(size = 5)) + 
  scale_fill_viridis_c() + 
  facet_wrap(~grp, scales = "free_y")


# treatments ----
data_panel_final |> 
  group_by(iso3, treatment) |> 
  summarise(n_yrs = n_distinct(year)) |> 
  print(n=Inf)


tab <- 
  data_panel_final |> 
  select(iso3, starts_with(("tax_gdp"))) |> 
  pivot_longer(tax_gdp_ecgte:tax_gdp_ecgtet, names_to = "tax_type", values_to = "value") |> 
  filter(!is.na(value)) |> 
  group_by(iso3) |> 
  summarise(uni = n_distinct(tax_type),
            num = n())

tab |> filter(iso3 == "USA")

tab |> 
  filter(uni <= 3) |> 
  select(iso3) |> 
  distinct() |> 
  nrow()


data_panel_final |> 
  select(iso3, tax_gdp_ecgtep) |> 
  na.omit() |>
  summary()
select(iso3) |> 
  distinct() |> 
  nrow()

data_panel_final |> 
  select(iso3, starts_with(("tax_gdp"))) |> 
  pivot_longer(tax_gdp_ecgte:tax_gdp_ecgtet, names_to = "tax_type", values_to = "value") |> 
  filter(!is.na(value)) |> 
  summary()

sapply(data_panel_final |> ungroup() |> select(starts_with("tax_gdp")) |> colnames(),
       function(k){
         coef(lm(as.formula(paste("carbon_dioxide ~ ", k)), data = data_panel_final))
       })

coef(lm(carbon_dioxide ~ tax_gdp_ecgte, data = data_panel_final ))
lm(carbon_dioxide ~ tax_gdp_ecgten, data = data_panel_final )
lm(carbon_dioxide ~ tax_gdp_ecgte, data = data_panel_final )



