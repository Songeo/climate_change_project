
# Libraries ----
library(tidyverse)
library(ggplot2)
theme_set(theme_bw())

# DATA ----
iso <- 
  read_csv("data/raw/list_country_iso_code.csv") |> 
  rename_all(~ gsub(" ", "_", tolower(.))) |> 
  select(country_name, iso3)

emmisions <- 
  read_csv("data/raw/IMF-CID/3_National_Greenhouse_Gas_Emissions_Inventories_and_Implied_National_Mitigation_Nationally_Determined_Contributions_Targets.csv") |>
  rename_all(~ gsub(" ", "_", tolower(.))) |> 
  pivot_longer(cols = c(`1970`:`2030`), names_to = "year",values_to = "value_emission") |> 
  mutate_at("year", as.numeric) |> 
  filter(!is.na(value_emission)) |> 
  inner_join(select(iso, iso3), 
             by = join_by(iso3))

taxes <- 
  read_csv("data/raw/IMF-CID/7_Environmental_Taxes.csv") |> 
  rename_all(~ gsub(" ", "_", tolower(.))) |> 
  pivot_longer(cols = c(`1995`:`2021`), names_to = "year", values_to = "value_tax") |> 
  mutate_at("year", as.numeric) |> 
  filter(!is.na(value_tax)) |>
  inner_join(select(iso, iso3), 
             by = join_by(iso3))
  

# Emmisions ----

# unique emissions per year including landscape 
emmisions_yr <- 
  emmisions |> 
  filter(cts_code %in% c("ECNGDI")) |> 
  select(iso3, country, gas_type, year, value_emission) |> 
  mutate_at("gas_type", ~ gsub(" ", "_", tolower(.)) ) |> 
  pivot_wider(names_from = "gas_type", values_from = "value_emission") |> 
  group_by(iso3) |> 
  mutate(n_yr = n_distinct(year))
  
# review of data
summary(emmisions_yr)
emmisions_yr |> 
  filter(n_yr < 53) |> 
  select(iso3, country) |> 
  unique()

# table of counts pero country
emmisions |> 
  filter(cts_code %in% c("ECNGDI")) |> 
  group_by(iso3, indicator, unit, source, cts_name, cts_code, gas_type, industry) |> 
  summarise(n_rows = n(), 
            n_iso3 = n_distinct(iso3), 
            min_yr = min(year), 
            max_yr = max(year)) |> 
  write.table("results/tables/pre_emissions_iso.txt", sep = ";", row.names = F)

# Taxes ----

# unique emissions per year including landscape 
taxes_yr <- 
  taxes |> 
  select(iso3, country, unit, cts_code, year, value_tax) |> 
  mutate_at(c("unit", "cts_code"), ~ gsub(" |[(]", "_", tolower(.))) |> 
  mutate_at("unit", ~ gsub("domestic_currency", "tax_currency", .)) |> 
  mutate_at("unit", ~ gsub("percent_of_gdp", "tax_gdp", .)) |> 
  unite("tax_desc", c(unit, cts_code), sep = "_") |> 
  pivot_wider(names_from = "tax_desc", values_from = "value_tax") 

# review of data
summary(taxes_yr)
taxes_yr |> 
  select(iso3, country) |> 
  unique()


# table of counts pero country
taxes |> 
  #filter(cts_code %in% c("ECNGDI")) |> 
  group_by(iso3, indicator, unit, source, cts_name, cts_code) |> 
  summarise(n_rows = n(), 
            n_iso3 = n_distinct(iso3), 
            min_yr = min(year), 
            max_yr = max(year)) |> 
  write.table("results/tables/pre_taxes_iso.txt", sep = ";", row.names = F)


# Union ----
emmisions_filtered <- 
  emmisions_yr |> 
  filter(n_yr == 53) |> 
  select(-n_yr)

taxes_filtered <- 
  taxes_yr

data_panel <- 
  emmisions_filtered |>  
  left_join(taxes_filtered,
            by = join_by(iso3, country, year))

data_panel |> write_csv("data/processed/panel_data.csv")



# EDA ----
n_distinct(data_panel$iso3)

data |> select(iso3, year) |> distinct()

# missing data
gg <- data_panel |> 
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
  
data_panel |> 
  filter(year >= 1990) |> 
  select(iso3:year, starts_with("tax")) |> 
  pivot_longer(starts_with("tax"), 
               names_to = "tax_type", 
               values_to = "value") |>
  filter(!is.na(value)) |> 
  mutate(no_nas = !(is.na(value))) |> 
  ggplot(aes(x = year, y = iso3, fill = no_nas) ) + 
  geom_tile(color = "white") + 
  facet_wrap(~tax_type, nrow = 1) + 
  theme(axis.text.y = element_text(size = 4), 
        axis.text.x = element_text(angle = 90))

# pollution metrics summary
data_panel |> 
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

data_panel |> 
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
tab <- 
  data_panel |> 
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


data_panel |> 
  select(iso3, tax_gdp_ecgtep) |> 
  na.omit() |>
  summary()
  select(iso3) |> 
  distinct() |> 
  nrow()
  
data_panel |> 
  select(iso3, starts_with(("tax_gdp"))) |> 
  pivot_longer(tax_gdp_ecgte:tax_gdp_ecgtet, names_to = "tax_type", values_to = "value") |> 
  filter(!is.na(value)) |> 
  summary()

sapply(data_panel |> ungroup() |> select(starts_with("tax_gdp")) |> colnames(),
       function(k){
         coef(lm(as.formula(paste("carbon_dioxide ~ ", k)), data = data_panel))
})

coef(lm(carbon_dioxide ~ tax_gdp_ecgte, data = data_panel ))
lm(carbon_dioxide ~ tax_gdp_ecgten, data = data_panel )
lm(carbon_dioxide ~ tax_gdp_ecgte, data = data_panel )



