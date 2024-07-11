
# Libraries ----
library(tidyverse)


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
  filter(n_yr < 27) |> 
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
data_panel |> 
  select(iso3:year, carbon_dioxide:nitrous_oxide) |> 
  pivot_longer(carbon_dioxide:nitrous_oxide, 
               names_to = "gas_type", 
               values_to = "value") |> 
  mutate(nas = (!is.na(value))) |> 
  ggplot(aes(x = year, y = iso3, fill = nas) ) + 
  geom_tile() + 
  facet_wrap(~gas_type, nrow = 1) + 
  theme(axis.text.y = element_text(size = 4),
        axis.text.x = element_text(angle = 90))
  

data_panel |> 
  filter(year >= 1990) |> 
  select(iso3:year, starts_with("tax")) |> 
  pivot_longer(starts_with("tax"), 
               names_to = "tax_type", 
               values_to = "value") |> 
  mutate(nas = (!is.na(value))) |> 
  ggplot(aes(x = year, y = iso3, fill = nas) ) + 
  geom_tile(color = "gray") + 
  facet_wrap(~tax_type, nrow = 1) + 
  theme(axis.text.y = element_text(size = 4), 
        axis.text.x = element_text(angle = 90))

