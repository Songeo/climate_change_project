
# LIBS ----
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
  

population <-  
  read_csv("data/raw/world_bank/population.csv") |> 
  rename_all(~ gsub(" ", "_", tolower(.))) |> 
  pivot_longer(cols = c(`1960`:`...69`), names_to = "year", values_to = "population") |> 
  mutate_at("year", as.numeric) |> 
  select(iso3 = country_code, year, population) |> 
  inner_join(select(iso, iso3), 
             by = join_by(iso3))

urban <-  
  read_csv("data/raw/world_bank/Urban Population.csv") |> 
  rename_all(~ gsub(" ", "_", tolower(.))) |> 
  pivot_longer(cols = c(`1960`:`2023`), names_to = "year", values_to = "urban_pop") |> 
  mutate_at("year", as.numeric) |> 
  select(iso3 = country_code, year, urban_pop) |> 
  inner_join(select(iso, iso3), 
             by = join_by(iso3))

gdp <-  
  read_csv("data/raw/world_bank/GDP per capita (in US$).csv") |> 
  rename_all(~ gsub(" ", "_", tolower(.))) |> 
  pivot_longer(cols = c(`1960`:`2023`), names_to = "year", values_to = "gdp_capita") |> 
  mutate_at("year", as.numeric) |> 
  select(iso3 = country_code, year, gdp_capita) |> 
  filter(!is.na(gdp_capita)) |>
  inner_join(select(iso, iso3), 
             by = join_by(iso3))

industry_gdp <-  
  read_csv("data/raw/world_bank/Industry (percentage of GDP).csv") |> 
  rename_all(~ gsub(" ", "_", tolower(.))) |> 
  pivot_longer(cols = c(`1960`:`2023`), names_to = "year", values_to = "gdp_industry") |> 
  mutate_at("year", as.numeric) |> 
  select(iso3 = country_code, year, gdp_industry) |> 
  filter(!is.na(gdp_industry)) |>
  inner_join(select(iso, iso3), 
             by = join_by(iso3))

renewal <- 
  read_csv("data/raw/IMF-CID/10_Renewable_Energy.csv") |> 
  rename_all(~ gsub(" ", "_", tolower(.))) |>
  pivot_longer(cols = c(`2000`:`2022`), names_to = "year", values_to = "value") |> 
  group_by(iso3, year, energy_type) |> 
  summarise(energy_sum = sum(value, na.rm = T)) |> 
  pivot_wider(names_from = energy_type, values_from = energy_sum) |> 
  rename_all(~ gsub(" |-", "_", tolower(.))) |> 
  mutate(total_energy = total_non_renewable + total_renewable) |> 
  mutate_at("year", as.numeric) |> 
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


# UNION ----
emmisions_filtered <- 
  emmisions_yr |> 
  filter(n_yr == 53) |> 
  select(-n_yr)

taxes_filtered <- 
  taxes_yr

data_panel <- 
  emmisions_filtered |>  
  left_join(taxes_filtered,
            by = join_by(iso3, country, year)) |> 
  left_join(population,
            by = join_by(iso3, year)) |> 
  left_join(urban,
            by = join_by(iso3, year)) |> 
  left_join(gdp,
            by = join_by(iso3, year)) |> 
  left_join(industry_gdp,
            by = join_by(iso3, year)) |> 
  left_join(renewal,
            by = join_by(iso3, year)) |> 
  filter(year >= 2000 & year <= 2021)

data_panel
data_panel |> write_csv("data/processed/panel_data.csv")

data_panel <- read_csv("data/processed/panel_data.csv")


# FINAL ----

data_panel_final$iso3 |> n_distinct()

data_panel_final <- 
  data_panel |> 
  mutate(treatment = ifelse( is.na(tax_gdp_ecgtep), 0, 1), 
         outcome = carbon_dioxide,
         renewable_pct = total_renewable/total_energy, 
         urban_pct = urban_pop/population) |> 
  select( iso3, year, outcome, treatment, population, gdp_capita, gdp_industry, urban_pct, renewable_pct)
  
data_panel_final |> 
  group_by(iso3, treatment) |> 
  mutate(treatment = cumsum(treatment))

# last check
data_panel_final |> 
  group_by(treatment) |> 
  summarise(max_tmt = max(treatment),
            min_tmt = min(treatment)) 

data_panel_final |> 
  group_by(iso3) |> 
  summarise(n_yrs = n_distinct(year)) |> 
  filter(n_yrs != 22)

data_panel_final |> 
  head()
  

data_panel_final |> 
  write_csv("data/processed/panel_data_final.csv")
data_panel_final <- read_csv("data/processed/panel_data_final.csv")


