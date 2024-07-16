
# LIBS ----
library(tidyverse)
library(ggplot2)
library(imputeTS)

theme_set(theme_bw())

# 1. DATA ----
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


# Emmisions

# unique emissions per year including landscape 
# Total GHG emissions including land-use, land-use change and forestry	
# Million metric tons of CO2 equivalent

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

# Taxes

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


# 2. UNION ----
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

# write table
data_panel
data_panel |> write_csv("data/processed/panel_data.csv")
data_panel <- read_csv("data/processed/panel_data.csv")



# 3. TRANSFORMATIONS ----
data_panel_transformed <- 
  data_panel |> 
  mutate(treatment = ifelse( is.na(tax_gdp_ecgtep), 0, 1), 
         outcome = carbon_dioxide,
         renewable_pct = total_renewable/total_energy, 
         urban_pct = urban_pop/population) |> 
  select( iso3, 
          year, 
          outcome, 
          treatment, 
          population, 
          gdp_capita, 
          gdp_industry, 
          urban_pct, 
          renewable_pct)

data_panel_transformed$iso3 |> n_distinct()

data_panel_transformed |> 
  group_by(iso3, treatment) |> 
  summarise(n_yrs = n_distinct(year)) |> 
  filter(n_yrs != 22) |> 
  pivot_wider(names_from = treatment, values_from = n_yrs) |> 
  print(n = Inf)

# write table
data_panel_transformed
data_panel_transformed |> write_csv("data/processed/panel_transformed_data.csv")
data_panel_transformed <- read_csv("data/processed/panel_transformed_data.csv")


# MISSING VALUES ---

# nas per covariate
nas_tab <- 
  data_panel_transformed |> 
  pivot_longer(cols = population:renewable_pct) |> 
  filter(is.na(value)) |> 
  group_by(iso3, name) |> 
  count() |> 
  pivot_wider(names_from = name, 
              values_from = n) |> 
  mutate(max_total = max(c(gdp_industry, 
                        gdp_capita, 
                        population, 
                        urban_pct, 
                        renewable_pct), 
                      na.rm = T))

nas_tab |> 
  filter(max_total < 22) |> 
  nrow()

nas_tab |> 
  filter(max_total >= 22) |> 
  nrow()

# filtering missing values in all years in at least one variable
data_panel_filtered <- 
  data_panel_transformed |> 
  anti_join(nas_tab |> 
              filter(max_total >= 22) |> 
              select(iso3), 
            by = join_by(iso3))

# last check
data_panel_filtered$iso3 |> n_distinct()
data_panel_filtered |> summary()

data_panel_filtered |> 
  group_by(treatment) |> 
  summarise(max_tmt = max(treatment),
            min_tmt = min(treatment)) 

data_panel_filtered |> 
  group_by(iso3) |> 
  summarise(n_yrs = n_distinct(year)) |> 
  filter(n_yrs != 22)

data_panel_filtered |> 
  group_by(iso3, treatment) |> 
  summarise(n_yrs = n_distinct(year)) |> 
  filter(n_yrs != 22) |> 
  pivot_wider(names_from = treatment, values_from = n_yrs) |> 
  print(n = Inf)

# write table
data_panel_filtered
data_panel_filtered |> write_csv("data/processed/panel_filtered_data.csv")
data_panel_filtered <- read_csv("data/processed/panel_filtered_data.csv")



# 4. IMPUTATION ----


# missing values visualization
tab <- 
  data_panel_filtered |> 
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


# pre imputation evaluation
covars_long <-
  data_panel_filtered |> 
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

sapply(unique(covars_long$variable), function(var){
  gg <- 
   covars_long  |> 
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
        filename = glue::glue("results/figures/covariate_pre_{var_lower}.png"), 
        width = 16, 
        height = 12)
})



# imputation
impute_ts <- function(group) {
  group <- group  |> 
    arrange(year) |> 
    mutate(across(population:renewable_pct,
                  ~ na.interpolation(.)))
  return(group)
}

data_panel_imputed <- 
  data_panel_filtered |> 
  group_by(iso3) |> 
  group_modify(~ impute_ts(.x)) |> 
  ungroup()

# post imputation evaluation
comparison <- 
  data_panel_imputed |> 
  pivot_longer(cols = population:renewable_pct, values_to = "imputation") |> 
  left_join(data_panel_filtered |> 
              pivot_longer(cols = population:renewable_pct, values_to = "original"),
            by = join_by(iso3, year, outcome, treatment, name)) |> 
  group_by(iso3, name) |> 
  mutate(nas_sum = sum(is.na(original))) |> 
  filter(nas_sum > 0)

gg <- 
  comparison |> 
  ggplot(aes( x= year,
              y = original, 
              group = name)) + 
  geom_vline(aes(xintercept = ifelse(treatment == 1, year, NA)), 
             alpha = .7, 
             color = "gray40",
             linetype = 2)  + 
  geom_line(aes(y = imputation), color = "red", size = 2, alpha = .7) + 
  geom_line(color = "blue") + 
  facet_wrap(~name + iso3, scales = "free")

gg
ggsave(plot = gg, 
       filename = "results/figures/imputation_comparison.png", 
       width = 14, 
       height = 10)


# no missing values in the data set
data_panel_imputed |> 
  is.na() |> 
  sum()

data_panel_imputed$iso3 |> n_distinct()


# FINAL DATA ----

data_panel_final <- data_panel_imputed
data_panel_final |> write_csv("data/processed/panel_data_final.csv")
data_panel_final <- read_csv("data/processed/panel_data_final.csv")

