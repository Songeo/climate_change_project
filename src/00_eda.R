

library(tidyverse)

iso <- read_csv("data/raw/list_country_iso_code.csv") |> 
  select(country_name, ISO3)

co2 <- read_csv("data/raw/IMF-CID/3_National_Greenhouse_Gas_Emissions_Inventories_and_Implied_National_Mitigation_Nationally_Determined_Contributions_Targets.csv") |>
  pivot_longer(cols = c(`1995`:`2021`), names_to = "year",values_to = "value") |> 
  mutate_at("year", as.numeric) |> 
  filter(!is.na(value))

taxes <- read_csv("data/raw/IMF-CID/7_Environmental_Taxes.csv") |> 
  pivot_longer(cols = c(`1995`:`2021`), names_to = "year",values_to = "value") |> 
  mutate_at("year", as.numeric) |> 
  filter(!is.na(value))


# countries ----
taxes |> filter(is.na(ISO3)) |> select(Country) |> unique()
co2 |> filter(is.na(ISO3)) |> select(Country) |> unique()

co_co2 <- co2 |> 
  select(ISO3) |>
  unique() |> 
  mutate(co2 = 1)

co_tax <- taxes |> 
  select(ISO3) |> 
  unique() |>
  mutate(tax = 1) 


co_tax |> 
  full_join(co_co2) |> 
  left_join(iso, by = join_by(ISO3)) |> 
  filter(!is.na(country_name)) |> 
  print(n = Inf)

sco_tax |> 
  full_join(co_co2) |> 
  left_join(iso, by = join_by(ISO3)) |> 
  filter(!is.na(co2)) |> 
  print(n = Inf)


taxes |> head()
taxes |> group_by(Unit) |>  count() |> print(n = Inf)
taxes |> group_by(ISO3) |>  count() |> print(n = Inf)


# plots ----

co2 |> 
  filter(ISO3 == "ARG") |> 
  ggplot(aes( x= year, y = value, color = `CTS Code`, group = `CTS Code`)) + 
  geom_point() + 
  geom_line() + 
  facet_wrap(~ISO3, scales = "free_y")
