




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

