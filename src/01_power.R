

# LIBS ----
library(tidyverse)
library(ggplot2)

library(pwr)
library(tidymodels)
library(lme4)
library(broom.mixed)

theme_set(theme_bw())


# DATA ----
panel_data <- read_csv("data/processed/panel_data_final.csv")
panel_data |> summary()



# Parameters
n <- nrow(panel_data)
min_date <- min(panel_data$year)

alpha <- 0.05
effect_size <- 0.15  # Assuming a medium effect size
num_predictors <- 8



# Simulation

generate_data <- function(n_countries, n_years, effect_size) {
  countries <- rep(1:n_countries, each = n_years)
  years <- rep(1970:(1970 + n_years - 1), times = n_countries)
  data <- tibble(
    country = factor(countries),
    year = years,
    tax = ifelse(years >= sample(1970:(1970 + n_years - 1), n_countries, replace = TRUE), 1, 0),
    GDP = rnorm(n_countries * n_years, mean = 10000, sd = 2000),
    population = rnorm(n_countries * n_years, mean = 1e6, sd = 200000),
    industrial_activity = rnorm(n_countries * n_years, mean = 100, sd = 20),
    energy_consumption = rnorm(n_countries * n_years, mean = 5000, sd = 1000),
    CO2 = 10 + effect_size * tax + 0.001 * GDP + 
      0.00001 * population + 0.1 * industrial_activity + 
      0.05 * energy_consumption + rnorm(n_countries * n_years)
  )
  return(data)
}


# Post-hoc power analysis for linear regression
power_analysis <- pwr.f2.test(u = num_predictors,  # number of predictors
                              v = n - num_predictors - 1,  # df for error
                              f2 = effect_size,
                              sig.level = alpha)

print(power_analysis)




# Assuming `data` is your dataset with variables `CO2_emissions`, `country`, and `year`
library(tidymodels)
library(multilevelmod)

# Specify the model
lmer_spec <- 
  linear_reg() %>% 
  set_engine("lmer")

lmer_wflow <- 
  workflow() %>% 
  add_variables(outcomes = outcome, 
                predictors = c(iso3, year)) %>% 
  add_model(lmer_spec, 
            formula = outcome ~ (1 | iso3) + (1 | year))

fit(lmer_wflow, data = panel_data)


lmer_fit <- 
  lmer_spec %>% 
  fit(outcome ~ (1 | iso3) + (1 | year), data = panel_data)
lmer_fit

model <- lmer(outcome ~ (1 | iso3) + (1 | year), data = panel_data)
icc_estimate <- performance::icc(model)
print(icc_estimate)



# Calculate effect size
effect_size <- cohen.ES(test = "f2", size = "medium")$effect.size

# Power analysis
power_analysis <- pwr.f2.test(u = 2,
                              v = n - 2, 
                              f2 = effect_size, 
                              sig.level = alpha, 
                              power = power)
print(power_analysis)