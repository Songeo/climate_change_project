

# LIBS ----
library(tidyverse)
library(ggplot2)

library(pwr)
#library(tidymodels)
#library(lme4)
#library(broom.mixed)

theme_set(theme_bw())


# DATA ----
panel_data <- read_csv("data/processed/panel_data_final.csv")
panel_data |> summary()



# Parameters
alpha <- 0.05
n <- nrow(panel_data)
effect_size <- 0.15  # Assuming a medium effect size
num_predictors <- 8


# Parameters
alpha <- 0.05
total_observations <- 10918  # total observations
effect_size <- 0.8  # Assuming a medium effect size
num_predictors <- 8 # 4 covariables + tmt + time + country + intercept

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



linear_reg() %>% 
  set_engine("lmer") %>% 
  set_mode("regression") %>% 
  translate()

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