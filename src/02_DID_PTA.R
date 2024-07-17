
### LIBS ###

library(bacondecomp)
library(did)
library(eatATA)
library(haven)
library(lfe)
library(panelView)
library(tidyverse)
library(fixest)

### Load Data ###
data_panel_final <- read_csv("data/processed/panel_data_final.csv")
data_panel_final <- data_panel_final %>%
  group_by(iso3) %>%
  mutate(popweight = mean(population, na.rm = TRUE)) %>%
  ungroup()

#Standard TWFE estimation (Cluster Std Error by Countries)
STWFE <- felm(outcome ~ treatment | year + iso3 | 0 | iso3,
               weights = data_panel_final$popweight, 
               data = data_panel_final)
summary(STWFE)

main_ATT = summary(STWFE)$coefficients["treatment", "Estimate"]
ATT_CI_LB = abs(main_ATT - qnorm(0.975)*summary(STWFE)$coefficients["treatment", "Cluster s.e."])
ATT_CI_UB = abs(main_ATT + qnorm(0.975)*summary(STWFE)$coefficients["treatment", "Cluster s.e."])

ATT_CI_LB
ATT_CI_UB

#Event study regression
event_study_formula <- as.formula(
  paste("outcome ~ treatment ",
        "| year + iso3 | 0 | iso3"
  ),
)

event_study_reg <- felm(event_study_formula, weights = data_panel_final$popweight, data = data_panel_final)
summary(event_study_reg)

#Plotting results
leadslags_plot <- tibble(
  sd = c(event_study_reg$cse[plot_order], 0),
  mean = c(coef(event_study_reg)[plot_order], 0),
  label = c(-9,-8,-7,-6, -5, -4, -3, -2, -1, 1,2,3,4,5, 0)
)
leadslags_plot %>%
  ggplot(aes(x = label, y = mean,
             ymin = mean-qnorm(0.95)*sd, 
             ymax = mean+qnorm(0.95)*sd)) +
  geom_hline(yintercept = ATT_CI_LB, color = "red") +
  geom_hline(yintercept = -ATT_CI_LB, color = "red") +
  geom_pointrange() +
  theme_minimal() +
  xlab("Years before and after tax policy") +
  ylab("CO2 Emission") +
  geom_hline(yintercept = 0,
             linetype = "dashed") +
  geom_vline(xintercept = 0,
             linetype = "dashed")


#Examine adoption status
panelview(data_panel_final,
          outcome ~ treatment,
          index = c("iso3", "year"))

#Bacon decomposition
df_bacon <- bacon(outcome ~ treatment,
                  data = data_panel_final,
                  id_var = "iso3",
                  time_var = "year"
)
df_bacon



