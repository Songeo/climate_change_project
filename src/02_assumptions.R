

# 0. LIBS ----
library(tidyverse)
library(ggplot2)
theme_set(theme_bw())

library(panelView)
library(patchwork)
library(did)
library(fixest)
library(fect)



# 1. DATA ----
data_panel_final <- 
  read_csv("data/processed/panel_data_final.csv") |> 
  mutate(iso3num = as.numeric(factor(iso3)) ) |> 
  group_by(iso3) |> 
  mutate(first_treat = ifelse(any(treatment == 1), min(year[treatment == 1]), 0),
         time_to_treatment = ifelse(any(treatment == 1), min(year[treatment == 1]), 3000) - year) |> 
  mutate_at("treatment", factor)# |> 
  filter(iso3 != "GRL")

data_panel_final |> filter(iso3 == "MEX") |> print(n = 45)
data_panel_final |> filter(iso3 == "ABW") |> print(n = 45)
data_panel_final |> summary()
data_panel_final$outcome |> summary()


# 2. VIS ----
gg <- panelview(outcome ~ treatment, 
          data = data_panel_final,
          index = c("iso3","year"), 
          axis.lab = "time", xlab = "Time", ylab = "Unit", 
          background = "white", 
          main = "Treatment Status")
gg
ggsave(plot = gg, 
       filename = "results/figures/panelview_treatment_wgrl.png", 
       width = 7, 
       height = 6)

gg <- panelview(outcome ~ treatment, 
          data = data_panel_final,
          index = c("iso3","year"),
          axis.lab = "time", xlab = "Time", ylab = "Unit", 
          theme.bw = TRUE, type = "outcome", main = "Outcome")
gg
ggsave(plot = gg, 
       filename = "results/figures/panelview_outcome_wgrl.png", 
       width = 8, 
       height = 5)


# 3. MODEL ----
model_formula <- outcome ~ treatment + 
  population + gdp_capita + 
  urban_pct+  renewable_pct
print(model_formula)

# 4. ATT ----

# event study design ----
event_model <- feols(outcome ~ i(time_to_treatment, treatment, ref = -1) | iso3 + year, 
                     data = data_panel_final)
summary(event_model)


# staggered adoption DiD ----
att_gt <- att_gt(yname = "outcome",
                 tname = "year",
                 idname = "iso3num",
                 gname = "first_treat",
                 data = data_panel_final,
                 control_group = "nevertreated")

#' Dropped 88 units that were already treated in the first period.
#' Check groups: 2005,2008,2010,2017.
#' Not returning pre-test Wald statistic due to singular covariance matrix

summary(att_gt)
ggdid(att_gt)



# individual-level fixed effects and treatment interactions ----
interaction_model <- feols(outcome ~ treatment | iso3 + year, data = data_panel_final)
summary(interaction_model)

# factor-augmented fixed effects model ----
model_fe <- fect(model_formula,
                 data = data_panel_final,
                 index = c("iso3", "year"),
                 method = "fe", 
                 force = "two-way",
                 CV = T, 
                 r = c(0, 5), 
                 se = T, 
                 nboots = 200, 
                 parallel = T, 
                 loo = T)

model_fe

plot(model_fe, 
     main = "Estimated ATT (FEct)", 
     ylab = "Effect of D on Y", 
     cex.main = 0.8, 
     cex.lab = 0.8, 
     cex.axis = 0.8)

# interactive fixed effects model ----
model_ife <- fect(model_formula,
                  data = data_panel_final,
                  index = c("iso3", "year"),
                  method = "ife", 
                  force = "two-way",
                  CV = T, 
                  r = c(0, 5), 
                  se = T, 
                  nboots = 200, 
                  parallel = T, 
                  loo = F)

model_ife

plot(model_ife, 
     main = "Estimated ATT (IFEct)", 
     ylab = "Effect of D on Y", 
     cex.main = 0.8, 
     cex.lab = 0.8, 
     cex.axis = 0.8)



# matrix completion fixed effects model ----
model_mcf <- fect(model_formula,
                  data = data_panel_final,
                  index = c("iso3", "year"),
                  method = "mc", 
                  force = "two-way",
                  CV = T, 
                  r = c(0, 5), 
                  se = T, 
                  nboots = 100, 
                  parallel = T, 
                  loo = F)

model_mcf

plot(model_mcf, 
     main = "Estimated ATT (CFct)", 
     ylab = "Effect of D on Y", 
     cex.main = 0.8, 
     cex.lab = 0.8, 
     cex.axis = 0.8)



# summary 
model_fe
model_ife
model_mcf
