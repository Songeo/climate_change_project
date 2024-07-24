

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
  mutate(iso3num = as.numeric(factor(iso3)),
         l_outcome = log(outcome + 34)) |> 
  group_by(iso3) |> 
  mutate(first_treat = ifelse(any(treatment == 1), min(year[treatment == 1]), 0),
         time_to_treatment = ifelse(any(treatment == 1), min(year[treatment == 1]), 3000) - year) 
  #mutate_at("treatment", factor) |> 
  # filter(iso3 != "GRL")

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

gg <- panelview(l_outcome ~ treatment, 
                data = data_panel_final,
                index = c("iso3","year"),
                axis.lab = "time", xlab = "Time", ylab = "Unit", 
                theme.bw = TRUE, type = "outcome", main = "Outcome")
gg
ggsave(plot = gg, 
       filename = "results/figures/panelview_loutcome_wgrl.png", 
       width = 8, 
       height = 5)

# 3. MODEL ----
model_formula <- l_outcome ~ treatment + 
  population + gdp_capita + 
  urban_pct+  renewable_pct
print(model_formula)


# 4. ATT ----
# individual-level fixed effects and treatment interactions ----
individual_model <- feols(l_outcome ~ treatment | iso3 + year, data = data_panel_final)
summary(individual_model)


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

fe_att_lb <- model_fe$att.avg - qnorm(0.975)*sd(model_fe$att.avg.boot)
fe_att_ub <- model_fe$att.avg + qnorm(0.975)*sd(model_fe$att.avg.boot)

gg <- 
  plot(model_fe, 
     main = "Estimated ATT (FEct)", 
     ylab = "Effect of D on Y", 
     bound = "both", 
     cex.main = 0.8, 
     cex.lab = 0.8, 
     cex.axis = 0.8,
     tost.threshold = min(fe_att_lb, fe_att_ub))
gg
ggsave(plot = gg, 
       filename = "results/figures/pta_plot_fe.png", 
       width = 9, 
       height = 6.5)


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
                  loo = T)

model_ife

ife_att_lb <- model_ife$att.avg - qnorm(0.975)*sd(model_ife$att.avg.boot)
ife_att_ub <- model_ife$att.avg + qnorm(0.975)*sd(model_ife$att.avg.boot)


gg <- 
  plot(model_ife, 
     main = "Estimated ATT (IFEct)", 
     ylab = "Effect of D on Y", 
     type = "gap", 
     bound = "both", 
     cex.main = 0.8, 
     cex.lab = 0.8, 
     cex.axis = 0.8,
     tost.threshold = min(ife_att_lb, ife_att_ub))
gg
ggsave(plot = gg, 
       filename = "results/figures/pta_plot_ife.png", 
       width = 9, 
       height = 6.5)

# matrix completion fixed effects model ----
model_mcf <- fect(model_formula,
                  data = data_panel_final,
                  index = c("iso3", "year"),
                  method = "mc", 
                  force = "two-way",
                  CV = T, 
                  r = c(0, 5), 
                  se = T, 
                  nboots = 200, 
                  parallel = T, 
                  loo = T)

model_mcf

mcf_att_lb <- model_mcf$att.avg - qnorm(0.975)*sd(model_mcf$att.avg.boot)
mcf_att_ub <- model_mcf$att.avg + qnorm(0.975)*sd(model_mcf$att.avg.boot)

gg <- 
  plot(model_mcf, 
     main = "Estimated ATT (MCct)", 
     ylab = "Effect of D on Y", 
     bound = "both", 
     cex.main = 0.8, 
     cex.lab = 0.8, 
     cex.axis = 0.8,
     tost.threshold = min(ife_att_lb, ife_att_ub))

gg
ggsave(plot = gg, 
       filename = "results/figures/pta_plot_mc.png", 
       width = 9, 
       height = 6.5)

# summary
confint(individual_model)

individual_model
model_fe
model_ife
model_mcf

summary_tab <- 
  tibble(
  method = c("baseline", "fe", "ife", "mc"),
  att = c(-0.083436, 
          model_fe$att.avg, 
          model_ife$att.avg,
          model_mcf$att.avg),
  se    = c(0.072308, 
            model_fe$est.avg[2],
            model_ife$est.avg[2] ,
            model_mcf$est.avg[2]),
  ci_lb = c(-0.2260859, 
            model_fe$est.avg[3],
            model_ife$est.avg[3] ,
            model_mcf$est.avg[3]),
  ci_ub = c(0.05921331, 
            model_fe$est.avg[4],
            model_ife$est.avg[4] ,
            model_mcf$est.avg[4]),
  p_value = c(0.25002, 
              model_fe$est.avg[5],
              model_ife$est.avg[5] ,
              model_mcf$est.avg[5])
)



gg <- 
  summary_tab |> 
  filter(method != 'baseline') |> 
  ggplot(aes( x= att, y = method)) + 
  geom_vline(xintercept = 0, 
             linetype = 2, 
             color = "gray60") + 
  geom_errorbarh(aes(xmin = ci_lb, xmax = ci_ub), height = .2) + 
  geom_point(size = 3, 
             color = "red", 
             aes(size = p_value)) +
  theme(axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 12)) +
  labs(title = "Estimated ATT of D on Y ",
       x = "Year",
       y = "Method") 
gg
ggsave(plot = gg, 
       filename = "results/figures/att_summary.png", 
       width = 7, 
       height = 3.5)
