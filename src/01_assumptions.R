

# LIBS ----
library(tidyverse)
library(ggplot2)
theme_set(theme_bw())

library(fect)
library(panelView)
library(patchwork)

# library(lfe)


# DATA ----
data_panel_final <- read_csv("data/processed/panel_data_final.csv") |> 
  mutate(l_outcome = log(outcome + 100))


data_panel_final$outcome |> summary()


panelview(l_outcome ~ treatment, 
          data = data_panel_final,
          index = c("iso3","year"), 
          axis.lab = "time", xlab = "Time", ylab = "Unit", 
          background = "white", 
          main = "Treatment Status")

panelview(l_outcome ~ treatment, 
          data = data_panel_final,
          index = c("iso3","year"),
          axis.lab = "time", xlab = "Time", ylab = "Unit", 
          theme.bw = TRUE, type = "outcome", main = "Outcome")


out.fect <- fect(outcome ~ treatment + population + gdp_capita,
                 data = data_panel_final,
                 index = c("iso3","year"),
                 method = "fe", force = "two-way")

plot(out.fect, type = "gap")

plot(out.fect, 
     main = "Estimated ATT (FEct)", 
     ylab = "Effect of D on Y", 
     cex.main = 0.8, cex.lab = 0.8, cex.axis = 0.8)

out.fect <- fect(outcome ~ treatment + population + gdp_capita,
                 data = data_panel_final,
                 index = c("iso3","year"), 
                 method = "fe",
                 force = "two-way", 
                 se = TRUE, parallel = TRUE, nboots = 200)
# For identification purposes, units whose number of untreated periods <1 are dropped automatically.

plot(out.fect, 
     main = "Estimated ATT (FEct)", 
     ylab = "Effect of D on Y", 
     cex.main = 0.8, cex.lab = 0.8, cex.axis = 0.8)

out.fect$est.att
out.fect$est.avg
out.fect$est.beta

plot(out.fect, 
     type = "equiv", 
     cex.legend = 0.6,
     main = "Testing Pre-Trend (FEct)", 
     cex.text = 0.8)

out.fect.loo <- fect(outcome ~ treatment + population + gdp_capita,
                     data = data_panel_final,
                     index = c("iso3","year"), 
                     method = "fe", 
                     force = "two-way", 
                     se = TRUE, parallel = TRUE, nboots = 200, loo = TRUE)
out.ife.loo <- fect(outcome ~ treatment + population + gdp_capita,
                    data = data_panel_final,
                    index = c("iso3","year"), 
                    method = "ife", 
                    force = "two-way", 
                    se = TRUE, parallel = TRUE, nboots = 200, loo = TRUE)
out.mc.loo <- fect(outcome ~ treatment + population + gdp_capita,
                   data = data_panel_final,
                   index = c("iso3","year"), 
                   method = "mc", 
                   force = "two-way", 
                   se = TRUE, parallel = TRUE, nboots = 200, loo = TRUE)

plot(out.fect.loo, type = "equiv", loo = TRUE,
     cex.legend = 0.6, main = "Testing Pre-Trend LOO (FEct)", cex.text = 0.8)

plot(out.ife.loo, type = "equiv",  loo = TRUE,
     cex.legend = 0.6, main = "Testing Pre-Trend LOO (IFEct)", cex.text = 0.8)

