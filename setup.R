
# R version: R version 4.4.1 (2024-06-14)

# List of packages to run impact analysis 
packages_list <- 
  c("tidyverse", 
    "imputeTS", 
    "fect", 
    "patchwork", 
    "did", 
    "haven",
    "devtools")
install.packages(packages_list)
devtools::install_github('xuyiqing/panelView') 

# List of packages to run report
reports_list <- 
  c("rmarkdown")
install.packages(reports_list)