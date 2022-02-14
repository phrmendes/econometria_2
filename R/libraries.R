if (!require("pacman")) install.packages("pacman")

pacman::p_load(
  tidyverse,
  data.table,
  styler,
  tabulator,
  fixest,
  vroom,
  modelsummary,
  knitr,
  kableExtra,
  Hmisc,
  skimr,
  collapse,
  pbapply,
  usethis,
  install = F
)
