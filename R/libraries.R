if (!require("pacman")) {
  install.packages("pacman")
  require("pacman")
}

pacman::p_load(
  tidyverse,
  data.table,
  styler,
  fixest,
  knitr,
  wooldridge,
  collapse,
  markdown,
  rmarkdown,
  car,
  olsrr,
  flightsbr,
  plm,
  lmtest,
  AER,
  janitor,
  mice,
  margins,
  prais,
  AER,
  install = TRUE
)
