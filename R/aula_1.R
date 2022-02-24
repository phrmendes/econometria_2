# ------------------------------- #
# --- AULA 1 - Econometria II --- #
# ------------------------------- #

#' Econometria II - Graduação Economia (UFABC) -  Prof. Guilherme Lima
#' Tema: Multicolinearidade
#' Referências:
#' (1) Wooldridge J.M. (2012) "Introductory Econometrics".
#' (2) Wooldridge J.M. (2002) "Econometric analysis of cross section and panel data".
#' (3) Kleiber, C. &  Zeileis, A. (2008) "Applied Econometrics with R". p. 106

# bibliotecas e funções ---------------------------------------------------

library("R/libraries.R")

# dados -------------------------------------------------------------------

wage1 <- tibble::as_tibble(wooldridge::wage1)

# regressão ---------------------------------------------------------------

# MRL: wage = beta0 + beta1*educ + beta2*exper + u

model <- wage1 |>
  fixest::feols(wage ~ 1 + educ + exper)

fixest::eta

lm(wage ~ 1 + educ + exper)
Model <- lm(wage ~ 1 + educ + exper)
summary(Model)

names(Model)				# Objetos gerados na estima??o
Model$residuals
plot(Model$residuals, type="l")


## III. Multicolinearidade

# 1. Considere a vari?vel x4 correlacionada com x3:

cor(x4,x3)

Modelo2 <- lm(y ~ x1 + x2 + x3 + x4)
summary(Modelo2)

# Matrix de correla??o entre as vari?veis explicativas:

mat.var <- matrix(nrow=length(x1), ncol=4)
mat.var[,1] <- x1
mat.var[,2] <- x2
mat.var[,3] <- x3
mat.var[,4] <- x4
colnames(mat.var) <- c("x1", "x2", "x3", "x4")
mat.var

cor(mat.var)

# Fator de Infla??o da Vari?ncia (VIF) {car}

install.packages("car")
library(car)

vif(Modelo2) # se vif~1 n?o colineridade; se vif >=5, alta colinearidade

