# ------------------------------- #
# --- AULA 2 - Econometria II --- #
# ------------------------------- #

# Econometria II - Gradua??o Economia (UFABC) -  Prof. Guilherme Lima
# Rotina para os Alunos - Aula Pr?tica 2

# Objetivos:
# 1.	Dar continuidade ? Introdu??o ao Software Estat?stico R e pacotes econom?tricos dispon?veis.
# 2.	Estima??o e Infer?ncia sob Hip?tese de Heterocedasticidade: Estimador Robusto ? Heterocedasticidade de White (1980).
#		  utilizando exemplos dos livros (1) e (2).
# 3.  Estima??o e Infer?ncia sob a Hip?tese de Autocorrela??o nos Res?duos.

# Refer?ncias:
# (1) Wooldridge J.M. (2012) "Introductory Econometrics".
# (2) Wooldridge J.M. (2002) "Econometric analysis of cross section and panel data".
# (3) Kleiber, C. &  Zeileis, A. (2008) "Applied Econometrics with R". p. 106

# Sites que cont?m material auxiliar:
# https://justinmshea.github.io/wooldridge/index.html								# Base de dados de (1) e (2)
# http://eclr.humanities.manchester.ac.uk/index.php/R_robust_se					# White Robust Standard Errors
# https://www.rdocumentation.org/packages/lmtest/versions/0.9-37/topics/coeftest		# "coeftest"

# Pacote com dados dos exmplos do Wooldridge(2012):
# 1. wooldridge
# http://127.0.0.1:31689/library/wooldridge/doc/Introductory-Econometrics-Examples.html	# Exemplos do livro

# Pacotes que possuem rotinas para Heterocedasticidade e Estimador da Vari?ncia Robusto de White:
# 1. sandwich (vcovHC)
# 2. AER,  incorpora o sandwich, Ref. (3)
# 3. olsrr	(ols_test_breusch_pagan) - Teste de Godfrey-Breusch-Pagan

# Instalando os pacotes:

# install.packages("wooldridge")  # J? instalado na Aula Pr?tica 1
library(wooldridge)
help(package = "wooldridge")

data("mroz")
ls(mroz)
head(mroz)
attach(mroz)
wage
age

install.packages("sandwich")	# cont?m os testes robustos de White
library(sandwich)
help(package = "sandwich")

install.packages("lmtest")	# cont?m "coeftest"
library(lmtest)
help(package = "lmtest")

install.packages("olsrr")	# teste de Godfrey-Breusch-Pagan
library(olsrr)
help(package = "olsrr")


# Example 1 (Wage Equation for Married, Working Women):
#
# log(wage)= ?0 + ?1*exper + ?2*exper^2 + ?3*educ + ?4*age + ?5*kidslt6 + ?6*kidsge6 + u

Ex1 <- lm(lwage ~ exper + expersq + educ + age + kidslt6 + kidsge6, data=mroz)
summary(Ex1)

ols_test_breusch_pagan(Ex1)			# Teste de Breusch-Pagan-Godfrey

het.t <- vcovHC(Ex1, type = "HC0")		# HC0 Wihite (1980) => validade asy.
coeftest(Ex1, het.t)

# Opcionalmente, podemos executar a fun??o por:
coeftest(Ex1, vcovHC(Ex1, type = "HC0"))	# Diretamente, sem calcular a matriz Cov antes

het.t <- vcovHC(Ex1, type = "HC3")		# HC3 MacKinnon & Wihite(1985)  => peq. amostras
coeftest(Ex1, het.t)

coeftest(Ex1, vcovHC(Ex1, type = "HC3"))


# Teste F Robusto

# log(wage)= ?0 + ?1*exper + ?2*exper^2 + ?3*educ + ?4*age + ?5*kidslt6 + ?6*kidsge6 + u

# H0: ?4=?5=?6=0

waldtest(Ex1, .~. - age - kidslt6 - kidsge6)				      		# Sem Var. Rob. Whitw
waldtest(Ex1, .~. - age - kidslt6 - kidsge6, vcov = vcovHC)		# Utiliza HC3 como default

# Alternativamente:

Ex1.Irrest <- lm(lwage ~ exper + expersq + educ + age + kidslt6 + kidsge6, data=mroz)
Ex1.Rest <- lm(lwage ~ exper + expersq + educ, data=mroz)

waldtest(Ex1.Irrest, Ex1.Rest, vcov = vcovHC(Ex1.Irrest, type = "HC0"))


# Autocorrela??o Serial:

serie.y <- scan("seriey.csv")
serie.x1 <- scan("seriex1.csv")
serie.x2 <- scan("seriex2.csv")

Modelo3 <- lm(serie.y ~ serie.x1 + serie.x2)
summary(Modelo3)

dwtest(Modelo3)   # Teste de Durbin-Watson {lmtest}

# Quest?o: A elevada autocorrela??o positiva t?m algum efeito sobre a infer?ncia?

# FGLS:

# 1. Estimar o modelo 1.1 e salvar os res?duos:
u.hat <- residuals(lm(serie.y ~ serie.x1 + serie.x2))
plot.ts(u.hat)

# 2. Estimar o modelo 1.2 e salvar rho.hat

Modelo.aux <- lm(u.hat[2:250] ~ u.hat[1:249])   # ou:  Modelo.aux <- lm(u.hat[2:250] ~ lag(u.hat,1))

names(Modelo.aux)
rho.hat <- Modelo.aux$coefficients[2]

# 3. Aplicar transforma??es ara gerar o Modelo 4:

# 3.1 Quase-diferen?a:

serie.y.asterisk <- serie.y[2:250] - rho.hat*serie.y[1:249]
serie.x1.asterisk <- serie.x1[2:250] - rho.hat*serie.x1[1:249]
serie.x2.asterisk <- serie.x2[2:250] - rho.hat*serie.x2[1:249]

length(serie.y.asterisk)

# 3.2 Multipliar os primeiros elementos das s?ries por sqrt(1-rho.hat^2)

y1.tilda <- serie.y[1]*sqrt(1-(rho.hat^2))
x11.tilda <- serie.x1[1]*sqrt(1-(rho.hat^2))
x21.tilda <- serie.x2[1]*sqrt(1-(rho.hat^2))

# 3.3 S?ries transformadas para Modelo 4:

serie.y.transf <- numeric(250)
serie.y.transf[1] <- y1.tilda
serie.y.transf[2:250] <- serie.y.asterisk

serie.x1.transf <- numeric(250)
serie.x1.transf[1] <- x11.tilda
serie.x1.transf[2:250] <- serie.x1.asterisk

serie.x2.transf <- numeric(250)
serie.x2.transf[1] <- x21.tilda
serie.x2.transf[2:250] <- serie.x2.asterisk

# 3.4 Estimar o Modelo por OLS

Modelo.FGLS <- lm(serie.y.transf ~ serie.x1.transf + serie.x2.transf)
summary(Modelo.FGLS)



