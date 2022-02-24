
# Econometria II - Graduação Economia (UFABC) -  Prof. Guilherme Lima
# Rotina para os Alunos - Aula 4

# Objetivos:
# 1.	Dar continuidade à Introdução ao Software Estatístico R e pacotes econométricos disponíveis.
# 2.	Identificação, Estimação e Inferência sob Hipótese de Autocorrelação.
#	utilizando exemplos dos livro (1).

# Referências:
# (1) Wooldridge J.M. (2012) "Introductory Econometrics".  
# (2) Wooldridge J.M. (2002) "Econometric analysis of cross section and panel data".
# (3) Kleiber, C. &  Zeileis, A. (2008) "Applied Econometrics with R". p. 106

# Sites que contém material auxiliar:
# https://justinmshea.github.io/wooldridge/index.html								# Base de dados de (1) e (2)
# http://eclr.humanities.manchester.ac.uk/index.php/R_robust_se						# White Robust Standard Errors
# https://www.rdocumentation.org/packages/lmtest/versions/0.9-37/topics/coeftest	# "coeftest"


# Pacotes:

# install.packages("wooldridge") 	# O pacote wooldridge para R contém os dados do livro Introductory Econometrics
library(wooldridge)
# help(package = "wooldridge")

install.packages("prais")	# Estimação por FGLS (Prais Winsten)
library(prais)
# help(package = "prais")


##################################################################################################
# Example 1: Effects of inflation and deficits on interest rates
##################################################################################################

data("intdef")
ls(intdef)
head(intdef)
attach(intdef)
length(i3)

# i3 = ß0 + ß1*inf + ß2*def + ut

# i3: 3 month T-bill rate
# inf: CPI inflation rate
# rec: federal receipts, percent GDP
# out: federal outlays, percent GDP
# def: out - rec

# Model 1: OLS

M1 <- lm(i3 ~ inf + def)
summary(M1)
ut <- residuals(M1)
pacf(ut)

durbinWatsonTest(M1)

# Model 2: FGLS

M2 <- prais_winsten(i3 ~ inf + def, data=intdef )
summary(M2)


# Exemplos do Cap. 10 


##################################################################################################
# Example 2: Puerto rican employment and the Minimum Wage
##################################################################################################

data("prminwge")
ls(prminwge)
head(prminwge)
attach(prminwge)

# log(prepop) = ß0 + ß1*log(mincov) + ß2*log(usgnp) + ut

# prepopt is the employment rate in Puerto Rico during year t (ratio of those working to total population)
# usgnpt is real U.S. gross national product (in billions of dollars)
# mincov measures the importance of the minimum wage relative to average wages. 
# mincov =(avgmin/avgwage)·avgcov, where avgmin is the average minimum wage,
# avgwage is the average overall wage, and avgcov is the average coverage rate (the proportion of workers actually covered by the minimum wage law).


M3 <- lm(lprepop ~ lmincov + lusgnp, data=prminwge)
summary(M3)

M4 <- prais_winsten(lprepop ~ lmincov + lusgnp, data=prminwge)
summary(M4)


