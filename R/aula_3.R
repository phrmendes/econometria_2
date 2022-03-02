
# Econometria II - Gradua??o Economia (UFABC) -  Prof. Guilherme Lima
# Rotina para os Alunos - Aula 3

# Objetivos:
# 1.	Dar continuidade ? Introdu??o ao Software Estat?stico R e pacotes econom?tricos dispon?veis.
# 2.	Identifica??o, Estima??o e Infer?ncia sob Hip?tese de Multicolinearidade e Heterocedasticidade.
#		utilizando exemplos dos livro (1).

# Refer?ncias:
# (1) Wooldridge J.M. (2012) "Introductory Econometrics".
# (2) Wooldridge J.M. (2002) "Econometric analysis of cross section and panel data".
# (3) Kleiber, C. &  Zeileis, A. (2008) "Applied Econometrics with R". p. 106

# Sites que cont?m material auxiliar:
# https://justinmshea.github.io/wooldridge/index.html								# Base de dados de (1) e (2)
# http://eclr.humanities.manchester.ac.uk/index.php/R_robust_se					# White Robust Standard Errors
# https://www.rdocumentation.org/packages/lmtest/versions/0.9-37/topics/coeftest		# "coeftest"


# Pacotes:

# install.packages("wooldridge") 	# O pacote wooldridge para R cont?m os dados do livro Introductory Econometrics
library(wooldridge)
# help(package = "wooldridge")

# install.packages("sandwich")	# cont?m os testes robustos de White
library(sandwich)
# help(package = "sandwich")

# install.packages("lmtest")	# cont?m "coeftest"
library(lmtest)
# help(package = "lmtest")

# install.packages("olsrr")	# cont?m os testes robustos de White
library(olsrr)
# help(package = "olsrr")


##################################################################################################
# Example 1: Economic Model of Crime
##################################################################################################

data("crime1")
ls(crime1)
head(crime1)
attach(crime1)

# narr86 = ?0 + ?1*pcnv + ?2*avgsen + ?3*tottime + ?4*ptime86 + ?5*qemp86 + u

# narr86	n?mero de vezes que o indiv?duo foi preso.
# pcnv		propor??o de pris?es anteriores que levaram ? condena??o (proxy para a probabilidade de ser condenado por um crime)
# avgsen	senten?a m?dia das condena??es passadas (medida da severidade dos crimes passados)
# tottime	tempo total que o indiv?duo passou na pris?o antes de 1986, desde que completou 18 anos.
# ptime86	meses de pris?o em 1986 (captura os efeitos do encarceiramente em 1986 sobre o n?mero de pris?es).
# qemp86	n?mero de trimestres em 1986 em que o indiv?duo esteve formalmente empregado.


# Propor??o dos homens que foram presos em 1986:
sum(as.integer(narr86==0))/length(narr86)	# Parcela que n?o foi presa em 1986 (narr86=0)
sum(as.integer(narr86>0))/length(narr86)		# Parcela que foi presa em 1986 (narr86>0)
sum(as.integer(narr86>1))/length(narr86)		# Parcela que foi presa mais de 1 vez em 1986 (narr86>1)


crime <- lm(narr86 ~ pcnv + avgsen + tottime + ptime86 + qemp86 )
summary(crime)

# 1. Diagn?stoco da Multicolinearidade: VIF

# Mede o quanto a vari?ncia do coeficiente estimado ? inflacionado pela correla??o com os outros regressores.
# Regras de Bolso para VIF
# Se VIF~1 (N?o existe infla??o da vari?ncia);
# Se 4 < VIF < 10 (multicolinearidade "leve");
# Se VIF > 10 (multicolinearidade "grave");

ols_vif_tol(crime)

crime.1 <- lm(narr86 ~ pcnv + avgsen + ptime86 + qemp86 )
summary(crime.1)

crime.2 <- lm(narr86 ~ pcnv + tottime + ptime86 + qemp86 ) #*
summary(crime.2)


# Heterocedasticidade:

ols_test_breusch_pagan(crime)			# Teste de Breusch-Pagan-Godfrey
ols_test_breusch_pagan(crime.2)			# Teste de Breusch-Pagan-Godfrey


# Infer?ncia Robusta:

coeftest(crime, vcovHC(crime, type = "HC3"))	# Diretamente, sem calcular a matriz Cov antes
coeftest(crime.2, vcovHC(crime.2, type = "HC3"))	# Diretamente, sem calcular a matriz Cov antes


waldtest(crime, .~. - avgsen - tottime, vcov = vcovHC)		# Utiliza HC3 como default


# Normalidade dos res?duos:

ols_plot_resid_qq(crime)
ols_test_normality(crime)


##################################################################################################
# Example 2: Housing prices and air pollution
##################################################################################################

detach(crime1)
attach(hprice2)
head(hprice2)

# Amostra de 506 comunidades na ?rea de Boston:

# log(price) = ?0 + ?1*log(nox) + ?2*log(dist) + ?3*rooms + ?4*stratio + u
#  ?1 e ?2: representam Elasticidades (Var%)/Var(%)
#  ?3 e ?4: representam Semi-Elasticidades (Var%)/Var (1unid) => 100*?3 = Var(%) nos pre?os


# price: preco mediano das casas na comunidade;
# nox:	quantidade de ?xido de nitrog?nio no ar (partes por milh?o);
# dist: dist?ncia "ponderada" da comunidade de 5 centros de trabalho (em milhas);
# rooms: n?mero m?dio de quartos nas casas da comunidade;
# stratio: ? a raz?o (propor??o) m?dia  de Professor/Alunos das escolas da comunidade;

h.price <- lm(log(price) ~ log(nox) + log(dist) + rooms + stratio )
summary(h.price)

# 1. Colinearidade:

ols_vif_tol(h.price)


# 2. Heterocedastidicade:

ols_test_breusch_pagan(h.price)			# Teste de Breusch-Pagan-Godfrey


# 3. Infer?ncia Robusta:

coeftest(h.price, vcovHC(h.price, type = "HC3"))	# Diretamente, sem calcular a matriz Cov antes

#               Estimate Std. Error  t value  Pr(>|t|)
# (Intercept) 11.0838610  0.3825081  28.9768 < 2.2e-16 ***
# log(nox)    -0.9535385  0.1282244  -7.4365 4.516e-13 ***	+1% nox => -0.95% price
# log(dist)   -0.1343394  0.0540771  -2.4842   0.01331 *  	+1% dist=> -0.13% price
# rooms        0.2545271  0.0252019  10.0995 < 2.2e-16 ***	+1 room => +25.45% price
# stratio     -0.0524511  0.0046592 -11.2576 < 2.2e-16 ***	+1 stratio => -5.24% price

# Obs: Corre??o da Semi_Elasticidade: (p.171)
# A medida que a varia??o em log(price) aumenta, a aproxima??o %Dy ~ 100*Dlog(y)
# Corre??o: %Dy ~ 100*[(exp(?j*Dxj)-1]


100*(exp(0.2545271)-1)
100*(exp(-0.0524511)-1)

# E = -1(?). Isto ?, um aumento de 1% nox gera em m?dia queda de 1% no pre?o m?dio das resid?ncias?

t0 <- (-0.95353852 + 1)/(0.1282244)
2*pt(abs(t0), df = 501, lower.tail=FALSE)	# H0:?1=1 vs. H1:?1<>1
pt(t0, df = 501, lower.tail=TRUE)				# H0:?1=1 vs. H1:?1<1

# Obs: P-Valores:

# Left-Tailed Tests:	P-value = pt(t0, df = n-1, lower.tail=TRUE)
# Right-Tailed Tests: P-value = pt(t0, df = n-1, lower.tail=FALSE)
# Two-Tailed Tests:	P-value = 2 * pt( abs(t0), df = n-1, lower.tail=FALSE)


# Normalidade dos res?duos:

ols_plot_resid_qq(h.price)
ols_test_normality(h.price)




