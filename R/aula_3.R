
# Econometria II - Graduação Economia (UFABC) -  Prof. Guilherme Lima
# Rotina para os Alunos - Aula 3

# Objetivos:
# 1.	Dar continuidade à Introdução ao Software Estatístico R e pacotes econométricos disponíveis.
# 2.	Identificação, Estimação e Inferência sob Hipótese de Multicolinearidade e Heterocedasticidade.
#		utilizando exemplos dos livro (1).

# Referências:
# (1) Wooldridge J.M. (2012) "Introductory Econometrics".  
# (2) Wooldridge J.M. (2002) "Econometric analysis of cross section and panel data".
# (3) Kleiber, C. &  Zeileis, A. (2008) "Applied Econometrics with R". p. 106

# Sites que contém material auxiliar:
# https://justinmshea.github.io/wooldridge/index.html								# Base de dados de (1) e (2)
# http://eclr.humanities.manchester.ac.uk/index.php/R_robust_se					# White Robust Standard Errors
# https://www.rdocumentation.org/packages/lmtest/versions/0.9-37/topics/coeftest		# "coeftest"


# Pacotes:

# install.packages("wooldridge") 	# O pacote wooldridge para R contém os dados do livro Introductory Econometrics
library(wooldridge)
# help(package = "wooldridge")

# install.packages("sandwich")	# contém os testes robustos de White
library(sandwich)
# help(package = "sandwich")

# install.packages("lmtest")	# contém "coeftest"
library(lmtest)
# help(package = "lmtest")

# install.packages("olsrr")	# contém os testes robustos de White
library(olsrr)
# help(package = "olsrr")


##################################################################################################
# Example 1: Economic Model of Crime
##################################################################################################

data("crime1")
ls(crime1)
head(crime1)
attach(crime1)

# narr86 = ß0 + ß1*pcnv + ß2*avgsen + ß3*tottime + ß4*ptime86 + ß5*qemp86 + u

# narr86	número de vezes que o indivíduo foi preso.
# pcnv		proporção de prisões anteriores que levaram à condenação (proxy para a probabilidade de ser condenado por um crime)
# avgsen	sentença média das condenações passadas (medida da severidade dos crimes passados)
# tottime	tempo total que o indivíduo passou na prisão antes de 1986, desde que completou 18 anos.
# ptime86	meses de prisão em 1986 (captura os efeitos do encarceiramente em 1986 sobre o número de prisões).
# qemp86	número de trimestres em 1986 em que o indivíduo esteve formalmente empregado.


# Proporção dos homens que foram presos em 1986:
sum(as.integer(narr86==0))/length(narr86)	# Parcela que não foi presa em 1986 (narr86=0)
sum(as.integer(narr86>0))/length(narr86)		# Parcela que foi presa em 1986 (narr86>0)
sum(as.integer(narr86>1))/length(narr86)		# Parcela que foi presa mais de 1 vez em 1986 (narr86>1)


crime <- lm(narr86 ~ pcnv + avgsen + tottime + ptime86 + qemp86 )
summary(crime)

# 1. Diagnóstoco da Multicolinearidade: VIF

# Mede o quanto a variância do coeficiente estimado é inflacionado pela correlação com os outros regressores. 
# Regras de Bolso para VIF
# Se VIF~1 (Não existe inflação da variância); 
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


# Inferência Robusta:

coeftest(crime, vcovHC(crime, type = "HC3"))	# Diretamente, sem calcular a matriz Cov antes
coeftest(crime.2, vcovHC(crime.2, type = "HC3"))	# Diretamente, sem calcular a matriz Cov antes


waldtest(crime, .~. - avgsen - tottime, vcov = vcovHC)		# Utiliza HC3 como default


# Normalidade dos resíduos: 

ols_plot_resid_qq(crime)
ols_test_normality(crime)


##################################################################################################
# Example 2: Housing prices and air pollution
##################################################################################################

detach(crime1)
attach(hprice2)
head(hprice2)

# Amostra de 506 comunidades na área de Boston:

# log(price) = ß0 + ß1*log(nox) + ß2*log(dist) + ß3*rooms + ß4*stratio + u
#  ß1 e ß2: representam Elasticidades (Var%)/Var(%)
#  ß3 e ß4: representam Semi-Elasticidades (Var%)/Var (1unid) => 100*ß3 = Var(%) nos preços


# price: preco mediano das casas na comunidade;
# nox:	quantidade de óxido de nitrogênio no ar (partes por milhão);
# dist: distância "ponderada" da comunidade de 5 centros de trabalho (em milhas);
# rooms: número médio de quartos nas casas da comunidade; 
# stratio: é a razão (proporção) média  de Professor/Alunos das escolas da comunidade; 

h.price <- lm(log(price) ~ log(nox) + log(dist) + rooms + stratio )
summary(h.price)

# 1. Colinearidade:

ols_vif_tol(h.price)


# 2. Heterocedastidicade:

ols_test_breusch_pagan(h.price)			# Teste de Breusch-Pagan-Godfrey


# 3. Inferência Robusta:

coeftest(h.price, vcovHC(h.price, type = "HC3"))	# Diretamente, sem calcular a matriz Cov antes

              Estimate Std. Error  t value  Pr(>|t|)    
(Intercept) 11.0838610  0.3825081  28.9768 < 2.2e-16 ***
log(nox)    -0.9535385  0.1282244  -7.4365 4.516e-13 ***	+1% nox => -0.95% price
log(dist)   -0.1343394  0.0540771  -2.4842   0.01331 *  	+1% dist=> -0.13% price
rooms        0.2545271  0.0252019  10.0995 < 2.2e-16 ***	+1 room => +25.45% price
stratio     -0.0524511  0.0046592 -11.2576 < 2.2e-16 ***	+1 stratio => -5.24% price

# Obs: Correção da Semi_Elasticidade: (p.171)
# A medida que a variação em log(price) aumenta, a aproximação %Dy ~ 100*Dlog(y) 
# Correção: %Dy ~ 100*[(exp(ßj*Dxj)-1]


100*(exp(0.2545271)-1)
100*(exp(-0.0524511)-1)

# E = -1(?). Isto é, um aumento de 1% nox gera em média queda de 1% no preço médio das residências?

t0 <- (-0.95353852 + 1)/(0.1282244)
2*pt(abs(t0), df = 501, lower.tail=FALSE)	# H0:ß1=1 vs. H1:ß1<>1
pt(t0, df = 501, lower.tail=TRUE)				# H0:ß1=1 vs. H1:ß1<1

# Obs: P-Valores:

# Left-Tailed Tests:	P-value = pt(t0, df = n-1, lower.tail=TRUE)
# Right-Tailed Tests: P-value = pt(t0, df = n-1, lower.tail=FALSE)
# Two-Tailed Tests:	P-value = 2 * pt( abs(t0), df = n-1, lower.tail=FALSE)


# Normalidade dos resíduos:

ols_plot_resid_qq(h.price)
ols_test_normality(h.price)




