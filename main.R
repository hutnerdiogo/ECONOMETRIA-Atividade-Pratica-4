library(AER)
#install.packages('strucchange')
library(strucchange)
library(fBasics)
library(quantreg)
library(quantmod)
library(stargazer)
#install.packages("tseries")
library(tseries)


DP <- read.table("Dados/BaseDP.csv",sep=';',dec='.',header = TRUE)

##### Parte 1 - Logit #####

#### Questão 1 ####
#'1.Baixe os dados (BaseLogit.csv) e em seguida, calcule a proporção e a
#'probabilidade de secionar um investidor de criptomoedas aleatoriamente na base
logit <- read.table("Dados/BaseLogit.csv",sep=";",dec=".",header=TRUE)

cont_cripto <- as.matrix(as.data.frame(table(logit[,"CRIPTO"]))[,-1])
rownames(cont_cripto) <- c("Nao tem cripto","Tem Cripto")
total_amostral <- colSums(cont_cripto)
cont_cripto <- cbind(cont_cripto,cont_cripto/total_amostral)
colnames(cont_cripto) <- c("Absoluto","Porcentagem")
proporcao <- cont_cripto['Tem Cripto','Absoluto'] / cont_cripto['Nao tem cripto','Absoluto']
proporcao
cont_cripto
#' Segundo o espaço amostral, a chance de escolher aleatoriamente uma pessoa e ela fazer parte
#' do mercado de criptomoedas é de 4,53%

#### Questão 2 ####
regressao_logit <- lm(CRIPTO ~ IDADE + MULHER + BANCODIGITAL + LEITURA + ECON2022
  + INFLUENCERS + ENSINOSUP + RENDFAM,data = logit)
summary(regressao_logit)
# Analisar coeficiente
# R2 ajustado
# VIF
vif(regressao_logit)

#### Questao 3 ####
#'3.Estime o modelo anterior utilizando a metodologia LOGIT. Analise os coeficientes do modelo e
#' compare os resultados com os do modelo MQO.
modellogit <-  glm(CRIPTO ~ IDADE + MULHER + BANCODIGITAL + LEITURA + ECON2022
  + INFLUENCERS + ENSINOSUP + RENDFAM,data = logit,family=binomial(link="logit"))
summary(modellogit)
#### Questão 4 ####
#' 4.Analise o odds-ratio dos coeficientes e os efeitos marginais do modelo.
exp(modellogit$coefficients)

LogitScalar<-mean(dlogis(predict(modellogit,type="link")))
LogitScalar*coef(modellogit)
#### Questao 5 ####
#'5.Analise os pseudo-R2 do modelo via metodologia de McFadden, r2ML e r2CU.
#' Em seguida, analise a acurácia da modelagem, tomando como ponto crítico,
#' a probabilidade de sortear um individuo que investe em criptomoedas na amostra.
install.packages("pscl")
library(pscl)
pR2(modellogit)

classif <- table(fitted(modellogit)>0.0453878,logit$CRIPTO)/sum(table(fitted(modellogit)>0.0453878,logit$CRIPTO))
classif
(acuracia <- sum(diag(classif)))
#Ponto critico = 0.0453878
logit$prob <- predict(modellogit,data=logit,type="response")
