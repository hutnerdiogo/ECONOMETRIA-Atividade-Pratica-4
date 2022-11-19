library(AER)
#install.packages('strucchange')
library(strucchange)
library(fBasics)
library(quantreg)
library(quantmod)
library(stargazer)
#install.packages("tseries")
library(tseries)
#install.packages("plm")
library(plm)

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
#Ponto critico = 0.0453878 - Quantidade de
logit$prob <- predict(modellogit,data=logit,type="response")
#### Questão 6 ####
#'6.	Com base nos coeficientes da regressão, estime, para cada membro do grupo,
#' a probabilidade dele ser um potencial investidor de criptomoedas. Em seguida, ,
#' considerando o ponto de corte da questão 5, sinalizem se o modelo acertou ou errou a previsão.
#- Só colocar Idade, Dummy Mulhe, Banco Digital, Influencers
coef_significantes <- c(modellogit$coefficients[2],modellogit$coefficients[3],
                        modellogit$coefficients[4],modellogit$coefficients[8])

vetorDiogo <- c(20,0,1,1)
(probDiogo <- exp(sum(t(vetorDiogo) * coef_significantes)) / (1+exp(sum(t(vetorDiogo) * coef_significantes))))
vetorDiogo <- c(vetorDiogo,probDiogo)

vetorLeticia <- c(19,1,1,1)
(probLeticia <- exp(sum(t(vetorLeticia) * coef_significantes)) / (1+exp(sum(t(vetorLeticia) * coef_significantes))))
vetorLeticia <- c(vetorLeticia,probLeticia)


vetorJoana <- c(19,1,1,1)
(probJoana <- exp(sum(t(vetorJoana) * coef_significantes)) / (1+exp(sum(t(vetorJoana) * coef_significantes))))
vetorJoana <- c(vetorJoana, probJoana)

vetorLuisa <- c(19,1,1,1)
probLuisa <- exp(sum(t(vetorLuisa) * coef_significantes)) / (1+exp(sum(t(vetorLuisa) * coef_significantes)))
vetorLuisa <- c(vetorLuisa, probLuisa)
# investem :
ponto_critico <- 0.0453878
dados <- matrix(data=rbind(vetorDiogo,vetorLeticia,vetorLuisa, vetorJoana),nrow=4)
rownames(dados) <- c("Diogo","Leticia","Luisa","Joana")
colnames(dados) <- c("Idade","Mulher","Banco Digital","Influencers","Probabilidade")

dados <- cbind(dados, as.numeric(dados[,'Probabilidade'] > ponto_critico))
colnames(dados) <- c(colnames(dados)[-6],"Previsao")

dados <- cbind(dados, c(1,0,0,0))
colnames(dados) <- c(colnames(dados)[-7],"Realidade")

dados <- cbind(dados, dados[,'Previsao'] == dados[,'Realidade'])
colnames(dados) <- c(colnames(dados)[-8],"Acertou")
stargazer(dados,type="text")
results <- matrix(rbind(c(0,.75),c(0,.25)),nrow=2)
colnames(results) <- c("Falso","Verdadeiro")
rownames(results) <- c("Falso","Verdadeiro")

#### Parte 2 - Dados Painel ####
#### Questão 7 ####
#7.	Baixe os dados (BaseDP.csv) e em seguida, declare a base de dados como painel.
DP <- read.table("Dados/BaseDP.csv",sep=';',dec='.',header = TRUE)

