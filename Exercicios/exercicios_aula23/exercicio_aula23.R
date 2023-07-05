pacman::p_load(tidyverse,dplyr,tidyr)
#montando o dataframe
fator_A = rep(c('A baixo','A alto'),2)
fator_B = rep(c('B baixo','B alto'),each = 2)
rep_1 = c(28,36,18,31)
rep_2 = c(25,32,19,30)
rep_3 = c(27,32,23,29)

dados = data.frame(fator_A,fator_B,rep_1,rep_2,rep_3) 


#padronizando o data frame para analise

dados_padronizado = dados %>%
  pivot_longer(cols = c(rep_1,rep_2,rep_3), values_to = "values", names_to = "rep") %>%
  mutate(fator_A = as.factor(fator_A),
         fator_B = as.factor(fator_B),
         tratamento = interaction(fator_A,fator_B))  #nosso tratamento vai ser a interaçcao entre A e B

#1.1) O modelo considerado e as hipoteses de interesse.

#1.2) A tabela de analise de variancia e suas conclusoes.
# quantidade de linhas, colunas e tratamentos sao todas iguais no caso é 5
a = length(unique(dados_padronizado$material)) #fator A
b = length(unique(dados_padronizado$temperatura)) #fator B
rep = 4

N <- length(dados_padronizado$values) # total sample size
n <- length(dados_padronizado$values) / a # number of samples per group (since sizes are equal)

#soma de quadrados
media_total <- mean(dados_padronizado$values)

ssqtot = sum((dados_padronizado$values - media_total)^2)
ssqA = sum((tapply(dados_padronizado$values,dados_padronizado$material,mean) - media_total)^2)*b*rep
ssqB = sum((tapply(dados_padronizado$values,dados_padronizado$temperatura,mean) - media_total)^2)*a*rep
ssqsub = sum((tapply(dados_padronizado$values,dados_padronizado$interacao,mean) - media_total)^2)*rep
ssqAB = ssqsub - (ssqA + ssqB)
ssqres = ssqtot-(ssqA+ssqB+ssqAB)

















 