pacman::p_load(tidyverse)

reps = c(1,2,3,4,5)
tecnica_1 = c(7,8,5,9,10)
tecnica_2 = c(5,4,4,6,3)
tecnica_3 = c(17,9,10,12,13)

dados = data.frame(reps,tecnica_1,tecnica_2,tecnica_3) %>%
  mutate(reps = as.factor(reps))
dados


dados_long = dados %>%
  pivot_longer(cols = c(tecnica_1,tecnica_2,tecnica_3), values_to = "values", names_to = "tratamento") %>%
  mutate(tratamento = as.factor(tratamento),
         values = as.numeric(values))
dados_long

#boxplot

boxplot(values ~ tratamento, data = dados_long)
#residuos


dados_long = dados_long %>% 
  mutate(media_trat = ave(values, tratamento, FUN = mean) - mean(values),
         y_obs = mean(values) + media_trat,
         residuo = values - y_obs)


#hipotese de interesse



aov_res = aov(dados_long$values ~ dados_long$tratamento)
summary(aov_res)


residuos = aov_res$residuals

#normalidade

shapiro.test(residuos)

#igualdade de variancias

bartlett.test(dados_long$residuo ~ dados_long$tratamento)

#independencia

plot(residuos)

#estimando parametros

media = mean(dados_long$values)
media_trat  = tapply(dados_long$values,dados_long$tratamento,mean) - media

#contrastes
QMres = 4.90
n = 3
r = 5
c1 = c(1,-1,0)
c2 = c(0,1,-1)
c3 = c(1,0,-1)

media_trat = tapply(dados_long$values,dados_long$tratamento,mean)

teste_stat_c1 = sum(c1*media_trat)^2/((QMres/n)*sum(c1^2))

teste_stat_c2 = sum(c2*media_trat)^2/((QMres/n)*sum(c2^2))

teste_stat_c3 = sum(c3*media_trat)^2/((QMres/n)*sum(c3^2))

pf(teste_stat_c1,n-1,(n*r)-n,lower.tail = F)
pf(teste_stat_c2,n-1,(n*r)-n,lower.tail = F)
pf(teste_stat_c3,n-1,(n*r)-n,lower.tail = F)

#exercicios 2 --------------------------------------------------------------------

especie = c(1,2,3,4)
norte = c(105.17,88.42,100.78,102.09)
nordeste = c(102.21,89.36,99.26,99.45)
centro_oeste = c(99.43,90.16,96.77,102.63)
sudeste = c(107.74,92.3,102.50,107.63)
sul = c(106.2,91.5,104.1,105.9)


dados = data.frame(especie,norte,nordeste,centro_oeste,sudeste,sul) %>%
  mutate(especie = as.factor(especie))
dados


dados_long = dados %>%
  pivot_longer(cols = c(norte,nordeste,centro_oeste,sudeste,sul), values_to = "values", names_to = "bloco") %>%
  mutate(bloco = as.factor(bloco),
         values = as.numeric(values))
dados_long

#boxplot

boxplot(values ~ especie, data = dados_long)
#residuos

dados_long = dados_long %>% 
  mutate(media_trat = ave(values, especie, FUN = mean) - mean(values),
         media_bloco = ave(values, bloco, FUN = mean) - mean(values),
         y_obs = mean(values) + media_trat+ media_bloco,
         residuo = values - y_obs)
#anova
aov_res = aov(dados_long$values ~ dados_long$especie + dados_long$bloco)
summary(aov_res)


residuos = aov_res$residuals
dados_long$residuo
#normalidade

shapiro.test(residuos)

#igualdade de variancias

bartlett.test(dados_long$residuo ~ dados_long$especie)

#independencia

plot(residuos)

mod = lm(values~especie+bloco,dados_long)

ad = predict(mod)

admod = lm(values~especie+bloco+ad,dados_long)

aov_mod = aov(mod,admod)

#tukey

TukeyHSD(aov_res)

#erro tipo 2 
alfa = 0.05
sigma2 = 2.72
b = 5
taui = c(0,-2,0,2)


delta = b*sum(taui^2/sigma2)

fcrit = qf(1-alfa,3,(4-1)*(b-1))

beta = pf(fcrit,3,(4-1)*(b-1), ncp = delta)

1 - beta

#diminuindo o erro
b = 7

delta = b*sum(taui^2/sigma2)

fcrit = qf(1-alfa,t-1,(4-1)*(b-1))

beta = pf(q = fcrit,df1 = 4-1,df2 = (4-1)*(b-1), ncp = delta)

poder = 1 - beta
poder

