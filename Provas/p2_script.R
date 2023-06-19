pacman::p_load(tidyverse,dplyr,tidyr)


#montando o dataframe
estacao = c("e1","e2","e3","e4")
v1 = c(159,189,184,208)
v2 = c(196,178,216,137)
v3 = c(178,215,221,166)
v4 = c(225,189,147,198)

dados = data.frame(estacao,v1,v2,v3,v4)


#montando o dataframe
estacao = c("e1","e2","e3","e4")
v1 = c("t4",'t1','t2','t3')
v2 = c("t1",'t4','t3','t2')
v3 = c("t2",'t3','t1','t4')
v4 = c("t3",'t2','t4','t1')

dados_trat = data.frame(estacao,v1,v2,v3,v4)


dados_long = dados %>%
  pivot_longer(cols = c(v1,v2,v3,v4), values_to = "values", names_to = "vacas") %>%
  mutate(estacao = as.factor(estacao)) 

dados_long_trat = dados_trat %>%
  pivot_longer(cols = c(v1,v2,v3,v4), values_to = "tratamento", names_to = "vacas") %>%
  mutate(estacao = as.factor(estacao)) 


dados_padronizado = left_join(dados_long,dados_long_trat,by = join_by(vacas == vacas,estacao == estacao)) %>%
  mutate(estacao = as.factor(estacao),
         tratamento = as.factor(tratamento))



# anova no r para compara os valores

aov_res = aov(dados_padronizado$values ~ dados_padronizado$tratamento + dados_padronizado$vacas + dados_padronizado$estacao)

aov_res %>% summary()

#teste de tukey no R

TukeyHSD(aov_res)

boxplot(dados_padronizado$values ~ dados_padronizado$tratamento)

qmres = 312.3

dados_padronizado = dados_padronizado %>% 
  mutate(media_vaca = ave(values, vacas, FUN = mean) - mean(values),
         media_estacao = ave(values, estacao, FUN = mean) - mean(values),
         media_trat = ave(values, tratamento, FUN = mean) - mean(values),
         y_obs = mean(values) + media_vaca + media_estacao + media_trat,
         residuo = values - y_obs,
         residuo_normalizado = residuo/qmres)

mean(dados_padronizado$values)
#pressupostos

residuos = aov_res$residuals


#normalidade
shapiro.test(residuos)



#homocedasticidade
bartlett.test(residuos ~ dados_padronizado$tratamento)


#independencia

plot(residuos)


#aditividade

mod = lm(dados_padronizado$values ~ dados_padronizado$vacas + dados_padronizado$estacao + dados_padronizado$tratamento)

ad = (predict(mod))^2

admod = lm(dados_padronizado$values ~ dados_padronizado$vacas + dados_padronizado$estacao + dados_padronizado$tratamento + ad)

anova(mod,admod)

#--------------------------------------------------------------------------
#fatorial

#montando o dataframe
nitrogenio = c("nitro_baixa","nitro_baixa","nitro_media","nitro_media","nitro_alta","nitro_alta")
pot_baixa = c(53.3,52.1,54.5,55.4,59.1,61.7)
pot_media = c(54.0,55.2,57.3,59.9,60.2,62.2)
pot_alta = c(56.8,57.3,59.0,60.3,61.3,63.7)


dados = data.frame(nitrogenio,pot_baixa,pot_media,pot_alta)


dados_padronizado = dados %>%
  pivot_longer(cols = c(pot_baixa,pot_media,pot_alta), values_to = "values", names_to = "potassio") %>%
  mutate(nitrogenio = as.factor(nitrogenio),
         interacao = interaction(nitrogenio,potassio))



# anova no r para compara os valores

aov_res = aov(dados_padronizado$values ~ dados_padronizado$nitrogenio + dados_padronizado$potassio + dados_padronizado$interacao)

aov_res %>% summary()

#teste de tukey no R

TukeyHSD(aov_res)

boxplot(dados_padronizado$values ~ dados_padronizado$tratamento)


qmres = 1.61

dados_padronizado = dados_padronizado %>% 
  mutate(media_nitro = ave(values, nitrogenio, FUN = mean) - mean(values),
         media_pot = ave(values, potassio, FUN = mean) - mean(values),
         media_interacao = ave(values, interacao, FUN = mean) - ave(values, potassio, FUN = mean) - ave(values, nitrogenio, FUN = mean) + mean(values),
         y_obs = mean(values) + media_nitro + media_pot + media_interacao,
         residuo = values - y_obs,
         residuo_normalizado = residuo/qmres)

mean(dados_padronizado$values)
#pressupostos

residuos = aov_res$residuals
residuos
dados_padronizado$residuo

#normalidade
shapiro.test(residuos)



#homocedasticidade
bartlett.test(dados_padronizado$residuo ~ dados_padronizado$nitrogenio)
bartlett.test(dados_padronizado$residuo ~ dados_padronizado$potassio)
bartlett.test(dados_padronizado$residuo ~ dados_padronizado$interacao)

#independencia

plot(residuos)


#aditividade

mod = lm(dados_padronizado$values ~ dados_padronizado$nitrogenio + dados_padronizado$potassio + dados_padronizado$interacao)

ad = (predict(mod))^2

admod = lm(dados_padronizado$values ~ dados_padronizado$nitrogenio + dados_padronizado$potassio + dados_padronizado$interacao + ad)

anova(mod,admod)


# anova no r para compara os valores(potassio na dose alta)

aov_res = aov(dados_padronizado$values ~ dados_padronizado$nitrogenio + dados_padronizado$potassio + dados_padronizado$interacao)

aov_res %>% summary()

#teste de tukey no R

TukeyHSD(aov_res)


#parametros de nao centralidade


ncpnitro = 3*2*sum(c(-0.5,0,0.5)^2/qmres)


# Fator nitrogenio
alpha = 0.05
fcrit = qf(1-alpha,2,9)

beta = pf(q = fcrit,df1 = 2,df2 = 9, ncp = ncpnitro)

poder = 1 - beta
poder

# Fator B

fcrit = qf(1-alpha,glb,glr)

beta = pf(q = fcrit,df1 = glb,df2 = glr, ncp = ncpB)

poder = 1 - beta
poder

# Fator C

fcrit = qf(1-alpha,glab,glr)

beta = pf(q = fcrit,df1 = glab,df2 = glr, ncp = ncpAB)

poder = 1 - beta
poder





# anova no r para compara os valores(potassio na dose alta)

aov_res = aov(dados_padronizado$values ~ dados_padronizado$nitrogenio + dados_padronizado$potassio + dados_padronizado$nitrogenio*dados_padronizado$potassio)

aov_res %>% summary()

#teste de tukey no R

TukeyHSD(aov_res)
