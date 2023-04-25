pacman::p_load(tidyverse,car)

tratamentos = c("a","b","c")
obs_1 = c(2,1,7)
obs_2 = c(2,0,9)
obs_3 = c(1,2,8)
obs_4 = c(1,1,10)
obs_5 = c(0,3,11)

dados = data.frame(tratamentos,obs_1,obs_2,obs_3,obs_4,obs_5) %>%
  mutate(tratamentos = as.factor(tratamentos))


#quais sao as hipoteses do teste
dados_aov = dados %>%
  pivot_longer(c(obs_1,obs_2,obs_3,obs_4,obs_5), values_to = "values", names_to = "var")


res_aov = aov(values ~ tratamentos, data = dados_aov)
res_aov %>% 
  summary()

shapiro.test(dados_aov$values)

pairwise.t.test(dados_aov$values,dados_aov$tratamentos)


# ANOVA
modelo <- aov(values ~ tratamentos, data = dados_aov)
summary(modelo)

# Teste de Tukey
TukeyHSD(modelo)

bartlett.test(dados_aov$values,dados_aov$tratamentos)


leveneTest(dados_aov$values,dados_aov$tratamentos)










