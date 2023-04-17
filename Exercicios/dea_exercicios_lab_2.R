pacman::p_load(tidyverse,car)
# Exercicio 1



potencia = c(160,180,200,220)
obs_1 = c(575,565,600,725)
obs_2 = c(542,593,651,700)
obs_3 = c(530,590,610,715)
obs_4 = c(539,579,637,685)
obs_5 = c(570,610,629,710)
total = c(2756,2937,3127,3535)

dados = data.frame(potencia,obs_1,obs_2,obs_3,obs_4,obs_5,total)

dados_2 = dados %>%
  select(-7) %>%
  pivot_longer(cols = c(obs_1,obs_2,obs_3,obs_4,obs_5), values_to = "values", names_to = "nomes") %>%
  mutate(potencia = as.factor(potencia)) %>%
  select(-2)

#1.1 - Hipoteses de interesse 

#1.2 - Calcule a estatistica do teste e o p-valor usando so resultados encontrados na aula teorica e no software R


#manual
ssqtot = sum((c(obs_1,obs_2,obs_3,obs_4,obs_5) - (sum(total)/20))**2)

trat_medias = dados_2 %>%
  group_by(potencia) %>%
  summarise(media = mean(values))

ssqtrat = 5*sum(((trat_medias$media) - (sum(total)/20))**2)

ssqres = ssqtot-ssqtrat

a = 4 #tratamentos
n = 5 #repeticoes

test_stat = (ssqtrat/(a-1))/(ssqres/(a*n-a))

qf(0.05,(a-1),(a*n-a), lower.tail = FALSE)

valor_p = pf(test_stat,(a-1),(a*n-a),lower.tail = FALSE)

#Pelo R

dados_2 = dados %>%
  select(-7) %>%
  pivot_longer(cols = c(obs_1,obs_2,obs_3,obs_4,obs_5), values_to = "values", names_to = "nomes") %>%
  mutate(potencia = as.factor(potencia)) %>%
  select(-2)
#anova
res.aov <- aov(values ~ potencia, data = dados_2)
#sumário
summary(res.aov)

#1.3 - Os pressupostos necessarios foram atendidos
dados_residuo <- dados %>%
  mutate(mtrat = total/5) %>%
  pivot_longer(cols = c(obs_1,obs_2,obs_3,obs_4,obs_5),
               values_to = 'obs',names_to='rep') %>%
  select(1,3,5) %>%
  mutate(residuo = obs-mtrat)
  
#normalidade por gráfico
qqnorm(dados_residuo$residuo)
qqline(dados_residuo$residuo)


#normalidade
shapiro.test(dados_residuo$residuo)

#independencia por gráfico

plot(dados_residuo$residuo)

#variancias (homscedasticidade)

#testeF (variancia)


#teste de bartlett (parametrico)
bartlett.test(dados_residuo$residuo ~ dados_residuo$potencia)

#teste de levene



leveneTest(y = dados_residuo$obs, group = dados_residuo$potencia)

#1.4 - Qual sua conclusao sobre os resultados encontrados 

#1.5 - Qual a proporcao da variacao total explicada pelo modelo ajustado no item 1.2?


r_quadrado = ssqtrat/ssqtot

#1.6 -Se a hipotese nula for rejeita, quais potencias de radio frequencia diferem entre si? Apresente as hipoteses que serao testadas e a estatıstica do teste.


#teste de fisher (manual)
n = 5
N = 20
test_stat = (max(trat_medias$media) - min(trat_medias$media))/sqrt((ssqres/(a*n-a))*(1/n + 1/n))


qt((1-0.95)/2,N-1*sqrt((ssqres/(a*n-a))*(1/n + 1/n)))

pt(test_stat,N-1*sqrt((ssqres/(a*n-a))*(1/n + 1/n)),lower.tail = FALSE)

#teste de tukey (manual)





