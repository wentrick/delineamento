pacman::p_load(tidyverse)
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

valor_p = pf()
#auto

dados_2 = dados %>%
  select(-7) %>%
  pivot_longer(cols = c(obs_1,obs_2,obs_3,obs_4,obs_5), values_to = "values", names_to = "nomes") %>%
  mutate(potencia = as.factor(potencia)) %>%
  select(-2)

res.aov <- aov(values ~ potencia, data = dados_2)
# Summary of the analysis
summary(res.aov)

#1.3 - Os pressupostos necessarios foram atendidos
dados_residuos = dados_2 %>%
  pivot_wider(cols = potencia, values_from = values, names_from = potencia, names_prefix = "pot", names_sep = "_")
  

residuos = c(c(obs_1 - trat_medias$media[1]),c(obs_2 - trat_medias$media[2]),c(obs_3 - trat_medias$media[3]),c(obs_4 - trat_medias$media[4]),c(obs_5 - trat_medias$media[5]))
#normalidade
shapiro.test(dados_2$values)

#variancias (homscedasticidade)

bartlett.test(dados_2$values ~ dados_2$potencia)

#1.4 - Qual sua conclusao sobre os resultados encontrados 


