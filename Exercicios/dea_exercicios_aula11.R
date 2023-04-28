pacman::p_load(tidyverse)


produtos = c(1,2,3,4)
rolo_1 = c(73,73,75,73)
rolo_2 = c(68,67,68,71)
rolo_3 = c(74,75,78,75)
rolo_4 = c(71,72,73,75)
rolo_5 = c(67,70,68,69)


dados = data.frame(produtos,rolo_1,rolo_2,rolo_3,rolo_4,rolo_5)


dados_long = dados %>%
  pivot_longer(cols = c(rolo_1,rolo_2,rolo_3,rolo_4,rolo_5), values_to = "values", names_to = "blocos") %>%
  mutate(produtos = as.factor(produtos)) 


#soma de quadrados

media_total <- mean(dados_long$values)

ssqtot = sum((dados_long$values - media_total)^2)
ssqtrat   = sum((tapply(dados_long$values,dados_long$produtos,mean) - media_total)^2) * length(unique(dados_long$blocos))
ssqblocos = sum((tapply(dados_long$values,dados_long$blocos,mean) - media_total)^2) * length(unique(dados_long$produtos))
ssqres = ssqtot-ssqtrat-ssqblocos

#quadrados medios

qmtrat = ssqtrat/(length(unique(dados_long$blocos))-1)
qmbloco = ssqblocos/(length(unique(dados_long$produtos))-1)
# 1.1) O modleo considerado e as hipoteses de interesse

# 1.2) A tabela de análise de variancia e suas conclusoes

# 1.3) Os pressupostos necessarios foram atendidos ?

# anova no r para compara os valores

aov_res = aov(dados_long$values ~ dados_long$produtos+dados_long$blocos)
aov_res %>% summary()




















