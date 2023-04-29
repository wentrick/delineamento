pacman::p_load(tidyverse)

#montando o dataframe
produtos = c(1,2,3,4)
rolo_1 = c(73,73,75,73)
rolo_2 = c(68,67,68,71)
rolo_3 = c(74,75,78,75)
rolo_4 = c(71,72,73,75)
rolo_5 = c(67,70,68,69)

dados = data.frame(produtos,rolo_1,rolo_2,rolo_3,rolo_4,rolo_5)

#padronizando o data frame para analise

dados_long = dados %>%
  pivot_longer(cols = c(rolo_1,rolo_2,rolo_3,rolo_4,rolo_5), values_to = "values", names_to = "blocos") %>%
  mutate(produtos = as.factor(produtos)) 

# 1.1) O modelo considerado e as hipoteses de interesse

# 1.2) A tabela de análise de variancia e suas conclusoes

# quantidade de blocos e tratamentos
t = length(unique(dados_long$produtos)) #tratamentos
b = length(unique(dados_long$blocos)) #blocos
#soma de quadrados

media_total <- mean(dados_long$values)

ssqtot = sum((dados_long$values - media_total)^2)
ssqtrat   = sum((tapply(dados_long$values,dados_long$produtos,mean) - media_total)^2) * b
ssqblocos = sum((tapply(dados_long$values,dados_long$blocos,mean) - media_total)^2) * t
ssqres = ssqtot-ssqtrat-ssqblocos

#graus de liberdade

glb = (b-1)
glt = (t-1)
glr = glb*glt
gltot = (b*t)-1
#quadrados medios

qmtrat = ssqtrat/glt
qmbloco = ssqblocos/glb
qmres = ssqres/glr

# valor F observado

f_obs = qmtrat/qmres
f_obs_blocos = qmbloco/qmres

#valor critico de 5%
alfa = 0.05
f_crit = qf(alfa,glt,glr)
f_crit_bloco = qf(alfa,glb,glr)

#p-valor observado

f_value = round(pf(f_obs,glt,glr,lower.tail = FALSE),7)

f_value_blocos = round(pf(f_obs_blocos,glb,glr,lower.tail = FALSE),7)


# Tabela da ANOVA
anova_table <- data.frame(Fonte_de_variacao = c("Produto", "Rolo de tecido", "Residuos", "Total"),
                          SS = c(ssqtrat, ssqblocos, ssqres, ssqtot),
                          GL = c(glt, glb, glr, gltot),
                          MQ = c(qmtrat, qmbloco, qmres, NA),
                          F = c(f_value, f_value_blocos, NA, NA),
                          stringsAsFactors = FALSE)
rownames(anova_table) <- NULL

anova_table
# 1.3) Os pressupostos necessarios foram atendidos ?

shapiro.test(dados_long$values)

bartlett.test(dados_long$values ~ dados_long$produtos)


# anova no r para compara os valores

aov_res = aov(dados_long$values ~ dados_long$produtos+dados_long$blocos)
aov_res %>% summary()














