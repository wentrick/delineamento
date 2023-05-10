pacman::p_load(tidyverse,dplyr,tidyr)


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

boxplot(dados_long$values ~ dados_long$produtos)

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

f_value = round(pf(f_obs,glt,glr,lower.tail = FALSE),3)

f_value_blocos = round(pf(f_obs_blocos,glb,glr,lower.tail = FALSE),7)


# Tabela da ANOVA
anova_table <- data.frame(Fonte_de_variacao = c("Produto", "Rolo de tecido", "Residuos", "Total"),
                          GL = c(glt, glb, glr, gltot),
                          SS = c(ssqtrat, ssqblocos, ssqres, ssqtot),
                          MQ = c(round(qmtrat,2), round(qmbloco,2), round(qmres,2), ''),
                          F = c(round(f_obs,3),round(f_obs_blocos,3),'',''),
                          Pf = c(f_value, f_value_blocos, "", ''),
                          stringsAsFactors = FALSE)
rownames(anova_table) <- NULL

anova_table
# 1.3) Os pressupostos necessarios foram atendidos ?

#nossos estimadores
media_total = mean(dados_long$values)

trat_media = tapply(dados_long$values,dados_long$produtos,mean) - media_total

blocos_media = tapply(dados_long$values,dados_long$blocos,mean) - media_total


#calculando os residuos
dados_long = dados_long %>% 
  mutate(media_trat = ave(values, produtos, FUN = mean) - mean(values),
         media_bloco = ave(values, blocos, FUN = mean) - mean(values),
         y_obs = mean(values) + media_trat + media_bloco,
         residuo = values - y_obs,
         residuo_normalizado = residuo/qmres)

shapiro.test(dados_long$residuo)

bartlett.test(dados_long$residuo ~ dados_long$produtos)

#pressuposto de modelo aditivo

mod = lm(dados_long$values ~ dados_long$produtos + dados_long$blocos)

ad = (predict(mod))^2

admod = lm(dados_long$values ~ dados_long$produtos + dados_long$blocos + ad)

anova(mod,admod)
# 1.4) Qual a proporção da variacao total explicada pelo modelo ajustado no item 1.2?

R2 = 1 - (ssqres/ssqtot)

cat("A variancia explicada pelo medole é", R2)

R2 = ((ssqtrat+ssqblocos)/ssqtot)

cat("A variancia explicada pelo medole é", R2)


# 1.5) Considerando que o objetivo do experimento ´e m´aximizar a vari´avel resposta, 
# qual ´e o elemento quımico que deve ser recomendado? Use teste de Tukey para subsidiar sua resposta.

alfa = 0.05

q.value <- qtukey(alfa, t, glr, lower.tail = F)

hsd = q.value * sqrt(qmres/b)

round(2*ptukey(5.3, t, glr, lower.tail = F),4)

combinacoes = combn(unique(dados$produtos),2)
comb_diff = combn(trat_media,2)

diferencas <- data.frame(
  comparacao = apply(combinacoes, 2, paste0, collapse = "-"),
  diferenca = apply(comb_diff, 2, diff)) %>%
  mutate(lwr = diferenca - q.value * sqrt(qmres/b),
         upr = diferenca + q.value * sqrt(qmres/b),
         pvalor = round(ptukey(abs(diferenca/(sqrt(qmres/b))), t, glr, lower.tail = F),6),
         hsd = abs(diferenca) >= hsd)

diferencas



# 1.6) Refa¸ca as contas necessarias para responder os itens (1.2), (1.3) e (1.6) utilizando as funcoes
#aov e TukeyHSD e confira com os resultados obtidos.


# anova no r para compara os valores

aov_res = aov(dados_long$values ~ dados_long$produtos+dados_long$blocos)

aov_res %>% summary()

#teste de tukey no R

TukeyHSD(aov_res)

#teste de kruskall-wallis

kruskal.test(dados_long$values ~ dados_long$produtos)

# 1.7) Determine a probabilidade do erro tipo 2 para o caso de: (τ1 = −1.5, τ2 = 0, τ3 = 0, τ4 = 1.5).
alpha = 0.05
taui = c(-1.5,0,0,1.5)

sigma2 = qmres

delta = b*sum(taui^2/sigma2)

fcrit = qf(1-alpha,t-1,(t-1)*(b-1))

beta = pf(q = fcrit,df1 = t-1,df2 = (t-1)*(b-1), ncp = delta)

poder = 1 - beta
poder

# 1.8) Para os valores de taus considerados no item anterior, determine qual deve ser o n´umero de
#blocos para que o erro tipo 2 seja inferior a 10%?

b = 8

delta = b*sum(taui^2/sigma2)

fcrit = qf(1-alpha,t-1,(t-1)*(b-1))

beta = pf(q = fcrit,df1 = t-1,df2 = (t-1)*(b-1), ncp = delta)

poder = 1 - beta
poder

