pacman::p_load(tidyverse,dplyr,tidyr)


#montando o dataframe
marcas = c("A","B","C","D","E")
holandesa = c(122,125,120,150,153)
jersey = c(144,137,134,155,165)
girolando = c(145,144,136,156,171)


dados = data.frame(marcas,holandesa,jersey,girolando)

#padronizando o data frame para analise

dados_long = dados %>%
  pivot_longer(cols = c(holandesa,jersey,girolando), values_to = "values", names_to = "blocos") %>%
  mutate(marcas = as.factor(marcas)) 

boxplot(dados_long$values ~ dados_long$marcas)

# 1.1) O modelo considerado e as hipoteses de interesse

# 1.2) A tabela de análise de variancia e suas conclusoes

# quantidade de blocos e tratamentos
t = length(unique(dados_long$marcas)) #tratamentos
b = length(unique(dados_long$blocos)) #blocos
#soma de quadrados

media_total <- mean(dados_long$values)

ssqtot = sum((dados_long$values - media_total)^2)
ssqtrat   = sum((tapply(dados_long$values,dados_long$marcas,mean) - media_total)^2) * b
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

f_value = round(pf(f_obs,glt,glr,lower.tail = FALSE),5)

f_value_blocos = round(pf(f_obs_blocos,glb,glr,lower.tail = FALSE),6)


# Tabela da ANOVA
anova_table <- data.frame(Fonte_de_variacao = c("Marca", "Raça", "Residuos", "Total"),
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

trat_media = tapply(dados_long$values,dados_long$marcas,mean) - media_total

blocos_media = tapply(dados_long$values,dados_long$blocos,mean) - media_total


#calculando os residuos
dados_long = dados_long %>% 
  mutate(media_trat = ave(values, marcas, FUN = mean) - mean(values),
         media_bloco = ave(values, blocos, FUN = mean) - mean(values),
         y_obs = mean(values) + media_trat + media_bloco,
         residuo = values - y_obs,
         residuo_normalizado = residuo/qmres)

shapiro.test(dados_long$residuo)

bartlett.test(dados_long$residuo ~ dados_long$marcas)

# 1.4) Qual a proporção da variacao total explicada pelo modelo ajustado no item 1.2?

var_exp = 1 - (ssqres/ssqtot)

cat("A variancia explicada pelo medole é", var_exp)

var_exp = ((ssqtrat+ssqblocos)/ssqtot)

cat("A variancia explicada pelo medole é", var_exp)

# 1.5) Considerando que o objetivo do experimento e maximizar a variavel resposta, 
# qual e o elemento quımico que deve ser recomendado? Use teste de Tukey para subsidiar sua resposta.

alfa = 0.05

q.value <- qtukey(alfa, t, glr, lower.tail = F)

hsd = q.value * sqrt(qmres/b)

round(2*ptukey(5.3, t, glr, lower.tail = F),4)

combinacoes = combn(unique(dados$marcas),2)
comb_diff = combn(trat_media,2)

diferencas <- data.frame(
  comparacao = apply(combinacoes, 2, paste0, collapse = "-"),
  diferenca = apply(comb_diff, 2, diff)) %>%
  mutate(lwr = diferenca - q.value * sqrt(qmres/b),
         upr = diferenca + q.value * sqrt(qmres/b),
         pvalor = round(ptukey(abs(diferenca/(sqrt(qmres/b))), t, glr, lower.tail = F),6),
         hsd = abs(diferenca) >= hsd)

diferencas

# 1.6) Refaca as contas necessarias para responder os itens (1.2), (1.3) e (1.6) utilizando as funcoes
#aov e TukeyHSD e confira com os resultados obtidos.


# anova no r para compara os valores

aov_res = aov(dados_long$values ~ dados_long$marcas+dados_long$blocos)

aov_res %>% summary()

#teste de tukey no R

TukeyHSD(aov_res)

#teste de kruskall-wallis

kruskal.test(dados_long$values ~ dados_long$marcas)

# 1.7) Determine a probabilidade do erro tipo 2 para o caso de: (τ1 = −5, τ2 = 0, τ3 = 0, τ4 = 0, τ5 = 5).
alpha = 0.05
taui = c(-5,0,0,0,5)

sigma2 = qmres

delta = b*sum(taui^2/sigma2)

fcrit = qf(1-alpha,t-1,(t-1)*(b-1))

beta = pf(q = fcrit,df1 = t-1,df2 = (t-1)*(b-1), ncp = delta)

poder = 1 - beta
poder

# 1.8) Para os valores de taus considerados no item anterior, determine qual deve ser o numero de
#blocos para que o erro tipo 2 seja inferior a 10%?

b = 6

delta = b*sum(taui^2/sigma2)

fcrit = qf(1-alpha,t-1,(t-1)*(b-1))

beta = pf(q = fcrit,df1 = t-1,df2 = (t-1)*(b-1), ncp = delta)

poder = 1 - beta
poder



