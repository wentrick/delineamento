#lista de exercicios delineamento 2023/1 - Davi Wentrick Feijó - 200016806
pacman::p_load(tidyverse,car)

tratamentos = c("a","b","c")
obs_1 = c(2,1,7)
obs_2 = c(2,0,9)
obs_3 = c(1,2,8)
obs_4 = c(1,1,10)
obs_5 = c(0,3,11)

dados = data.frame(tratamentos,obs_1,obs_2,obs_3,obs_4,obs_5) %>%
  mutate(tratamentos = as.factor(tratamentos))

#padronizando o data frame para analise

dados_long = dados %>%
  pivot_longer(cols = c(obs_1,obs_2,obs_3,obs_4,obs_5), values_to = "values", names_to = "reps") %>%
  mutate(tratamentos = as.factor(tratamentos)) 

boxplot(dados_long$values ~ dados_long$tratamentos)

#1.1) Quais sao as hípoteses de interesse?

#1.2) Calcule a estatıstica do Tese e o p-valor usando os resultados encontrados na aula teorica e usando o software R.

# quantidade de blocos e tratamentos
t = length(unique(dados_long$tratamentos)) #tratamentos
r = length(unique(dados_long$reps)) #repeticoes
#soma de quadrados

media_total <- mean(dados_long$values)

ssqtot = sum((dados_long$values - media_total)^2)
ssqtrat   = sum((tapply(dados_long$values,dados_long$tratamentos,mean) - media_total)^2) * r
ssqblocos = sum((tapply(dados_long$values,dados_long$reps,mean) - media_total)^2) * t
ssqres = ssqtot-ssqtrat-ssqblocos

#graus de liberdade


glt = (t-1)
glr = (r*t)-t
gltot = (r*t)-1
#quadrados medios

qmtrat = ssqtrat/glt
qmres = ssqres/glr

# valor F observado

f_obs = qmtrat/qmres

#valor critico de 5%
alfa = 0.05
f_crit = qf(alfa,glt,glr)

#p-valor observado

f_value = round(pf(f_obs,glt,glr,lower.tail = FALSE),11)



# Tabela da ANOVA
anova_table <- data.frame(Fonte_de_variacao = c("tratamentos", "Residuos", "Total"),
                          GL = c(glt, glr, gltot),
                          SS = c(ssqtrat, ssqres, ssqtot),
                          MQ = c(round(qmtrat,2), round(qmres,2),''),
                          F = c(round(f_obs,3),'',''),
                          Pf = c(f_value, '', ""),
                          stringsAsFactors = FALSE)
rownames(anova_table) <- NULL

anova_table


#1.3) Os pressupostos necess ́arios foram atendidos?


#nossos estimadores
media_total = mean(dados_long$values)

trat_media = tapply(dados_long$values,dados_long$tratamentos,mean) - media_total

#calculando os residuos
dados_long = dados_long %>% 
  mutate(media_trat = ave(values, tratamentos, FUN = mean) - mean(values),
         y_obs = mean(values) + media_trat,
         residuo = values - y_obs,
         residuo_normalizado = residuo/qmres)

#normalidade
shapiro.test(dados_long$residuo)

#variancias (homscedasticidade)
bartlett.test(dados_long$residuo ~ dados_long$tratamentos)


#normalidade por gráfico
qqnorm(dados_long$residuo)
qqline(dados_long$residuo)


#independencia por gráfico
plot(dados_residuo$residuo)

#1.4) Qual sua conclusao sobre os resultados encontrados?

#1.5) Qual a proporcao da variacao total explicada pelo modelo ajustado no item 1.2?

R2 = 1 - (ssqres/ssqtot)

cat("A variancia explicada pelo medole é", R2)

R2 = ((ssqtrat+ssqblocos)/ssqtot)

cat("A variancia explicada pelo medole é", R2)

#1.6) Se a hipotese nula for rejeita, quais cultivares diferem entre si? Apresente as hipoteses que serao testadas e a estatıstica do teste.


#teste de tukey (manual)
alfa = 0.05

q.value <- qtukey(alfa, t, glr, lower.tail = F)

hsd = q.value * sqrt(qmres/r)

combinacoes = combn(unique(dados$tratamentos),2)
comb_diff = combn(trat_media,2)

diferencas <- data.frame(
  comparacao = apply(combinacoes, 2, paste0, collapse = "-"),
  diferenca = apply(comb_diff, 2, diff)) %>%
  mutate(lwr = diferenca - q.value * sqrt(qmres/r),
         upr = diferenca + q.value * sqrt(qmres/r),
         pvalor = round(ptukey(abs(diferenca/(sqrt(qmres/r))), t, glr, lower.tail = F),6),
         hsd = abs(diferenca) >= hsd)

diferencas

