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

dados_long = dados %>%
  pivot_longer(cols = c(obs_1,obs_2,obs_3,obs_4,obs_5), values_to = "values", names_to = "reps") %>%
  mutate(potencia = as.factor(potencia)) 

boxplot(dados_long$values ~ dados_long$potencia)

#1.1 - Hipoteses de interesse 

#1.2 - Calcule a estatistica do teste e o p-valor usando so resultados encontrados na aula teorica e no software R

# quantidade de blocos e tratamentos
t = length(unique(dados_long$potencia)) #tratamentos
r = length(unique(dados_long$reps)) #repeticoes
#soma de quadrados

media_total <- mean(dados_long$values)

ssqtot = sum((dados_long$values - media_total)^2)
ssqtrat   = sum((tapply(dados_long$values,dados_long$potencia,mean) - media_total)^2) * r
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
anova_table <- data.frame(Fonte_de_variacao = c("Potencia", "Residuos", "Total"),
                          GL = c(glt, glr, gltot),
                          SS = c(ssqtrat, ssqres, ssqtot),
                          MQ = c(round(qmtrat,2), round(qmres,2),''),
                          F = c(round(f_obs,3),'',''),
                          Pf = c(f_value, '', ""),
                          stringsAsFactors = FALSE)
rownames(anova_table) <- NULL

anova_table

#1.3 - Os pressupostos necessarios foram atendidos

#nossos estimadores
media_total = mean(dados_long$values)

trat_media = tapply(dados_long$values,dados_long$potencia,mean) - media_total

#calculando os residuos
dados_long = dados_long %>% 
  mutate(media_trat = ave(values, potencia, FUN = mean) - mean(values),
         y_obs = mean(values) + media_trat,
         residuo = values - y_obs,
         residuo_normalizado = residuo/qmres)

#normalidade
shapiro.test(dados_long$residuo)

#variancias (homscedasticidade)
bartlett.test(dados_long$residuo ~ dados_long$potencia)


#normalidade por gráfico
qqnorm(dados_long$residuo)
qqline(dados_long$residuo)


#independencia por gráfico
plot(dados_residuo$residuo)


#1.4 - Qual sua conclusao sobre os resultados encontrados 

#1.5 - Qual a proporcao da variacao total explicada pelo modelo ajustado no item 1.2?

R2 = 1 - (ssqres/ssqtot)

cat("A variancia explicada pelo medole é", R2)

R2 = ((ssqtrat+ssqblocos)/ssqtot)

cat("A variancia explicada pelo medole é", R2)

#1.6 -Se a hipotese nula for rejeita, quais potencias de radio frequencia diferem entre si? Apresente as hipoteses que serao testadas e a estatıstica do teste.

#teste de tukey (manual)
alfa = 0.05

q.value <- qtukey(alfa, t, glr, lower.tail = F)

hsd = q.value * sqrt(qmres/r)

combinacoes = combn(unique(dados$potencia),2)
comb_diff = combn(trat_media,2)

diferencas <- data.frame(
  comparacao = apply(combinacoes, 2, paste0, collapse = "-"),
  diferenca = apply(comb_diff, 2, diff)) %>%
  mutate(lwr = diferenca - q.value * sqrt(qmres/r),
         upr = diferenca + q.value * sqrt(qmres/r),
         pvalor = round(ptukey(abs(diferenca/(sqrt(qmres/r))), t, glr, lower.tail = F),6),
         hsd = abs(diferenca) >= hsd)

diferencas

#teste de fisher (manual)
alfa = 0.05
n = 5
N = 20
test_stat = (max(trat_medias$media) - min(trat_medias$media))/sqrt((ssqres/(a*n-a))*(1/n + 1/n))


qt((1-0.95)/2,N-1*sqrt((ssqres/(a*n-a))*(1/n + 1/n)))

pt(test_stat,N-1*sqrt((ssqres/(a*n-a))*(1/n + 1/n)),lower.tail = FALSE)

lsd = qt(alfa/2,N-a, lower.tail = FALSE)*sqrt((ssqres/(a*n-a))*(1/n + 1/n))

#contrastes

c1 =  c(1,1,-1,-1)

c2 =  c(1,-1,0,0)

c3 =  c(0,0,1,-1)



teste_stat_c1 = sum(c1*trat_medias$media)/(sum((ssqres/n)*(c1)^2))
teste_stat_c2 = sum(c2*trat_medias$media)/(sum((ssqres/n)*(c2)^2))
teste_stat_c3 = sum(c3*trat_medias$media)/(sum((ssqres/n)*(c3)^2))


#1.8) Calcule a probabilidade do erro tipo II para μ1 = 575, μ2 = 600, μ3 = 650, μ4 = 675 e σ = 25.
alpha = 0.05
sigma = 25
medias = c(575,600,650,675)
npc =  r*sum(medias/(sigma^2))

fcrit = qf(1-alpha,glt,glr)

beta = pf(q = fcrit,df1 = glt,df2 = glr, ncp = delta)

poder = 1 - beta
poder

#1.9) Qual deve ser o n ́umero de repeti ̧c ̃oes no experimento para que o erro seja menor que 0.01%?

r = 20
npc =  r*sum(medias/(sigma^2))

fcrit = qf(1-alpha,glt,(r*t)-t)

beta = pf(q = fcrit,df1 = glt,df2 = (r*t)-t, ncp = delta)

poder = 1 - beta
poder


