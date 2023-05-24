#lista de exercicios delineamento 2023/1 - Davi Wentrick Feijó - 200016806
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


#padronizando o data frame para analise
dados_long = dados %>%
  pivot_longer(cols = c(obs_1,obs_2,obs_3,obs_4,obs_5), values_to = "values", names_to = "repeticoes") %>%
  mutate(potencia = as.factor(potencia)) 

#1.1 - Hipoteses de interesse 

#1.2 - Calcule a estatistica do teste e o p-valor usando so resultados encontrados na aula teorica e no software R


#manual

# quantidade de tratamentos e repeticoes

a = length(unique(dados_long$potencia)) #tratamentos
n = length(unique(dados_long$repeticoes)) #tratamentos

#soma de quadrados
media_total = mean(dados_long$values)

ssqtot = sum((dados_long$values - media_total)^2)
ssqtrat   = sum((tapply(dados_long$values,dados_long$potencia,mean) - media_total)^2) * n
ssqres = ssqtot-ssqtrat

#graus de liberdade

glt = (a-1)
glr = (a*n-a)
gltot = (a*n)-1
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
                          SS = c(ssqtrat, ssqres, ssqtot),
                          GL = c(glt, glr, gltot),
                          MQ = c(qmtrat,qmres, NA),
                          F = c(f_value, NA, NA),
                          stringsAsFactors = FALSE)
rownames(anova_table) <- NULL

anova_table

#Pelo R

#anova
res.aov <- aov(values ~ potencia, data = dados_long)
#sumário
summary(res.aov)
TukeyHSD(res.aov)
#1.3 - Os pressupostos necessarios foram atendidos
dados_residuo <- dados %>%
  mutate(mtrat = total/5) %>%
  pivot_longer(cols = c(obs_1,obs_2,obs_3,obs_4,obs_5),
               values_to = 'obs',names_to='rep') %>%
  select(1,3,5) %>%
  mutate(residuo = obs-mtrat)
  

#normalidade por gráfico
residuos = unlist(tapply(dados_long$values,dados_long$potencia,function(x) x - mean(x))) 
sum(residuos)

qqnorm(residuos)
qqline(residuos)


#normalidade
shapiro.test(residuos)

#independencia por gráfico

plot(residuos)

#variancias (homscedasticidade)


#testeF (variancia)


#teste de bartlett (parametrico)
bartlett.test(residuos ~ dados_residuo$potencia)

#teste de levene



leveneTest(y = dados_long$values, group = dados_long$potencia)

#1.4 - Qual sua conclusao sobre os resultados encontrados 

#1.5 - Qual a proporcao da variacao total explicada pelo modelo ajustado no item 1.2?


r_quadrado = ssqtrat/ssqtot

#1.6 -Se a hipotese nula for rejeita, quais potencias de radio frequencia diferem entre si? Apresente as hipoteses que serao testadas e a estatıstica do teste.


# teste de fisher (manual) camos usar o LSD ( que consiste me um valor qeu representa a regiao critica,
# e podemos comparar ele diretamente com a diferenca de medias e caso seja maior existe uma diferenca significativa)
alfa = 0.05
n = 5
N = 20
lsd = qt(alfa/2,N-a, lower.tail = FALSE)*sqrt((ssqres/(a*n-a))*(1/n + 1/n))

#calculando todos os pares de medias 
combinations = combn(tapply(dados_long$values,dados_long$potencia,mean), 2)
# Cálculo das diferenças entre cada par de médias
differences <- combinations[1,] - combinations[2,]

# Exibição das diferenças
abs(differences) > lsd #todosmos tratamentos tem diferencas entre si


#teste de tukey (manual)


#contrastes

c1 =  c(1,1,-1,-1)

c2 =  c(1,-1,0,0)

c3 =  c(0,0,1,-1)



teste_stat_c1 = sum(c1*trat_medias$media)/(sum((ssqres/n)*(c1)^2))
teste_stat_c2 = sum(c2*trat_medias$media)/(sum((ssqres/n)*(c2)^2))
teste_stat_c3 = sum(c3*trat_medias$media)/(sum((ssqres/n)*(c3)^2))


## erro tipo 2

n = 5
sigma = 25
medias = c(575,600,650,675)
media_medias = mean(medias)
npc =  n*ssqtrat/(sigma^2)

ssqtrat

