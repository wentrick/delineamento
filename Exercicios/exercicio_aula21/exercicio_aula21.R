pacman::p_load(tidyverse,dplyr,tidyr)


#montando o dataframe
operators = rep(c(1,2,3,4),each=2)
filter_type = rep(c(1,2),4)
low = c(90,86,96,84,100,92,92,81)
medium = c(102,87,106,90,105,97,96,80)
high = c(114,93,112,91,108,95,98,83)

dados = data.frame(operators,filter_type,low,medium,high)

#padronizando o data frame para analise

dados_padronizado = dados %>%
  pivot_longer(cols = c(low,medium,high), values_to = "values", names_to = "nivel") %>%
  mutate(material = as.factor(nivel),
         interacao = interaction(nivel,filter_type)) 

#1.1) O modelo considerado e as hipoteses de interesse.

#1.2) A tabela de analise de variancia e suas conclusoes.
# quantidade de linhas, colunas e tratamentos sao todas iguais no caso é 5
t = length(unique(dados_padronizado$nivel)) #fator A
b = length(unique(dados_padronizado$operators)) #blocos
f = length(unique(dados_padronizado$filter_type)) # Fator b
N <- t*b*f # total sample size
n <- length(dados_padronizado$values)/t # number of samples per group (since sizes are equal)
rep = b


#soma de quadrados
media_total <- mean(dados_padronizado$values)

ssqtot = sum((dados_padronizado$values - media_total)^2)
ssqA = sum((tapply(dados_padronizado$values,dados_padronizado$material,mean) - media_total)^2)*b*rep
ssqB = sum((tapply(dados_padronizado$values,dados_padronizado$temperatura,mean) - media_total)^2)*a*rep
ssqsub = sum((tapply(dados_padronizado$values,dados_padronizado$interacao,mean) - media_total)^2)*rep
ssqAB = ssqsub - (ssqA + ssqB)
ssqres = ssqtot-(ssqA+ssqB+ssqAB)


#graus de liberdade

gla = (a-1)
glb = (b-1)
glbloco = (n-1)
glab = (a-1)*(b-1)
gltot = a*b*rep - 1
glr = a*b*(rep-1)
#quadrados medios

qmA = ssqA/gla
qmB = ssqB/glb
qmAB = ssqAB/glab
qmres = ssqres/glr

# valor F observado

f_obs_A = qmA/qmres
f_obs_B = qmB/qmres
f_obs_AB = qmAB/qmres

#valor critico de 5%
alfa = 0.05
f_crit_A = qf(alfa,gla,glr)
f_crit_B = qf(alfa,glb,glr)
f_crit_AB = qf(alfa,glab,glr)
#p-valor observado

f_value_A = round(pf(f_obs_A,gla,glr,lower.tail = FALSE),3)

f_value_B = round(pf(f_obs_B,glb,glr,lower.tail = FALSE),7)
f_value_AB = round(pf(f_obs_AB,glab,glr,lower.tail = FALSE),7)

# Tabela da ANOVA
anova_table <- data.frame(Fonte_de_variacao = c("Fator A", "Fator B", "Interação","Residuos", "Total"),
                          GL = c(gla,glb ,glab, glr, gltot),
                          SS = c(ssqA,ssqB ,ssqAB, ssqres, ssqtot),
                          MQ = c(round(qmA,2),round(qmB,2) ,round(qmAB,2), round(qmres,2), ''),
                          F = c(round(f_obs_A,3),round(f_obs_B,3),round(f_obs_AB,3),'',''),
                          Pf = c(f_value_A, f_value_B,f_value_AB, "", ''),
                          stringsAsFactors = FALSE)
rownames(anova_table) <- NULL

anova_table

# 1.3) Os pressupostos necessarios foram atendidos ?

#nossos estimadores
media_total = mean(dados_padronizado$values)

factA_media = tapply(dados_padronizado$values,dados_padronizado$material,mean) - media_total

factB_media = tapply(dados_padronizado$values,dados_padronizado$temperatura,mean) - media_total

factAB_media = tapply(dados_padronizado$values,dados_padronizado$interacao,mean) - rep(tapply(dados_padronizado$values,dados_padronizado$material,mean),3) - rep(tapply(dados_padronizado$values,dados_padronizado$temperatura,mean),each = 3) + media_total


#calculando os residuos
dados_padronizado = dados_padronizado %>% 
  mutate(media_fatorA = ave(values, material, FUN = mean) - mean(values),
         media_fatorB = ave(values, temperatura, FUN = mean) - mean(values),
         media_fatorAB = ave(values, interacao, FUN = mean) - ave(values, temperatura, FUN = mean) - ave(values, material, FUN = mean) + mean(values),
         y_obs = mean(values) + media_fatorA + media_fatorB + media_fatorAB,
         residuo = values - y_obs,
         residuo_normalizado = residuo/qmres)

shapiro.test(dados_padronizado$residuo)

bartlett.test(dados_padronizado$residuo ~ dados_padronizado$material)
bartlett.test(dados_padronizado$residuo ~ dados_padronizado$temperatura)
bartlett.test(dados_padronizado$residuo ~ dados_padronizado$interacao)
#pressuposto de modelo aditivo

mod = lm(dados_padronizado$values ~ dados_padronizado$linhas + dados_padronizado$coluna)

ad = (predict(mod))^2

admod = lm(dados_padronizado$values ~ dados_padronizado$linhas + dados_padronizado$coluna + ad)

anova(mod,admod)
# 1.4) Qual a proporção da variacao total explicada pelo modelo ajustado no item 1.2?

R2 = 1 - (ssqres/ssqtot)

cat("A variancia explicada pelo modelo é", R2)

R2 = ((ssqA+ssqB+ssqAB)/ssqtot)

cat("A variancia explicada pelo modelo é", R2)

# 1.5) Considerando que o objetivo do experimento ´e m´aximizar a vari´avel resposta, 
# qual ´e o elemento quımico que deve ser recomendado? Use teste de Tukey para subsidiar sua resposta.

alfa = 0.05

q.value <- qtukey(alfa, a, glr, lower.tail = F)

hsd = q.value * sqrt(qmres/n)

trat = sort(unique(dados_padronizado$material)) #meus tratamentos ordenados

combinacoes <- combn(trat, 2, simplify = FALSE)  # todas as combinações possíveis de 2 a 2
comb_diff = combn(factA_media,2)

diferencas <- data.frame(
  comparacao = sapply(combinacoes, paste0, collapse = "-") ,
  diferenca = apply(comb_diff, 2, diff)) %>%
  mutate(lwr = diferenca - q.value * sqrt(qmres/n),
         upr = diferenca + q.value * sqrt(qmres/n),
         pvalor = round(ptukey(abs(diferenca/(sqrt(qmres/n))), a, glr, lower.tail = F),6),
         hsd = abs(diferenca) >= hsd)

diferencas #estamos fazendo o teste de tukey em relação ao fator A, o R por padrao ja solta o teste em relacao ao fator B e interação.

# 1.6) Refa¸ca as contas necessarias para responder os itens (1.2), (1.3) e (1.6) utilizando as funcoes
#aov e TukeyHSD e confira com os resultados obtidos.

# anova no r para compara os valores

aov_res = aov(dados_padronizado$values ~ dados_padronizado$material + dados_padronizado$temperatura + dados_padronizado$interacao)

aov_res %>% summary()

#teste de tukey no R

TukeyHSD(aov_res)

# Tamanho da amostra erro tipo II

#parametros de nao centralidade

ncpA = b*rep*sum(factA_media^2/qmres)
ncpB = n*rep*sum(factB_media^2/qmres)
ncpAB = rep*sum(factAB_media^2/qmres)

b = 8

# Fator A

fcrit = qf(1-alpha,gla,glr)

beta = pf(q = fcrit,df1 = gla,df2 = glr, ncp = ncpA)

poder = 1 - beta
poder

# Fator B

fcrit = qf(1-alpha,glb,glr)

beta = pf(q = fcrit,df1 = glb,df2 = glr, ncp = ncpB)

poder = 1 - beta
poder

# Fator C

fcrit = qf(1-alpha,glab,glr)

beta = pf(q = fcrit,df1 = glab,df2 = glr, ncp = ncpAB)

poder = 1 - beta
poder

