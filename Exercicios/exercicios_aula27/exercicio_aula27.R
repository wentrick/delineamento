pacman::p_load(tidyverse,dplyr,tidyr)
#montando o dataframe

# Vetores com os fatores
fatorA <- rep(c("Aracao", "Aracaoo+Gradagem","Subsolagem"),each = 3)
fatorB = rep(c("A", "B","C"),3)
# Gerar todas as combinações

bloco_1 = c(4.2,4.5,5.2,3.8,3.7,3.5,4.2,4,3.9)
bloco_2 = c(4.6,4.7,5,4.4,3.5,3.1,4.2,3.8,3.9)
bloco_3 = c(4.5,4.3,6.8,4.8,3.1,3.4,5.2,3.7,3.7)
bloco_4 = c(4.4,4.7,5.8,3.9,3.7,3.3,5.1,4.1,4)
dados = data.frame(fatorA,fatorB,bloco_1,bloco_2,bloco_3,bloco_4) %>%
  mutate(total = bloco_1 + bloco_2 + bloco_3 + bloco_4,
         int_AB = interaction(fatorA,fatorB),
         fatorA = as.factor(fatorA),
         fatorB = as.factor(fatorB))


n = 4 #numero de repeticoes = numero de blocos

#banco de dados padronizado
dados_padronizado = dados %>%
  pivot_longer(cols = c(bloco_1,bloco_2,bloco_3,bloco_4), values_to = "values", names_to = "bloco") %>%
  mutate(parcela = interaction(bloco,fatorA),
         bloco = as.factor(bloco),
         values = as.numeric(values))

#niveis

a = length(unique(dados_padronizado$fatorA))
b = length(unique(dados_padronizado$fatorB))
r = length(unique(dados_padronizado$bloco))

#soma de quadrados
media_total = mean(dados_padronizado$values)

ssqtot = sum((dados_padronizado$values - mean(dados_padronizado$values))^2)
ssqA = sum((tapply(dados_padronizado$values,dados_padronizado$fatorA,mean) - media_total)^2)*r*b
ssqB = sum((tapply(dados_padronizado$values,dados_padronizado$fatorB,mean) - media_total)^2)*r*a
ssqbloco = sum((tapply(dados_padronizado$values,dados_padronizado$bloco,mean) - media_total)^2)*a*b
ssqAB = sum((tapply(dados_padronizado$values,dados_padronizado$int_AB,mean) - media_total)^2)*r - ssqA - ssqB
ssqparc = sum((tapply(dados_padronizado$values,dados_padronizado$parcela,mean) - media_total)^2)*b
ssqresA = ssqparc - (ssqbloco+ssqA)
ssqres = ssqtot - (ssqparc+ssqB+ssqAB)

#graus de liberdade
glA = a-1
glB = b-1
glbloco = r-1
glAB = (a-1)*(b-1)
glparc = a*r-1
glresA = (r-1)*(a-1)
glres = a*(b-1)*(r-1) #verificar se esta certo
gltot = a*b*n - 1


#quadrados medios

qmA = ssqA/glA
qmB = ssqB/glB
qmbloco = ssqbloco/glbloco
qmAB = ssqAB/glAB
qmparc = ssqparc/glparc
qmresA = ssqresA/glresA
qmres = ssqres/glres

# valor F observado

f_obs_A = qmA/qmresA #aqui é dividido pelo residuo de A
f_obs_B = qmB/qmres
f_obs_bloco = qmbloco/qmres
f_obs_AB = qmAB/qmres
f_obs_parc = qmparc/qmres
f_obs_resA = qmresA/qmres


#p-valor observado

f_value_A = round(pf(f_obs_A,glA,glresA,lower.tail = FALSE),7)
f_value_B = round(pf(f_obs_B,glB,glres,lower.tail = FALSE),7)
f_value_bloco = round(pf(f_obs_bloco,glbloco,glres,lower.tail = FALSE),7)
f_value_AB = round(pf(f_obs_AB,glAB,glres,lower.tail = FALSE),7)
f_value_parc = round(pf(f_obs_parc,glparc,glres,lower.tail = FALSE),7)




# Tabela da ANOVA
anova_table <- data.frame(Fonte_de_variacao = c("Bloco", "Fator A", "Residuo A", "Parcela", "Fator B", "A:B","Residuo","Total"),
                          GL = c(glbloco, glA, glresA, glparc, glB, glAB, glres, gltot),
                          SS = round(c(ssqbloco, ssqA, ssqresA, ssqparc, ssqB, ssqAB, ssqres, ssqtot),4),
                          MQ = round(c(qmbloco, qmA, qmresA, NA, qmB, qmAB, qmres, NA),4),
                          F = round(c(NA, f_obs_A, NA, NA, f_obs_B, f_obs_AB, NA,NA),3),
                          Pf = round(c(NA, f_value_A, NA, NA, f_value_B, f_value_AB, NA,NA),7),
                          stringsAsFactors = FALSE)
rownames(anova_table) <- NULL


anova_table



modelo = aov(values~fatorA*fatorB + Error(bloco/fatorA),data = dados_padronizado)
summary(modelo)

modelo2 = aov(values~fatorA*fatorB + bloco:fatorA,data = dados_padronizado)
summary(modelo2)

tukey(modelo2)

ea2(dados_padronizado[,c(1,5,2,6)],design=5)




