pacman::p_load(tidyverse,dplyr,tidyr)
#montando o dataframe

# Vetores com os fatores
fatorA <- rep(c("Aração", "Aração+Gradagem","Subsolagem"),each = 3)
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
  mutate(parcela = interaction(bloco,fatorA))

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
ssqres = ssqtot - (ssqparc+ssqB+ssqA)
#graus de liberdade

glA = a-1
glB = b-1
glbloco = r-1
glAB = (a-1)*(b-1)
glparc = a*r-1
glresA = (r-1)*(a-1)
glres = (a*b-1)*(r-1)
gltot = a*b*r - 1


#quadrado médio


#p-valor

modelo = aov(values~fatorA*fatorB + Error(bloco/fatorA),data = dados_padronizado)
summary(modelo)

modelo2 = aov(values~fatorA*fatorB + bloco:fatorA,data = dados_padronizado)
summary(modelo2)


















