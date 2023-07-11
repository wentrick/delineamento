pacman::p_load(tidyverse,dplyr,tidyr)

# Vetores com os fatores
fatorA <- c("A baixo", "A alto")
fatorB <- c("B baixo", "B alto")

# Gerar todas as combinações
matriz_combinacoes <- expand.grid(fatorA, fatorB)
colnames(matriz_combinacoes) = c("A","B")
rep_1 = c(28,36,18,31)
rep_2 = c(25,32,19,30)
rep_3 = c(27,32,23,29)


dados = data.frame(matriz_combinacoes,rep_1,rep_2,rep_3) %>%
  mutate(total = rep_1 + rep_2 + rep_3)

n = 3 #numero de repeticoes

#calculando os efeitos

um = dados$total[1]
a = dados$total[2]
b = dados$total[3]
ab = dados$total[4]

contrasteA = (ab+a-b-um)
contrasteB = (ab+b-a-um)
contrasteAB = (ab+um-a-b)

A = contrasteA/(4*n)

B = contrasteB/(4*n)

AB = contrasteAB/(4*n)

#formatando o banco para futuras analises

dados_padronizado = dados %>%
  pivot_longer(cols = c(rep_1,rep_2,rep_3), values_to = "values", names_to = "rep") 


#soma de quadrados

SQA = (contrasteA^2)/(4*n)
SQB = (contrasteB^2)/(4*n)
SQAB = (contrasteAB^2)/(4*n)
SQtot = sum((dados_padronizado$values - mean(dados_padronizado$values))^2)
SQres = SQtot - (SQA+SQB+SQAB)

#graus de liberdade
a = length(unique(dados_padronizado$A))
b = length(unique(dados_padronizado$B))
r = length(unique(dados_padronizado$rep))

glA <- a - 1
glB <- b - 1
glAB <- (a-1)*(b-1)
glRES <- a*b*(r-1)
glTOT = (4*n-1)
#quadrados medios

qmA = SQA/glA
qmB = SQB/glB
qmAB = SQAB/glAB
qmres = SQres/glRES


# valor F observado

f_obs_A = qmA/qmres
f_obs_B = qmB/qmres
f_obs_AB = qmAB/qmres

#p-valor observado

f_value_A = round(pf(f_obs_A,glA,glRES,lower.tail = FALSE),7)
f_value_B = round(pf(f_obs_B,glB,glRES,lower.tail = FALSE),7)
f_value_AB = round(pf(f_obs_AB,glAB,glRES,lower.tail = FALSE),7)

# Tabela da ANOVA
anova_table <- data.frame(Fonte_de_variacao = c("Fator A", "Fator B", "A:B","Residuo","Total"),
                          GL = c(glA, glB, glAB, glRES, glTOT),
                          SS = round(c(SQA, SQB, SQAB, SQres, SQtot),1),
                          MQ = round(c(qmA, qmB, qmAB, qmres,0),1),
                          F = round(c(f_obs_A, f_obs_B, f_obs_AB,0,0),3),
                          Pf = round(c(f_value_A, f_value_B, f_value_AB,0,0),6),
                          stringsAsFactors = FALSE)
rownames(anova_table) <- NULL


anova_table


modelo = aov(values ~ A*B, data = dados_padronizado)
summary(modelo)












 