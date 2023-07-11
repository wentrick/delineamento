pacman::p_load(tidyverse,dplyr,tidyr)
#montando o dataframe

# Vetores com os fatores
fatorA <- c("A baixo", "A alto")
fatorB <- c("B baixo", "B alto")
fatorC <- c("C baixo", "C alto")

# Gerar todas as combinações
matriz_combinacoes <- expand.grid(fatorA, fatorB, fatorC)
colnames(matriz_combinacoes) = c("A","B","C")
rep_1 = c(550,669,633,642,1037,749,1075,729)
rep_2 = c(604,650,601,635,1052,868,1063,860)


dados = data.frame(matriz_combinacoes,rep_1,rep_2) %>%
  mutate(total = rep_1 + rep_2)

  
n = 2#numero de repeticoes

#calculando os efeitos

um = dados$total[1]
a = dados$total[2]
b = dados$total[3]
ab = dados$total[4]
c = dados$total[5]
ac = dados$total[6]
bc = dados$total[7]
abc = dados$total[8]

contrasteA = (a+ab+ac+abc-um-b-c-bc)
contrasteB = (b+ab+bc+abc-um-a-c-ac)
contrasteC = (c+ac+bc+abc-um-a-b-ab)
contrasteAB = (um-a-b+ab+c-ac-bc+abc)
contrasteAC = (um-a+b-ab-c+ac-bc+abc)
contrasteBC = (um+a-b-ab-c-ac+bc+abc)
contrasteABC = (abc-ac-bc+c-ab+a+b-um)

A = contrasteA/(4*n)

B = contrasteB/(4*n)

C = contrasteC/(4*n)

AB = contrasteAB/(4*n)

AC = contrasteAC/(4*n)

BC = contrasteABC/(4*n)

ABC = contrasteABC/(4*n)

#formatando o banco para futuras analises

dados_padronizado = dados %>%
  pivot_longer(cols = c(rep_1,rep_2), values_to = "values", names_to = "rep") 


#soma de quadrados

SQA = (contrasteA^2)/(8*n)
SQB = (contrasteB^2)/(8*n)
SQC = (contrasteC^2)/(8*n)
SQAB = (contrasteAB^2)/(8*n)
SQAC = (contrasteAC^2)/(8*n)
SQBC = (contrasteBC^2)/(8*n)
SQABC = (contrasteABC^2)/(8*n)
SQtot = sum((dados_padronizado$values - mean(dados_padronizado$values))^2)
SQres = SQtot - (SQA+SQB+SQC+SQAB+SQAC+SQBC+SQABC)

#graus de liberdade
a = length(unique(dados_padronizado$A))
b = length(unique(dados_padronizado$B))
c = length(unique(dados_padronizado$C))
r = length(unique(dados_padronizado$rep))

glA <- a - 1
glB <- b - 1
glC <- c - 1
glAB <- (a-1)*(b-1)
glAC <- (a-1)*(c-1)
glBC <- (b-1)*(c-1)
glABC <- (a-1)*(b-1)*(c-1)
glRES <- a*b*c*(r-1)
glTOT = (8*n-1)
#quadrados medios

qmA = SQA/glA
qmB = SQB/glB
qmC = SQC/glC
qmAB = SQAB/glAB
qmAC = SQAC/glAC
qmBC = SQBC/glBC
qmABC = SQABC/glABC
qmres = SQres/glRES


# valor F observado

f_obs_A = qmA/qmres
f_obs_B = qmB/qmres
f_obs_C = qmC/qmres
f_obs_AB = qmAB/qmres
f_obs_AC = qmAC/qmres
f_obs_BC = qmBC/qmres
f_obs_ABC = qmABC/qmres

#p-valor observado

f_value_A = round(pf(f_obs_A,glA,glRES,lower.tail = FALSE),7)
f_value_B = round(pf(f_obs_B,glB,glRES,lower.tail = FALSE),7)
f_value_C = round(pf(f_obs_C,glC,glRES,lower.tail = FALSE),7)
f_value_AB = round(pf(f_obs_AB,glAB,glRES,lower.tail = FALSE),7)
f_value_AC = round(pf(f_obs_AC,glAC,glRES,lower.tail = FALSE),7)
f_value_BC = round(pf(f_obs_BC,glBC,glRES,lower.tail = FALSE),7)
f_value_ABC = round(pf(f_obs_ABC,glABC,glRES,lower.tail = FALSE),7)

# Tabela da ANOVA
anova_table <- data.frame(Fonte_de_variacao = c("Fator A", "Fator B", "Fator C", "A:B", "A:C", "B:C","A:B:C","Residuo","Total"),
                          GL = c(glA, glB, glC, glAB, glAC, glBC, glABC, glRES, glTOT),
                          SS = round(c(SQA, SQB, SQC, SQAB, SQAC, SQBC, SQABC, SQres, SQtot),1),
                          MQ = round(c(qmA, qmB, qmC, qmAB, qmAC, qmBC, qmABC, qmres,0),1),
                          F = round(c(f_obs_A, f_obs_B, f_obs_C, f_obs_AB, f_obs_AC, f_obs_BC, f_obs_ABC,0,0),3),
                          Pf = round(c(f_value_A, f_value_B, f_value_C, f_value_AB, f_value_AC, f_value_BC, f_value_ABC,0,0),6),
                          stringsAsFactors = FALSE)
rownames(anova_table) <- NULL


anova_table


modelo = aov(values ~ A*B*C, data = dados_padronizado)
summary(modelo)




