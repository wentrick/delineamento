pacman::p_load(tidyverse,dplyr,tidyr)
#montando o dataframe

# Vetores com os fatores
fatorA <- c("A baixo", "A alto")
fatorB <- c("B baixo", "B alto")
fatorC <- c("C baixo", "C alto")

# Gerar todas as combinações
matriz_combinacoes <- expand.grid(fatorA, fatorB, fatorC)
colnames(matriz_combinacoes) = c("A","B","C")
rep_1 = c(22,32,35,55,44,40,60,39)
rep_2 = c(31,43,34,47,45,37,50,41)
rep_3 = c(25,29,50,46,38,36,54,47)

dados = data.frame(matriz_combinacoes,rep_1,rep_2,rep_3) %>%
  mutate(total = rep_1 + rep_2 + rep_3)




#calculando os efeitos

um = dados$total[1]
a = dados$total[2]
b = dados$total[3]
ab = dados$total[4]
c = dados$total[5]
ac = dados$total[6]
bc = dados$total[7]
abc = dados$total[8]

A = (a+ab+ac+abc-um-a-c-ac)/(4*n)

B = (b+ab+bc+abc-um-a-c-ac)/(4*n)

C = (c+ac+bc+abc-um-a-b-ab)/(4*n)

AB = (um-a-b+ab+c-ac-bc+abc)/(4*n)

AC = (um-a+b-ab-c-ac+bc+abc)/(4*n)

BC = (um+a-b-ab-c-ac+bc+abc)/(4*n)

ABC = ((A-1)*(B-1)*(C-1))/(4*n)

#formatando o banco para futuras analises

dados_padronizado = dados %>%
  pivot_longer(cols = c(rep_1,rep_2,rep_3), values_to = "values", names_to = "rep") %>%
  mutate(produtos = as.factor(produtos)) 
  

#soma de quadrados

SQA = (A^2)/(8*n)
SQB = (B^2)/(8*n)
SQC = (C^2)/(8*n)
SQAB = (AB^2)/(8*n)
SQAC = (AC^2)/(8*n)
SQBC = (BC^2)/(8*n)
SQABC = (ABC^2)/(8*n)
SQtot = sum((dados_padronizado$values - mean(dados_padronizado$values))^2)
SQres = SQtot - (SQA+SQB+SQAB+SQAC+SQBC+SQABC)

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
