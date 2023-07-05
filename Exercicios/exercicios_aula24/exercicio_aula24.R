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

  

                       
#calculando os efeitos

um = dados$total[1]
a = dados$total[2]
b = dados$total[3]
ab = dados$total[4]
c = dados$total[5]
ac = dados$total[6]
bc = dados$total[7]
abc = dados$total[8]









