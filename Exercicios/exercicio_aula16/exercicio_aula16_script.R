pacman::p_load(tidyverse,dplyr,tidyr)


#montando o dataframe
linhas = c(1:12)
col_1 = c(19.56,22.94,25.06,23.24,16.28,18.53,23.98,15.33,24.41,16.65,18.96,21.49)
col_2 = c(23.16,27.51,17.70,23.54,22.29,19.89,20.46,23.02,22.44,22.69,24.19,15.78)
col_3 = c(29.72,23.71,22.32,18.75,28.09,20.42,19.28,24.97,19.23,24.94,21.95,24.65)

dados = data.frame(linhas,col_1,col_2,col_3)

#montando o dataframe dos tratamentos
linhas = c(1:12)
col_1 = c("A","B","C","B","A","C","C","A","B","A","B","C")
col_2 = c("B","C","A","C","B","A","A","B","C","B","C","A")
col_3 = c("C","A","B","A","C","B","B","C","A","C","A","B")


dados_tratamento = data.frame(linhas,col_1,col_2,col_3)
#padronizando o data frame para analise

dados_long = dados %>%
  pivot_longer(cols = c(col_1,col_2,col_3), values_to = "values", names_to = "coluna") %>%
  mutate(linhas = as.factor(linhas)) 

dados_long_trat = dados_tratamento %>%
  pivot_longer(cols = c(col_1,col_2,col_3), values_to = "tratamento", names_to = "coluna") %>%
  mutate(linhas = as.factor(linhas)) 


dados_padronizado = left_join(dados_long,dados_long_trat,by = join_by(linhas == linhas,coluna == coluna)) %>%
  mutate(coluna = as.factor(coluna),
         tratamento = as.factor(tratamento))

dados_padronizado = left_join(dados_long,dados_long_trat,by = join_by(linhas == linhas,coluna == coluna)) %>%
  mutate(coluna = as.factor(coluna),
         tratamento = as.factor(tratamento))

#1.1) O modelo considerado e as hipoteses de interesse.

#1.2) A tabela de analise de variancia e suas conclusoes.
# quantidade de linhas, colunas e tratamentos sao todas iguais no caso é 5
p = length(unique(dados_padronizado$tratamento)) #tratamentos
N = p^2
n = 4 #numero de repetições
#soma de quadrados
media_total <- mean(dados_padronizado$values)

ssqtot = sum((dados_padronizado$values - media_total)^2)
ssqtrat = sum((tapply(dados_padronizado$values,dados_padronizado$tratamento,mean) - media_total)^2)* p*n
ssqcol = sum((tapply(dados_padronizado$values,dados_padronizado$coluna,mean) - media_total)^2) *p*n
ssqlinha = sum((tapply(dados_padronizado$values,dados_padronizado$linhas,mean) - media_total)^2) *p
ssqres = ssqtot-(ssqtrat+ssqcol+ssqlinha)


#graus de liberdade

gll = (n*p-1)
glc = (p-1)
glt = (p-1)
gltot = (n*p^2)-1
glr = gltot - (gll+glc+glt)
#quadrados medios

qmtrat = ssqtrat/glt
qmcol = ssqcol/glc
qmlinha = ssqlinha/gll
qmres = ssqres/glr

# valor F observado

f_obs = qmtrat/qmres
f_obs_col = qmcol/qmres
f_obs_linha = qmlinha/qmres

#valor critico de 5%
alfa = 0.05
f_crit = qf(alfa,glt,glr)
f_crit_col = qf(alfa,glc,glr)
f_crit_linha = qf(alfa,gll,glr)
#p-valor observado

f_value = round(pf(f_obs,glt,glr,lower.tail = FALSE),3)

f_value_col = round(pf(f_obs_col,glc,glr,lower.tail = FALSE),7)
f_value_linha = round(pf(f_obs_linha,gll,glr,lower.tail = FALSE),7)

# Tabela da ANOVA
anova_table <- data.frame(Fonte_de_variacao = c("tratamento", "linha", "coluna","Residuos", "Total"),
                          GL = c(glt,gll ,glc, glr, gltot),
                          SS = c(ssqtrat,ssqlinha ,ssqcol, ssqres, ssqtot),
                          MQ = c(round(qmtrat,2),round(qmlinha,2) ,round(qmcol,2), round(qmres,2), ''),
                          F = c(round(f_obs,3),round(f_obs_linha,3),round(f_obs_col,3),'',''),
                          Pf = c(f_value, f_value_linha,f_value_col, "", ''),
                          stringsAsFactors = FALSE)
rownames(anova_table) <- NULL

anova_table
# 1.3) Os pressupostos necessarios foram atendidos ?

#nossos estimadores
media_total = mean(dados_padronizado$values)

trat_media = tapply(dados_padronizado$values,dados_padronizado$tratamento,mean) - media_total

linhas_media = tapply(dados_padronizado$values,dados_padronizado$linhas,mean) - media_total

coluna_media = tapply(dados_padronizado$values,dados_padronizado$coluna,mean) - media_total


#calculando os residuos
dados_padronizado = dados_padronizado %>% 
  mutate(media_linha = ave(values, linhas, FUN = mean) - mean(values),
         media_coluna = ave(values, coluna, FUN = mean) - mean(values),
         media_trat = ave(values, tratamento, FUN = mean) - mean(values),
         y_obs = mean(values) + media_trat + media_linha+media_coluna,
         residuo = values - y_obs,
         residuo_normalizado = residuo/qmres)

shapiro.test(dados_padronizado$residuo)

bartlett.test(dados_padronizado$residuo ~ dados_padronizado$tratamento)

#pressuposto de modelo aditivo

mod = lm(dados_padronizado$values ~ dados_padronizado$linhas + dados_padronizado$coluna)

ad = (predict(mod))^2

admod = lm(dados_padronizado$values ~ dados_padronizado$linhas + dados_padronizado$coluna + ad)

anova(mod,admod)
# 1.4) Qual a proporção da variacao total explicada pelo modelo ajustado no item 1.2?

R2 = 1 - (ssqres/ssqtot)

cat("A variancia explicada pelo modelo é", R2)

R2 = ((ssqtrat+ssqcol+ssqlinha)/ssqtot)

cat("A variancia explicada pelo modelo é", R2)

# 1.5) Considerando que o objetivo do experimento ´e m´aximizar a vari´avel resposta, 
# qual ´e o elemento quımico que deve ser recomendado? Use teste de Tukey para subsidiar sua resposta.

alfa = 0.05

q.value <- qtukey(alfa, p, glr, lower.tail = F)

hsd = q.value * sqrt(qmres/p)

trat = sort(unique(dados_padronizado$tratamento)) #meus tratamentos ordenados

combinacoes <- combn(trat, 2, simplify = FALSE)  # todas as combinações possíveis de 2 a 2
comb_diff = combn(trat_media,2)

diferencas <- data.frame(
  comparacao = sapply(combinacoes, paste0, collapse = "-") ,
  diferenca = apply(comb_diff, 2, diff)) %>%
  mutate(lwr = diferenca - q.value * sqrt(qmres/p),
         upr = diferenca + q.value * sqrt(qmres/p),
         pvalor = round(ptukey(abs(diferenca/(sqrt(qmres/p))), p, glr, lower.tail = F),6),
         hsd = abs(diferenca) >= hsd)

diferencas

# 1.6) Refa¸ca as contas necessarias para responder os itens (1.2), (1.3) e (1.6) utilizando as funcoes
#aov e TukeyHSD e confira com os resultados obtidos.


# anova no r para compara os valores

aov_res = aov(dados_padronizado$values ~ dados_padronizado$tratamento + dados_padronizado$linhas + dados_padronizado$coluna)

aov_res %>% summary()

#teste de tukey no R

TukeyHSD(aov_res)







