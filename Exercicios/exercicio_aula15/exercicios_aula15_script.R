pacman::p_load(tidyverse,dplyr,tidyr)


#montando o dataframe
linhas = c(1,2,3,4,5)
col_1 = c(432,724,489,494,515)
col_2 = c(518,478,384,500,660)
col_3 = c(458,524,556,313,438)
col_4 = c(583,550,297,486,394)
col_5 = c(331,400,420,501,318)

dados = data.frame(linhas,col_1,col_2,col_3,col_4,col_5)

#montando o dataframe dos tratamentos
linhas = c(1,2,3,4,5)
col_1 = c("D","C","E","B","A")
col_2 = c("A","E","B","D","C")
col_3 = c("B","A","C","E","D")
col_4 = c("C","B","D","A","E")
col_5 = c("E","D","A","C","B")

dados_tratamento = data.frame(linhas,col_1,col_2,col_3,col_4,col_5)
#padronizando o data frame para analise

dados_long = dados %>%
  pivot_longer(cols = c(col_1,col_2,col_3,col_4,col_5), values_to = "values", names_to = "coluna") %>%
  mutate(linhas = as.factor(linhas)) 

dados_long_trat = dados_tratamento %>%
  pivot_longer(cols = c(col_1,col_2,col_3,col_4,col_5), values_to = "tratamento", names_to = "coluna") %>%
  mutate(linhas = as.factor(linhas)) 


dados_padronizado = left_join(dados_long,dados_long_trat,by = join_by(linhas == linhas,coluna == coluna)) %>%
  mutate(coluna = as.factor(coluna),
         tratamento = as.factor(tratamento))

#1.1) O modelo considerado e as hipoteses de interesse.

#1.2) A tabela de analise de variancia e suas conclusoes.
# quantidade de linhas, colunas e tratamentos sao todas iguais no caso é 5
p = length(unique(dados_padronizado$tratamento)) #tratamentos
N = p^2
#soma de quadrados
media_total <- mean(dados_padronizado$values)

ssqtot = sum((dados_padronizado$values - media_total)^2)
ssqtrat = sum((tapply(dados_padronizado$values,dados_padronizado$tratamento,mean) - media_total)^2)* p
ssqcol = sum((tapply(dados_padronizado$values,dados_padronizado$coluna,mean) - media_total)^2) *p
ssqlinha = sum((tapply(dados_padronizado$values,dados_padronizado$linhas,mean) - media_total)^2) *p
ssqres = ssqtot-(ssqtrat+ssqcol+ssqlinha)


#graus de liberdade

gll = (p-1)
glc = (p-1)
glt = (p-1)
gltot = (p^2)-1
glr = (p-2)*(p-1)
#quadrados medios

qmtrat = ssqtrat/glt
qmcol = ssqblocos/glc
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
                          F = c(round(f_obs,3),round(f_obs_col,3),round(f_obs_col,3),'',''),
                          Pf = c(f_value, f_value_linha,f_value_blocos, "", ''),
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
  mutate(media_trat = ave(values, linhas, FUN = mean) - mean(values),
         media_bloco = ave(values, coluna, FUN = mean) - mean(values),
         y_obs = mean(values) + media_trat + media_bloco,
         residuo = values - y_obs,
         residuo_normalizado = residuo/qmres)

shapiro.test(dados_padronizado$residuo)

bartlett.test(dados_padronizado$residuo ~ dados_padronizado$linhas)

#pressuposto de modelo aditivo

mod = lm(dados_padronizado$values ~ dados_padronizado$linhas + dados_padronizado$coluna)

ad = (predict(mod))^2

admod = lm(dados_padronizado$values ~ dados_padronizado$linhas + dados_padronizado$coluna + ad)

anova(mod,admod)
# 1.4) Qual a proporção da variacao total explicada pelo modelo ajustado no item 1.2?

R2 = 1 - (ssqres/ssqtot)

cat("A variancia explicada pelo medole é", R2)

R2 = ((ssqtrat+ssqcol+ssqlinha)/ssqtot)

cat("A variancia explicada pelo medole é", R2)

# 1.5) Considerando que o objetivo do experimento ´e m´aximizar a vari´avel resposta, 
# qual ´e o elemento quımico que deve ser recomendado? Use teste de Tukey para subsidiar sua resposta.

alfa = 0.05

q.value <- qtukey(alfa, t, glr, lower.tail = F)

hsd = q.value * sqrt(qmres/c)

trat = sort(unique(dados_padronizado$tratamento)) #meus tratamentos ordenados

combinacoes <- combn(trat, 2, simplify = FALSE)  # todas as combinações possíveis de 2 a 2
comb_diff = combn(trat_media,2)

diferencas <- data.frame(
  comparacao = sapply(combinacoes, paste0, collapse = "-") ,
  diferenca = apply(comb_diff, 2, diff)) %>%
  mutate(lwr = diferenca - q.value * sqrt(qmres/b),
         upr = diferenca + q.value * sqrt(qmres/b),
         pvalor = round(ptukey(abs(diferenca/(sqrt(qmres/c))), t, glr, lower.tail = F),6),
         hsd = abs(diferenca) >= hsd)

diferencas

# 1.6) Refa¸ca as contas necessarias para responder os itens (1.2), (1.3) e (1.6) utilizando as funcoes
#aov e TukeyHSD e confira com os resultados obtidos.


# anova no r para compara os valores

aov_res = aov(dados_padronizado$values ~ dados_padronizado$tratamento + dados_padronizado$linhas + dados_padronizado$coluna)

aov_res %>% summary()

#teste de tukey no R

TukeyHSD(aov_res)



