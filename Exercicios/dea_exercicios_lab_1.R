pacman::p_load(tidyverse)


#### Questao 1 ----

upop = 100
xb = 85
s = 12
n = 16
alfa = 0.10
# H0: mi  = 100
# H1: mi != 100
xcrit = tcrit*3 + 100 

tcalc = (85-100)/(12/sqrt(16))

tcrit = qt(alfa,n-1)

pt(tcalc,n-1)


#parte 2 calculando erro tipo 2

tbeta = (xcrit-90)/(12/sqrt(16))

beta = pt(tbeta, n-1, lower.tail = FALSE)

poder = 1-beta

#### Questao 2 ----

#H0:ua-ud = 0
#H1:ua-ud > 0

antes = c(13.6,13.6,14.7,12.1,12.3,13.2,11.0,12.4)
depois = c(11.4,12.5,14.6,13.0,11.7,10.3,9.80,10.4)



dt = data.frame(antes,depois)
dt = dt %>%
  mutate(diferenca = antes-depois)

alfa = 0.05
shapiro.test(dt$diferenca)

media_diff = mean(dt$diferenca)
desvio_diff = sd(dt$diferenca)
n = length(dt$diferenca)

tcrit = qt(alfa,n-1)

xcrit = tcrit*(desvio_diff/sqrt(n))

tcalc = (media_diff-0)/(desvio_diff/sqrt(n))

pt(tcalc,n-1, lower.tail = FALSE)


#utilizando outro comando

t.test(antes,depois, paired = TRUE, alternative = "greater")

t.test(dt$diferenca, alternative = "greater")


intervalo_superior = media_diff + qt(0.025,n-1,lower.tail = FALSE)*desvio_diff/sqrt(n)

intervalo_inferior = media_diff - qt(0.025,n-1,lower.tail = FALSE)*desvio_diff/sqrt(n)


#### Questao 3 ----


variA = c(1.3,1.4,1.1,1.4,1.5)
variB = c(1.8,1.6,1.9,1.9,1.8)



dt = data.frame(variA,variB)

VA <- c(1.3,1.4,1.1,1.4,1.5)
VB <- c(1.8,1.6,1.9,1.9,1.8)

shapiro.test(VA)
shapiro.test(VB)

shapiro.test(VA-VB)

#var teste
var.test(VA,VB)

t.test(VA,VB,var.equal=T)






