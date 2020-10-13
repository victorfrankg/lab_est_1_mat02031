library(circular)

setwd("C:/Users/diego/OneDrive/ARTIGOS ESCRITOS/Mauricio Tavares Carrion/analise circular")

data<-read.csv("dados mortalidade.csv",h=T,sep=",")
head(data)

taxa<-levels(data[,1])


Mysticeti<-data[data$group=="Mysticeti",]
Odontoceti<-data[data$group=="Odontoceti",]
SeaTurtles<-data[data$group=="Sea Turtles",]
Pinipedia<-data[data$group=="Pinipedia",] 
Sphenisciformes<-data[data$group=="Sphenisciformes",]
Procellariiformes<-data[data$group=="Procellariiformes",]

#Mysticeti
mysti <- rep(Mysticeti$angle, Mysticeti$abundance)
mysti_rad<-rad(mysti)
mysti_circ<-as.circular(mysti_rad)
plot.circular(mysti_circ, rotation = "clock", units = "rads")
#Média (a)
mean.circular(mysti_circ)
#Comprimento do vetor médio (r)
rho.circular(mysti_circ)
#Mediana
median.circular(mysti_circ)
#Moda (e frequência dos dados)
table(mysti_circ)
#Variância no pacote circular
var.circular(mysti_circ)
#variância circular
rho.circular(mysti_circ)
#variância angular
2*(1-rho.circular(mysti_circ))
#Desvio angular (ou desvio padrão angular, que vai de 0 a 81,03?)
sqrt(2*(1-rho.circular(mysti_circ)))
#desvio padrão angular (0 a infinito)
sd.circular(mysti_circ)
#intervalo de confiança (bootstrap)
mle.vonmises.bootstrap.ci(mysti_circ)

summary(mysti_circ)

#Teste de Rayleigh
rayleigh.test(mysti_circ)
#teste de Watson
watson.test(mysti_circ, dist = "vonmises")


#Gráficos com pontos:
  plot.circular(mysti_circ, rotation = "clock", bins = 80, zero = pi/2,
                stack = T, units = "rads", axes = F, col = "black", ticks = F)
#Para colocar os nomes dos meses:
  axis.circular(at=circular(sort(seq(0, 11/6*pi, pi/6), decreasing = T)), c(labels = c("M", "J",
                                                                                       "J", "A", "S", "O", "N", "D", "J", "F", "M","A")))
#Para colocar os vetores médios:
  arrows.circular(mean(mysti_circ), rho.circular(mysti_circ), zero = pi/2, rotation = "clock",
                  col = "red")

#Gráfico com linhas:
plot.circular(mysti_circ, rotation = "clock", bins = 80, zero = pi/2,
                  stack = T, units = "rads", axes = F, col = "black", ticks = F)
axis.circular(at=circular(sort(seq(0, 11/6*pi, pi/6), decreasing = T)), c(labels = c("M", "J",
                                                                                       "J", "A", "S", "O", "N", "D", "J", "F", "M","A")))
lines(density(mysti_circ, bw = 20), col = "black", rotation = "clock", zero = pi/2, shrink = 2)

#Gráfico com a frequência:
rose.diag(mysti_circ, rotation = "clock", zero = pi/2,units = "rads", axes
            = F, border = "black", ticks = F)
axis.circular(at=circular(sort(seq(0, 11/6*pi, pi/6), decreasing = T)), c(labels = c("M", "J",
                                                                                     "J", "A", "S", "O", "N", "D", "J", "F", "M","A")))
#Odontoceti
odonto <- rep(Odontoceti$angle, Odontoceti$abundance)
odonto_rad<-rad(odonto)
odonto_circ<-as.circular(odonto_rad)
plot.circular(odonto_circ, rotation = "clock", units = "rads")
#Média (a)
mean.circular(odonto_circ)
#Comprimento do vetor médio (r)
rho.circular(odonto_circ)
#Variância no pacote circular
var.circular(odonto_circ)
#variância circular
rho.circular(odonto_circ)
#variância angular
2*(1-rho.circular(odonto_circ))
#desvio padrão angular (0 a infinito)
sd.circular(odonto_circ)
#intervalo de confiança (bootstrap)
mle.vonmises.bootstrap.ci(odonto_circ)

#Pinipedia
pini <- rep(Pinipedia$angle, Pinipedia$abundance)
pini_rad<-rad(pini)
pini_circ<-as.circular(pini_rad)
plot.circular(pini_circ, rotation = "clock", units = "rads")
#Média (a)
mean.circular(pini_circ)
#Comprimento do vetor médio (r)
rho.circular(pini_circ)
#Variância no pacote circular
var.circular(pini_circ)
#variância circular
rho.circular(pini_circ)
#variância angular
2*(1-rho.circular(pini_circ))
#desvio padrão angular (0 a infinito)
sd.circular(pini_circ)
#intervalo de confiança (bootstrap)
mle.vonmises.bootstrap.ci(pini_circ)

#Procellariiformes
proce <- rep(Procellariiformes$angle, Procellariiformes$abundance)
proce_rad<-rad(proce)
proce_circ<-as.circular(proce_rad)
plot.circular(proce_circ, rotation = "clock", units = "rads")
#Média (a)
mean.circular(proce_circ)
#Comprimento do vetor médio (r)
rho.circular(proce_circ)
#Variância no pacote circular
var.circular(proce_circ)
#variância circular
rho.circular(proce_circ)
#variância angular
2*(1-rho.circular(proce_circ))
#desvio padrão angular (0 a infinito)
sd.circular(proce_circ)
#intervalo de confiança (bootstrap)
mle.vonmises.bootstrap.ci(proce_circ)

#Sea Turtles
turtles <- rep(SeaTurtles$angle, SeaTurtles$abundance)
turtles_rad<-rad(turtles)
turtles_circ<-as.circular(turtles_rad)
plot.circular(turtles_circ, rotation = "clock", units = "rads")
#Média (a)
mean.circular(turtles_circ)
#Comprimento do vetor médio (r)
rho.circular(turtles_circ)
#Variância no pacote circular
var.circular(turtles_circ)
#variância circular
rho.circular(turtles_circ)
#variância angular
2*(1-rho.circular(turtles_circ))
#desvio padrão angular (0 a infinito)
sd.circular(turtles_circ)
#intervalo de confiança (bootstrap)
mle.vonmises.bootstrap.ci(turtles_circ)

#Sphenisciformes
spheni <- rep(Sphenisciformes$angle, Sphenisciformes$abundance)
spheni_rad<-rad(spheni)
spheni_circ<-as.circular(spheni_rad)
plot.circular(spheni_circ, rotation = "clock", units = "rads")
#Média (a)
mean.circular(spheni_circ)
#Comprimento do vetor médio (r)
rho.circular(spheni_circ)
#Variância no pacote circular
var.circular(spheni_circ)
#variância circular
rho.circular(spheni_circ)
#variância angular
2*(1-rho.circular(spheni_circ))
#desvio padrão angular (0 a infinito)
sd.circular(spheni_circ)
#intervalo de confiança (bootstrap)
mle.vonmises.bootstrap.ci(spheni_circ)

#geral
geral <- rep(data$angle, data$abundance)
geral_rad<-rad(geral)
geral_circ<-as.circular(geral_rad)
plot.circular(geral_circ, rotation = "clock", units = "rads")
#Média (a)
mean.circular(geral_circ)
#Comprimento do vetor médio (r)
rho.circular(geral_circ)
#Variância no pacote circular
var.circular(geral_circ)
#variância circular
rho.circular(geral_circ)
#variância angular
2*(1-rho.circular(geral_circ))
#desvio padrão angular (0 a infinito)
sd.circular(geral_circ)
#intervalo de confiança (bootstrap)
mle.vonmises.bootstrap.ci(geral_circ)

#RAO Spacing test
rao.spacing.test(geral_circ)
rao.spacing.test(mysti_circ)
rao.spacing.test(odonto_circ)
rao.spacing.test(pini_circ)
rao.spacing.test(proce_circ)
rao.spacing.test(spheni_circ)
rao.spacing.test(turtles_circ)

#Rayleigh test
rayleigh.test(geral_circ)
rayleigh.test(mysti_circ)
rayleigh.test(odonto_circ)
rayleigh.test(pini_circ)
rayleigh.test(proce_circ)
rayleigh.test(spheni_circ)
rayleigh.test(turtles_circ)
