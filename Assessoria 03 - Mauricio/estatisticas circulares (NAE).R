#--------------------#
# Carregando Pacotes #
#--------------------#

library(circular)

#----------------#
# Banco de Dados #
#----------------#

data <- read.csv("Assessoria 03 - Mauricio/dados mortalidade.csv",h=T,sep=",")
head(data)

data$group = as.factor(data$group) # transformando em fator
taxa <- levels(data[,1])

# Grupos
Mysticeti <- data[data$group=="Mysticeti",]
Odontoceti <- data[data$group=="Odontoceti",]
Pinipedia <- data[data$group=="Pinipedia",] 
Procellariiformes <- data[data$group=="Procellariiformes",]
SeaTurtles <- data[data$group=="Sea Turtles",]
Sphenisciformes <- data[data$group=="Sphenisciformes",]

#---------------#
#               #
#   Análises !  #
#               #
#---------------#


#-------#
# Geral #
#-------#

## Explorando
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


## Testes de Hipóteses

rao.spacing.test(geral_circ) 
rayleigh.test(geral_circ)
watson.test(geral_circ, dist = "uniform")

## Gráficos

# frequência
rose.diag(geral_circ, rotation = "clock", zero = pi/2,units = "rads", 
          axes = F, border = "black", ticks = T, prop = 3, main = "bla bla")
axis.circular(at=circular(sort(seq(0, 11/6*pi, pi/6), decreasing = T)), c(labels = c("M", "J",
                                                                                     "J", "A", "S", "O", "N", "D", "J", "F", "M","A")))
lines(density(geral_circ, bw = 20), col = "red", rotation = "clock", zero = pi/2, shrink = 1.75)



#-----------#
# Mysticeti #
#-----------#

## Explorando
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


## Gráficos

# frequência
rose.diag(mysti_circ, rotation = "clock", zero = pi/2,units = "rads", 
          axes = F, border = "black", ticks = T, prop = 3, main = "bla bla")
axis.circular(at=circular(sort(seq(0, 11/6*pi, pi/6), decreasing = T)), c(labels = c("M", "J",
                                                                                     "J", "A", "S", "O", "N", "D", "J", "F", "M","A")))
lines(density(mysti_circ, bw = 20), col = "red", rotation = "clock", zero = pi/2, shrink = 1.75)


## Testes de Hipóteses

rao.spacing.test(mysti_circ) 
rayleigh.test(mysti_circ)
watson.test(mysti_circ, dist = "uniform")


#------------#
# Odontoceti #
#------------#

## Explorando
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


## Gráficos

# frequência
rose.diag(odonto_circ, rotation = "clock", zero = pi/2,units = "rads", 
          axes = F, border = "black", ticks = T, prop = 3, main = "bla bla")
axis.circular(at=circular(sort(seq(0, 11/6*pi, pi/6), decreasing = T)), c(labels = c("M", "J",
                                                                                     "J", "A", "S", "O", "N", "D", "J", "F", "M","A")))
lines(density(odonto_circ, bw = 20), col = "red", rotation = "clock", zero = pi/2, shrink = 1.75)

## Testes de Hipóteses

rao.spacing.test(odonto_circ) 
rayleigh.test(odonto_circ)
watson.test(odonto_circ, dist = "uniform")


#-----------#
# Pinipedia #
#-----------#

## Explorando
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


## Gráficos

# frequência
rose.diag(pini_circ, rotation = "clock", zero = pi/2,units = "rads", 
          axes = F, border = "black", ticks = T, prop = 3, main = "bla bla")
axis.circular(at=circular(sort(seq(0, 11/6*pi, pi/6), decreasing = T)), c(labels = c("M", "J",
                                                                                     "J", "A", "S", "O", "N", "D", "J", "F", "M","A")))
lines(density(pini_circ, bw = 20), col = "red", rotation = "clock", zero = pi/2, shrink = 1.75)

## Testes de Hipóteses

rao.spacing.test(pini_circ) 
rayleigh.test(pini_circ)
watson.test(pini_circ, dist = "uniform")

#-------------------#
# Procellariiformes #
#-------------------#

## Explorando
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


## Gráficos

# frequência
rose.diag(proce_circ, rotation = "clock", zero = pi/2,units = "rads", 
          axes = F, border = "black", ticks = T, prop = 3, main = "bla bla")
axis.circular(at=circular(sort(seq(0, 11/6*pi, pi/6), decreasing = T)), c(labels = c("M", "J",
                                                                                     "J", "A", "S", "O", "N", "D", "J", "F", "M","A")))
lines(density(proce_circ, bw = 20), col = "red", rotation = "clock", zero = pi/2, shrink = 1.75)

## Testes de Hipóteses

rao.spacing.test(proce_circ) 
rayleigh.test(proce_circ)
watson.test(proce_circ, dist = "uniform")

#------------#
# SeaTurtles #
#------------#

## Explorando
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


## Gráficos

# frequência
rose.diag(turtles_circ, rotation = "clock", zero = pi/2,units = "rads", 
          axes = F, border = "black", ticks = T, prop = 3, main = "bla bla")
axis.circular(at=circular(sort(seq(0, 11/6*pi, pi/6), decreasing = T)), c(labels = c("M", "J",
                                                                                     "J", "A", "S", "O", "N", "D", "J", "F", "M","A")))
lines(density(turtles_circ, bw = 20), col = "red", rotation = "clock", zero = pi/2, shrink = 1.75)

## Testes de Hipóteses

rao.spacing.test(turtles_circ) 
rayleigh.test(turtles_circ)
watson.test(turtles_circ, dist = "uniform")

#-----------------#
# Sphenisciformes #
#-----------------#

## Explorando
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


## Gráficos

# frequência
rose.diag(spheni_circ, rotation = "clock", zero = pi/2,units = "rads", 
          axes = F, border = "black", ticks = T, prop = 3, main = "bla bla")
axis.circular(at=circular(sort(seq(0, 11/6*pi, pi/6), decreasing = T)), c(labels = c("M", "J",
                                                                                     "J", "A", "S", "O", "N", "D", "J", "F", "M","A")))
lines(density(spheni_circ, bw = 20), col = "red", rotation = "clock", zero = pi/2, shrink = 1.75)

## Testes de Hipóteses

rao.spacing.test(spheni_circ) 
rayleigh.test(spheni_circ)
watson.test(spheni_circ, dist = "uniform")




#--------

par(mfrow=c(1,2))
rose.diag(turtles_circ, rotation = "clock", zero = pi/2,units = "rads", 
          axes = F, border = "black", ticks = T, prop = 3, main = "bla bla")
axis.circular(at=circular(sort(seq(0, 11/6*pi, pi/6), decreasing = T)), c(labels = c("M", "J",
                                                                                     "J", "A", "S", "O", "N", "D", "J", "F", "M","A")))
lines(density(turtles_circ, bw = 20), col = "red", rotation = "clock", zero = pi/2, shrink = 4)

rose.diag(spheni_circ, rotation = "clock", zero = pi/2,units = "rads", 
          axes = F, border = "black", ticks = T, prop = 3, main = "bla bla")
axis.circular(at=circular(sort(seq(0, 11/6*pi, pi/6), decreasing = T)), c(labels = c("M", "J",
                                                                                     "J", "A", "S", "O", "N", "D", "J", "F", "M","A")))
lines(density(spheni_circ, bw = 20), col = "red", rotation = "clock", zero = pi/2, shrink = 4)