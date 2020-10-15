# ---------------------------------------------------
# Carregando pacotes 

library(circular)


# ---------------------------------------------------
# Criando funções 

# roseta.nae

roseta.nae <- function(data, main, border = "white", col = "lightsalmon", shrink = 1.75, prop = 3, bins = 12*6,...){
  rose.diag(x = data,
    rotation = "clock",
    zero = pi / 2,
    units = "rads",
    axes = F,
    border = border,
    ticks = T,
    prop = prop,
    bins = bins,
    col = col,
    main = main
  )
  axis.circular(at = circular(sort(seq(0, 11 / 6 * pi, pi / 6), decreasing = T)),
                c(month.abb[c(5:12, 1:4)]))
  
  lines(
    density(x = data, bw = 20),
    col = col,
    rotation = "clock",
    zero = pi / 2,
    shrink = shrink
  )
}


# ---------------------------------------------------
# Carregando arquivo de dados 

data <- read.csv(here::here("Assessoria 03 - Mauricio", "dados mortalidade.csv"),
           header = T, sep = ",")

# head(data)

# ---------------------------------------------------
# Manipulação de dados

data$group <- as.factor(data$group) # transformando em fator
# taxa <- levels(data[, 1])

# ---------------------------------------------------
# Criando data.frames Grupo-específicos

Mysticeti         <- data[data$group == "Mysticeti", ]
Odontoceti        <- data[data$group == "Odontoceti", ]
Pinipedia         <- data[data$group == "Pinipedia", ]
Procellariiformes <- data[data$group == "Procellariiformes", ]
SeaTurtles        <- data[data$group == "Sea Turtles", ]
Sphenisciformes   <- data[data$group == "Sphenisciformes", ]

# ---------------------------------------------------
# Criando objetos circulares

# Geral
geral <- rep(data$angle, data$abundance)
geral_rad <- rad(geral)
geral_circ <- as.circular(geral_rad)

# Mysticeti
mysti <- rep(Mysticeti$angle, Mysticeti$abundance)
mysti_rad <- rad(mysti)
mysti_circ <- as.circular(mysti_rad)

# Odontoceti
odonto <- rep(Odontoceti$angle, Odontoceti$abundance)
odonto_rad <- rad(odonto)
odonto_circ <- as.circular(odonto_rad)

# Pinipedia
pini <- rep(Pinipedia$angle, Pinipedia$abundance)
pini_rad <- rad(pini)
pini_circ <- as.circular(pini_rad)

# Procellariiformes
proce <- rep(Procellariiformes$angle, Procellariiformes$abundance)
proce_rad <- rad(proce)
proce_circ <- as.circular(proce_rad)

# SeaTurtles
turtles <- rep(SeaTurtles$angle, SeaTurtles$abundance)
turtles_rad <- rad(turtles)
turtles_circ <- as.circular(turtles_rad)

# Sphenisciformes
spheni <- rep(Sphenisciformes$angle, Sphenisciformes$abundance)
spheni_rad <- rad(spheni)
spheni_circ <- as.circular(spheni_rad)

# ---------------------------------------------------
# Gráficos: diagrama de roseta

par(mfrow = c(2, 3))
roseta.nae(data = mysti_circ, main = "Mysticeti")
roseta.nae(data = odonto_circ, main = "Odontoceti")
roseta.nae(data = pini_circ, main = "Pinipedia")
roseta.nae(data = proce_circ, main = "Procellariiformes")
roseta.nae(data = turtles_circ, main = "Sea Turtles")
roseta.nae(data = spheni_circ, main = "Sphenisciformes")
par(mfrow = c(1, 1))

# ---------------------------------------------------
# Estatísticas descritivas

df.summary <- rbind(summary(mysti_circ), summary(odonto_circ))
df.summary <- rbind(df.summary, summary(pini_circ))
df.summary <- rbind(df.summary, summary(proce_circ))
df.summary <- rbind(df.summary, summary(turtles_circ))
df.summary <- rbind(df.summary, summary(spheni_circ))

# ---------------------------------------------------
# Avaliando unimodalidade (von Mises)

watson.test(mysti_circ, dist = "vonmises")
watson.test(odonto_circ, dist = "vonmises")
watson.test(pini_circ, dist = "vonmises")
watson.test(proce_circ, dist = "vonmises")
watson.test(turtles_circ, dist = "vonmises")
watson.test(spheni_circ, dist = "vonmises")

# ---------------------------------------------------
# Avaliando uniformidade da distribuição
# (ausência de agrupamentos no tempo)

# Teste de Rao
rao.spacing.test(mysti_circ) #,alpha = 0.5)
rao.spacing.test(odonto_circ)
rao.spacing.test(pini_circ)
rao.spacing.test(proce_circ)
rao.spacing.test(spheni_circ)
rao.spacing.test(turtles_circ)

# Teste de Rayleigh
rayleigh.test(mysti_circ)
rayleigh.test(odonto_circ)
rayleigh.test(pini_circ)
rayleigh.test(proce_circ)
rayleigh.test(spheni_circ)
rayleigh.test(turtles_circ)
