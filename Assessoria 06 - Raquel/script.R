library(xlsx)
library(here)

machos = read.xlsx(here("Assessoria 06 - Raquel","oleuropeina_limpos.xlsx"), sheetIndex = 1)
# femeas = read.xlsx(here("Assessoria 06 - Raquel","oleuropeina_limpos.xlsx"), sheetIndex = 2)

colnames(machos) = c("t","dia","periodo","grupo","rato","massa","alimento","agua")
# colnames(femeas) = c("t","dia","periodo","grupo","rato","massa","alimento","agua")

str(machos)
for (i in c(3,4,5)){
  machos[,i] = as.factor(machos[,i])
}
str(machos)

machos$agua = as.numeric(machos$agua)

summary(machos$agua)

library(ggplot2)
ggplot(data=machos, aes(x=t, y=massa, group=interaction(t, grupo))) +
  geom_boxplot(aes(fill=grupo)) +
  facet_wrap(~periodo, scales = "free")

ggplot(data=machos, aes(x=t, y=alimento, group=interaction(t, grupo))) +
  geom_boxplot(aes(fill=grupo)) +
  facet_wrap(~periodo, scales = "free")

ggplot(data=machos, aes(x=t, y=agua, group=interaction(t, grupo))) +
  geom_boxplot(aes(fill=grupo)) +
  facet_wrap(~periodo, scales = "free")
