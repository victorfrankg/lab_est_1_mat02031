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

# valores trocados (outliers digitados errados?):
# * massa
# - rato 9 do g2 valor 4772 para 477,2 
# - rato 9 do cn valor 4559 para 455,9
# * alimento
# - rato 4 do cn valor 2709 para 27,9
# - rato 9 do g2 valor 293,7 para 29,7
# * agua
# - rato 5 do cn valor 335 para 35

summary(machos$agua)

machos$periodo = relevel(machos$periodo,"acasalamento") 
machos$periodo = relevel(machos$periodo,"pre acasalamento") 
machos$periodo = relevel(machos$periodo,"adaptativo") 


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
