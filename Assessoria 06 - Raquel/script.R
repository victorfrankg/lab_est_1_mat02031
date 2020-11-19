library(xlsx)
library(here)

machos = read.xlsx(here("Assessoria 06 - Raquel","oleuropeina_limpos.xlsx"), sheetIndex = 1)
# femeas = read.xlsx(here("Assessoria 06 - Raquel","oleuropeina_limpos.xlsx"), sheetIndex = 2)

colnames(machos) = c("t","dia","periodo","grupo","rato","massa","alimento","agua")
# colnames(femeas) = c("t","dia","periodo","grupo","rato","massa","alimento","agua")

#--------#
# Machos #
#--------#

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

# órgãos
library(compareGroups)

org_machos = read.xlsx(here("Assessoria 06 - Raquel","registro_limpo.xlsx"), sheetIndex = 1)
colnames(org_machos) = c("grupo","rato","massa","coracao","baco","figado","rim_dir","rim_esq",
                         "test_dir","test_esq","vesicula_seminal","epididimo_dir","epididimo_esq","prostata",
                         "celulas_espermaticas","espermatozoides","total_espermatozoide",
                         "producao_espermatides")
res = compareGroups(grupo ~ . - rato, data = org_machos)
res = compareGroups(grupo ~ . - rato, data = org_machos, method = 1)
summary(res)

tabela = createTable(res, show.p.mul = T)

g = list()

for (i in 3:(ncol(org_machos))){
  g[[i-2]] = ggplot(data=org_machos, aes(y=org_machos[,i])) +
    geom_boxplot(aes(fill=grupo)) +
    ylab(paste(colnames(org_machos)[i]))
}

for (i in 1:length(g)){
  print(g[[i]])
}

#--------#
# Fêmeas #
#--------#