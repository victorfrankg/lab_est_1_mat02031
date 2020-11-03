require(ggplot2)
require(Hmisc)
require(summarytools)
require(compareGroups)
require(incidence)

# Análise Descritiva ------------------------------------------------------

dfSummary(df)


# Visualização Gráfica ----------------------------------------------------

# número de casos por município

ggplot(df,aes(x = Raiva, fill = Municipio))+
  geom_bar(position=position_dodge())+
  theme_minimal()+
  geom_text(aes(label=stat(count)),stat='count',vjust = - 0.3,
            position = position_dodge(width = 0.9))+
  facet_wrap(~Municipio)+
  theme(legend.position="none")

# número de casos por sexo

ggplot(df, aes(x = Raiva, fill = Sexo))+
  geom_bar(position=position_dodge())+
  geom_text(aes(label=stat(count)),stat='count',vjust = - 0.3,
            position = position_dodge(width = 0.9))

# número de casos por idade

ggplot(df, aes(x = Idade, fill = Raiva))+
  geom_bar(position=position_dodge())+
  geom_text(aes(label=stat(count)),stat='count',vjust = - 0.3,
            position = position_dodge(width = 0.9))+
  scale_x_continuous(n.breaks = 40)
  
