require(ggplot2)
require(dplyr)
require(summarytools)
require(compareGroups)
require(incidence)
require(scales)
require(webr)

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

ggplot(df, aes(x = Sexo, fill = Raiva))+
  geom_bar(position=position_dodge())+
  geom_text(aes(label=stat(count)),stat='count',vjust = - 0.3,
            position = position_dodge(width = 0.9))+
  lims(y = c(0,30))

# número de casos por idade

ggplot(df, aes(x = Idade, fill = Raiva))+
  geom_bar(position=position_dodge())+
  geom_text(aes(label=stat(count)),stat='count',vjust = - 0.3,
            position = position_dodge(width = 0.9))+
  scale_x_continuous(n.breaks = 40)
  

tb = function(x){
  as.data.frame(table(x, useNA = 'ifany')/length(x)) 
}

# pie chart


ggplot(tst$Sexo, aes(x = '', y = Freq, fill = x))+
  geom_bar(width = 1, stat = 'identity')+
  coord_polar("y", start = 0)+
  theme_minimal()+
  theme(
    panel.border = element_blank(),
    panel.grid = element_blank(),
    axis.text.x = element_blank())+
  geom_text(aes(label = paste(round(Freq / sum(Freq) * 100, 1), "%")),
            position = position_stack(vjust = 0.5))+
  labs(x = 'teste')


g = compareGroups(Raiva ~ . - `Data da coleta`, data = df, method = c(Idade = NA))

pt = function(v1,v2){
  teste = data.frame(select(df,all_of(v1)),
                     select(df,all_of(v2)))
  colnames(teste)= c('x','y')
  ggplot(teste, aes(x = x, fill = y))+
    geom_bar(position=position_dodge())+
    geom_text(aes(label=stat(count)),stat='count',vjust = - 0.3,
              position = position_dodge(width = 0.9))+
    labs(x = eval(v1), fill = eval(v2), y = 'Contagem')+
    theme_minimal()
}

df %>% arrange(`Data da coleta`) %>% mutate(Soma = cumsum(as.numeric(as.character(Raiva))))%>%ggplot(aes(x = `Data da coleta`, y = Soma, col = Raiva))+geom_point()+theme_minimal()+lims(y=c(0,40))
