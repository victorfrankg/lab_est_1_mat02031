require(ggplot2)
require(dplyr)
require(summarytools)
require(compareGroups)
require(incidence)
require(scales)
require(webr)

# Análise Descritiva ------------------------------------------------------

dfSummary(df) # apenas para nossa visualização


# Visualização  ----------------------------------------------------

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
  geom_bar(position=position_dodge(), color = 'black')+
  theme_minimal()+
  geom_text(aes(label=stat(count)),stat='count',vjust = - 0.3,
            position = position_dodge(width = 0.9))+
  lims(y = c(0,30))+
  labs(y = 'Contagem')+
  scale_fill_discrete(labels = c('Ausente','Presente'))

# número de casos por idade

ggplot(df, aes(x = Idade, fill = Raiva))+
  geom_bar(position=position_dodge(), colour = 'black')+
  theme_minimal()+
  geom_text(aes(label=stat(count)),stat='count',vjust = - 0.3,
            position = position_dodge(width = 0.9))+
  labs(y = 'Contagem')+
  lims(y = c(0,15))+
  scale_fill_discrete(labels = c('Ausente','Presente'))

# número de casos por origem

ggplot(df, aes(x = Origem, fill = Raiva))+
  geom_bar(position=position_dodge(), colour = 'black')+
  theme_minimal()+
  geom_text(aes(label=stat(count)),stat='count',vjust = - 0.3,
            position = position_dodge(width = 0.9))+
  labs(y = 'Contagem')+
  lims(y = c(0,40))+
  scale_fill_discrete(labels = c('Ausente','Presente'))

# número de casos por Sinais

ggplot(df, aes(x = SinaisA, fill = Raiva))+
  geom_bar(position=position_dodge(), colour = 'black')+
  theme_minimal()+
  geom_text(aes(label=stat(count)),stat='count',vjust = - 0.3,
            position = position_dodge(width = 0.9))+
  labs(y = 'Contagem')+
  lims(y = c(0,30))+
  scale_fill_discrete(labels = c('Ausente','Presente'))+
  scale_x_discrete(labels = c('Ausente','Presente','NA'))

# ...
ggplot(df, aes(x = SinaisAB, fill = Raiva))+
  geom_bar(position=position_dodge(), colour = 'black')+
  theme_minimal()+
  geom_text(aes(label=stat(count)),stat='count',vjust = - 0.3,
            position = position_dodge(width = 0.9))+
  labs(y = 'Contagem')+
  lims(y = c(0,40))+
  scale_fill_discrete(labels = c('Ausente','Presente'))+
  scale_x_discrete(labels = c('Ausente','Presente','NA'))

# ...
ggplot(df, aes(x = SinaisABC, fill = Raiva))+
  geom_bar(position=position_dodge(), colour = 'black')+
  theme_minimal()+
  geom_text(aes(label=stat(count)),stat='count',vjust = - 0.3,
            position = position_dodge(width = 0.9))+
  labs(y = 'Contagem')+
  lims(y = c(0,40))+
  scale_fill_discrete(labels = c('Ausente','Presente'))+
  scale_x_discrete(labels = c('Ausente','Presente','NA'))

#...
ggplot(df, aes(x = SinaisAC, fill = Raiva))+
  geom_bar(position=position_dodge(), colour = 'black')+
  theme_minimal()+
  geom_text(aes(label=stat(count)),stat='count',vjust = - 0.3,
            position = position_dodge(width = 0.9))+
  labs(y = 'Contagem')+
  lims(y = c(0,40))+
  scale_fill_discrete(labels = c('Ausente','Presente'))+
  scale_x_discrete(labels = c('Ausente','Presente','NA'))

#...
ggplot(df, aes(x = SinaisB, fill = Raiva))+
  geom_bar(position=position_dodge(), colour = 'black')+
  theme_minimal()+
  geom_text(aes(label=stat(count)),stat='count',vjust = - 0.3,
            position = position_dodge(width = 0.9))+
  labs(y = 'Contagem')+
  lims(y = c(0,30))+
  scale_fill_discrete(labels = c('Ausente','Presente'))+
  scale_x_discrete(labels = c('Ausente','Presente','NA'))

#...
ggplot(df, aes(x = SinaisBC, fill = Raiva))+
  geom_bar(position=position_dodge(), colour = 'black')+
  theme_minimal()+
  geom_text(aes(label=stat(count)),stat='count',vjust = - 0.3,
            position = position_dodge(width = 0.9))+
  labs(y = 'Contagem')+
  lims(y = c(0,40))+
  scale_fill_discrete(labels = c('Ausente','Presente'))+
  scale_x_discrete(labels = c('Ausente','Presente','NA'))

#...
ggplot(df, aes(x = SinaisC, fill = Raiva))+
  geom_bar(position=position_dodge(), colour = 'black')+
  theme_minimal()+
  geom_text(aes(label=stat(count)),stat='count',vjust = - 0.3,
            position = position_dodge(width = 0.9))+
  labs(y = 'Contagem')+
  lims(y = c(0,40))+
  scale_fill_discrete(labels = c('Ausente','Presente'))+
  scale_x_discrete(labels = c('Ausente','Presente','NA'))


# Incidência por Mês/Semana

df[which(df$Raiva == 1),1] %>%
  incidence(., interval = '30',first_date = '2018-01-01',last_date = '2018-12-31',standard = F) %>% 
  plot(., color = 'black',n_breaks = nrow(.),border = 'white')+
  theme_minimal()%+replace%
  theme(axis.text.x = element_text(angle = 45, hjust = 0.8, size = 10))

df[which(df$Raiva == 1),1] %>%
  incidence(., interval = 'week',first_date = '2018-01-01',last_date = '2018-12-31',standard = F) %>% 
  plot(., color = 'black',n_breaks = nrow(.)/3,border = 'white')+
  theme(axis.text.x = element_text(angle = 45, hjust = 0.8, size = 10))

df[which(df$Babesia == 1),1] %>%
  incidence(., interval = '30') %>% 
  plot(., color = 'black',n_breaks = nrow(.),border = 'white')+
  theme_minimal()%+replace%
  theme(axis.text.x = element_text(angle = 45, hjust = 0.8, size = 10))

