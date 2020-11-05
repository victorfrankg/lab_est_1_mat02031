require(readxl)
require(dplyr)
require(caret)
require(here)


# Importando Banco --------------------------------------------------------

df = merge(read_excel(here('Assessoria 05 - Adriana','Bancos','dados_adriana.xlsx'), na = 'NI', 
                      col_types = c("text", "date", "text", 
                                    "text", "numeric", "text", "text", 
                                    "numeric", "skip", "skip", "numeric", 
                                    "numeric", "numeric")),
           read_excel(here('Assessoria 05 - Adriana','Bancos','dados_adriana.xlsx'), sheet = 2))


# Criando novas colunas ---------------------------------------------------

dv = dummyVars('~Sinais',df)  # fç para criar novas colunas a partir de Sinais
df = dv %>% predict(df) %>% # one hot encoding
  cbind(df[-c(1,6)],.) %>% # juntando ao banco original e retirando uma coluna
  rename_with( ~(gsub("[.]", " ", .x))) %>% # tirando ponto dos nomes das colunas 
  mutate(across(5:16, factor), 
         Idade = cut(Idade, breaks = c(0,12,24,36,Inf), labels = c('0 a 12','13 a 24','25 a 36','37 a Inf'))) # transformando em factors


# Salvando banco ----------------------------------------------------------


saveRDS(df, here('Assessoria 05 - Adriana','Bancos','df_adriana.RDS'))



