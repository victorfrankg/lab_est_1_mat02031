require(readxl)
require(dplyr)
require(caret)
require(here)


# Importando Banco --------------------------------------------------------

df = read_excel(here('Assessoria 05 - Adriana','Bancos','dados_adriana.xlsx'), na = 'NI', 
                col_types = c("skip", "date", "text", 
                              "text", "numeric", "text", "text", 
                              "numeric", "skip", "skip", "numeric", 
                              "numeric", "numeric"))


# Criando novas colunas ---------------------------------------------------

dv = dummyVars('~Sinais',df)  # fç para criar novas colunas a partir de Sinais
df = dv %>% predict(df) %>% # one hot encoding
  cbind(df[-6],.) %>% # juntando ao banco original e retirando uma coluna
  rename_with( ~(gsub("[.]", " ", .x))) %>% # tirando ponto dos nomes das colunas 
  mutate(across(5:16, factor)) # transformando em factors


# Salvando banco ----------------------------------------------------------


saveRDS(df, here('Assessoria 05 - Adriana','Rotinas','df_adriana.RDS'))



