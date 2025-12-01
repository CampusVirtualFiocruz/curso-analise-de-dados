# ==============================================================================
# MÓDULO 1 - AULA 2: INTRODUÇÃO À LINGUAGEM DE PROGRAMAÇÃO R
# Curso: Introdução à Análise de Dados para Pesquisa no SUS
# Script: Manipulação de Dados 
# ==============================================================================

# ------------------------------------------------------------------------------
# INSTALAÇÃO (executar apenas uma vez)
# ------------------------------------------------------------------------------

# OPÇÃO 1: Instalação usando Tidyverse (RECOMENDADO - mais simples)
# O Tidyverse já inclui vários pacotes essenciais (ATENÇÃO: Este processo pode demorar)
install.packages("tidyverse")  # Inclui: dplyr, stringr, lubridate, readr, ggplot2 e outros
install.packages("readxl")     # Leitura de arquivos Excel
install.packages("arrow")      # Leitura de arquivos Parquet

# OPÇÃO 2: Instalação individual de cada pacote
# Use esta opção se quiser instalar apenas pacotes específicos
install.packages("dplyr")      # Manipulação de dados
install.packages("stringr")    # Manipulação de strings/texto
install.packages("lubridate")  # Manipulação de datas
install.packages("readr")      # Leitura de arquivos CSV
install.packages("ggplot2")    # Criação de gráficos
install.packages("tibble")     # Trabalhar com tibbles (dataframes modernos)
install.packages("tidyr")      # Organização de dados
install.packages("readxl")     # Leitura de arquivos Excel
install.packages("arrow")      # Leitura de arquivos Parquet

# ------------------------------------------------------------------------------
# CARREGAMENTO (executar toda vez que abrir o R/RStudio)
# ------------------------------------------------------------------------------

# OPÇÃO 1: Carregamento usando Tidyverse (RECOMENDADO - mais simples)
library(tidyverse)   # Carrega automaticamente: dplyr, stringr, lubridate, 
# readr, ggplot2, tibble, tidyr, purrr e forcats
library(readxl)      # Leitura de arquivos Excel (.xlsx, .xls)
library(arrow)       # Leitura de arquivos Parquet

# OPÇÃO 2: Carregamento individual de cada pacote
# Use esta opção se quiser controlar exatamente quais pacotes estão ativos
library(dplyr)       # Funções: mutate, filter, select, group_by, summarise, etc.
library(stringr)     # Funções: str_sub, str_replace, str_detect, etc.
library(lubridate)   # Funções: dmy, ymd, month, year, day, etc.
library(readr)       # Funções: read_csv, write_csv
library(ggplot2)     # Funções: ggplot, geom_col, geom_point, etc.
library(tibble)      # Funções: tibble, as_tibble
library(tidyr)       # Funções: pivot_wider, pivot_longer
library(readxl)      # Funções: read_excel
library(arrow)       # Funções: read_parquet, write_parquet

# ------------------------------------------------------------------------------
# RESUMO DOS PACOTES E SUAS FUNÇÕES PRINCIPAIS
# ------------------------------------------------------------------------------

# tidyverse (meta-pacote que inclui 8 pacotes principais):
#   ├── dplyr      → Manipulação de dados
#   ├── stringr    → Manipulação de strings
#   ├── lubridate  → Manipulação de datas
#   ├── readr      → Leitura/escrita de arquivos CSV
#   ├── ggplot2    → Criação de gráficos
#   ├── tibble     → Dataframes modernos
#   ├── tidyr      → Organização de dados
#   └── purrr      → Programação funcional

# Pacotes adicionais (não incluídos no tidyverse):
#   ├── readxl     → Leitura de arquivos Excel
#   └── arrow      → Leitura de arquivos Parquet

# ------------------------------------------------------------------------------
# VERIFICAR PACOTES CARREGADOS
# ------------------------------------------------------------------------------

# Ver quais pacotes estão atualmente carregados
search()

# Ver a versão de um pacote específico
packageVersion("tidyverse")
packageVersion("dplyr")

 # ------------------------------------------------------------------------------
 # CONFIGURAÇÃO INICIAL
 # ------------------------------------------------------------------------------
 
 # Definir diretório de trabalho
 # Altere para o caminho correto no seu computador
 setwd("C:/caminho/para/sua/pasta")
 
 # Verificar diretório atual
 getwd()
 
 
# ------------------------------------------------------------------------------
# SEÇÃO 4: IMPORTAÇÃO DE DADOS
# ------------------------------------------------------------------------------

# 4.1 Importar arquivo CSV
df_csv <- read_csv("sim_salvador_2023.csv")
 # df_csv <- read_csv("C:/caminho/para/sua/pasta/sim_salvador_2023.csv") <-- você pode escrever assim também
 
# 4.2 Importar arquivo Excel
df_xlsx <- read_excel("sim_salvador_2023.xlsx")

# 4.3 Importar arquivo Parquet
df_parquet <- read_parquet("sim_salvador_2023.parquet")

# 4.4 Remover dataframes duplicados da memória
rm(df_xlsx, df_parquet)

# ------------------------------------------------------------------------------
# SEÇÃO 5: EXPLORAÇÃO INICIAL DOS DADOS
# ------------------------------------------------------------------------------

# 5.1 Visualizar estrutura dos dados
glimpse(df_csv)
# Saída mostra: nome das variáveis, tipo e exemplos de valores

# 5.2 Visualizar primeiras linhas
head(df_csv)

# 5.3 Visualizar últimas linhas
tail(df_csv)

# 5.4 Resumo estatístico
summary(df_csv)

# ------------------------------------------------------------------------------
# SEÇÃO 6: ANÁLISE DE FREQUÊNCIAS
# ------------------------------------------------------------------------------

# 6.1 Usando table() do R base
table(df_csv$SEXO, useNA = "always")


# 6.2 Usando count() do Tidyverse (mais intuitivo)
df_csv %>% 
  count(SEXO, sort = TRUE)


# ------------------------------------------------------------------------------
# SEÇÃO 7: CRIAÇÃO E MODIFICAÇÃO DE VARIÁVEIS
# ------------------------------------------------------------------------------

# 7.1 Criar variável SEXO padronizada usando if_else
df_csv <- df_csv %>%
  mutate(
    sexo_p = if_else(SEXO == 1, "Masculino",
                     if_else(SEXO == 2, "Feminino", 
                             if_else(SEXO == 0, "Ignorado", NA_character_)))
  )

# Verificar resultado
# Verificar resultado (mesmo resultado do if_else)
df_csv %>% count(SEXO) ##Antes: variável SEXO original (valores numéricos: 0, 1, 2)
df_csv %>% count(sexo_p) ##Depois: variável sexo_p transformada (valores em texto)


# 7.2 Alternativa usando case_when (mais limpo para múltiplas condições)
df_csv <- df_csv %>%
  mutate(
    sexo_2 = case_when(
      SEXO == 1 ~ "Masculino",
      SEXO == 2 ~ "Feminino",
      SEXO == 0 ~ "Ignorado",
      is.na(SEXO) ~ NA_character_
    )
  )

# Verificar resultado (mesmo resultado do if_else)
df_csv %>% count(SEXO) ##Antes: variável SEXO original (valores numéricos: 0, 1, 2)
df_csv %>% count(sexo_2) ##Depois: variável sexo_p transformada (valores em texto)

# ------------------------------------------------------------------------------
# SEÇÃO 8: MANIPULAÇÃO DE DATAS
# ------------------------------------------------------------------------------

# 8.1 Converter variável de data (formato ddmmyyyy para Date)
df_csv <- df_csv %>%
  mutate(
    DTOBITO_dt = dmy(DTOBITO)  # dmy = dia-mês-ano
  )

# 8.2 Extrair ano da data
df_csv <- df_csv %>%
  mutate(
    ano_obito = year(DTOBITO_dt)
  )

# 8.3 Verificar as transformações
glimpse(df_csv)

# 8.4 Contar óbitos por ano
df_csv %>% count(ano_obito)

# 8.5 Contar óbitos por sexo e ano
df_csv %>% count(sexo_p, ano_obito)

# ------------------------------------------------------------------------------
# SEÇÃO 9: MANIPULAÇÃO DE STRINGS (TEXTO)
# ------------------------------------------------------------------------------

# 9.1 Extrair partes de uma string
# A variável IDADE tem formato: primeiro dígito = tipo (ex: dias, meses ou anos), demais = quantidade
# Exemplo: "204" = 4 dias, "305" = 5 meses, "450" = 50 anos

# Extrair primeiro caractere (tipo de idade)
df_csv <- df_csv %>%
  mutate(
    tipo_idade = str_sub(IDADE, 1, 1),    # Posição 1 até 1
    idade = str_sub(IDADE, 2)              # Posição 2 até o final
  )

# Verificar resultado
df_csv %>%
  select(IDADE, tipo_idade, idade) %>% ##select seleciona apenas a variáveis dentro dos parênteses
  head() ##visualiza apenas o início da tabela



# ------------------------------------------------------------------------------
# SEÇÃO 10: MÚLTIPLAS TRANSFORMAÇÕES
# ------------------------------------------------------------------------------
# Regra de conversão:
# - Se tipo_idade 0 a 3 (minutos, horas, dias, meses) → considera 0 anos (< 1 ano)
# - Se tipo_idade 4 (já está em anos) → usa o valor direto
# - Se tipo_idade 5 (centenários) → soma 100 + valor

# 10.1 Aplicar todas as transformações de uma vez
df_csv <- df_csv %>%
  mutate(
    tipo_idade = str_sub(IDADE, 1, 1), # Extrai posição 1
    idade = str_sub(IDADE, 2), # Extrai da posição 2 em diante
    idade_anos = case_when(
      tipo_idade <= 3 ~ 0, # Menores de 1 ano
      tipo_idade == 4 ~ as.numeric(idade), #Já em anos
      tipo_idade == 5 ~ 100 + as.numeric(idade) # Centenários
    )
  )

# Verificar estrutura final
glimpse(df_csv)
# Verificar resultado
View(df_csv)

# ------------------------------------------------------------------------------
# SEÇÃO 11: AGRUPAMENTO E AGREGAÇÃO DE DADOS
# ------------------------------------------------------------------------------

# 11.1 Agrupar óbitos por mês
df_csv_agrupado <- df_csv %>%
  mutate(mes_obito = month(DTOBITO_dt, label = TRUE)) %>% #Passo 1: Extrair o mês da data de óbito: month() extrai o número do mês (1=Jan, 2=Fev, etc.), label=TRUE converte para nome abreviado (Jan, Fev, Mar, etc.)
  group_by(mes_obito) %>% # Passo 2: Agrupar os dados por mês, todas as linhas do mesmo mês serão agrupadas juntas
  summarise( # Passo 3: Calcular estatísticas para cada grupo (mês)
    total_obitos = n(),  # Conta o número de óbitos
    idade_media = mean(idade_anos, na.rm = TRUE) # Calcula a idade média
  )

View(df_csv_agrupado)


# 11.2 Agrupar por sexo e calcular estatísticas: outro exemplo

df_csv %>%
  group_by(sexo_p) %>%
  summarise(
    n = n(),
    idade_media = mean(idade_anos, na.rm = TRUE),
    idade_min = min(idade_anos, na.rm = TRUE),
    idade_max = max(idade_anos, na.rm = TRUE)
  )

# ------------------------------------------------------------------------------
# SEÇÃO 12: JOINS (JUNÇÕES DE TABELAS)
# ------------------------------------------------------------------------------

# Exemplo conceitual - ajuste conforme seus dados reais

# 12.1 Left Join - mantém todas as linhas da tabela à esquerda
# resultado <- df_sim %>%
#   left_join(df_populacao, by = "codigo_ibge")

# 12.2 Inner Join - mantém apenas linhas com correspondência
# resultado <- df_sim %>%
#   inner_join(df_populacao, by = "codigo_ibge")

# 12.3 Right Join - mantém todas as linhas da tabela à direita
# resultado <- df_sim %>%
#   right_join(df_populacao, by = "codigo_ibge")

# 12.4 Full Join - mantém todas as linhas de ambas as tabelas
# resultado <- df_sim %>%
#   full_join(df_populacao, by = "codigo_ibge")


# ------------------------------------------------------------------------------
# FIM DO SCRIPT
# ------------------------------------------------------------------------------
# Salvar o dataframe processado (opcional)
  write_csv(df_csv, "sim_salvador_2023_processado.csv")
# write_parquet(df_csv, "sim_salvador_2023_processado.parquet")

