# ==============================================================================
# MÓDULO 1 - AULA 2: INTRODUÇÃO À LINGUAGEM DE PROGRAMAÇÃO R 
# Curso: Introdução à Análise de Dados para Pesquisa no SUS
# Script: Gabarito - ATIVIDADES PRÁTICAS AULA 2 
# ==============================================================================

# Carregar pacotes necessários
library(tidyverse)
library(lubridate)

# Assumindo que você já importou o dataframe df_csv conforme a aula
setwd("C:/caminho/para/sua/pasta")


df_csv <- read_csv("sim_salvador_2023_processado.csv")

# ==============================================================================
# ATIVIDADE 1: EXPLORAÇÃO E TRANSFORMAÇÃO DE DADOS
# ==============================================================================

# ------------------------------------------------------------------------------
# 1.1 Criar variável faixa_etaria
# ------------------------------------------------------------------------------

df_csv <- df_csv %>%
  mutate(
    faixa_etaria = case_when(
      idade_anos >= 0 & idade_anos <= 12 ~ "Criança",
      idade_anos >= 13 & idade_anos <= 17 ~ "Adolescente",
      idade_anos >= 18 & idade_anos <= 59 ~ "Adulto",
      idade_anos >= 60 ~ "Idoso",
      TRUE ~ NA_character_  # Para valores ausentes ou fora do esperado
    )
  )

# Verificar a criação da variável
glimpse(df_csv)


# ------------------------------------------------------------------------------
# 1.2 Contar óbitos por faixa etária
# ------------------------------------------------------------------------------

# Opção 1: Usando count()
df_csv %>%
  count(faixa_etaria, sort = TRUE)
 

# Opção 2: Usando group_by() + summarise()
df_csv %>%
  group_by(faixa_etaria) %>%
  summarise(
    total_obitos = n()
  ) %>%
  arrange(desc(total_obitos))

# ==============================================================================
# ATIVIDADE 2: MANIPULAÇÃO DE DATAS E AGRUPAMENTO
# ==============================================================================

# ------------------------------------------------------------------------------
# 2.1 Criar variável trimestre
# ------------------------------------------------------------------------------

df_csv <- df_csv %>%
  mutate(
    mes_numero = month(DTOBITO_dt),
    trimestre = case_when(
      mes_numero %in% c(1, 2, 3) ~ "1º Trimestre",
      mes_numero %in% c(4, 5, 6) ~ "2º Trimestre",
      mes_numero %in% c(7, 8, 9) ~ "3º Trimestre",
      mes_numero %in% c(10, 11, 12) ~ "4º Trimestre",
      TRUE ~ NA_character_
    )
  )

# Verificar a criação
df_csv %>%
  count(trimestre)

# ------------------------------------------------------------------------------
# 2.2 Calcular total de óbitos e idade média por trimestre e sexo
# ------------------------------------------------------------------------------

df_csv %>%
  group_by(trimestre, sexo_p) %>%
  summarise(
    total_obitos = n(),
    idade_media = mean(idade_anos, na.rm = TRUE),
    .groups = "drop"  # Remove agrupamento após summarise
  ) %>%
  arrange(trimestre, desc(total_obitos))



# Visualização alternativa com pivot_wider para facilitar comparação
df_csv %>%
  group_by(trimestre, sexo_p) %>%
  summarise(
    total_obitos = n(),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = sexo_p,
    values_from = total_obitos
  )



# ==============================================================================
# ATIVIDADE 3: ANÁLISE INTEGRADA
# ==============================================================================

# ------------------------------------------------------------------------------
# 3.1 Identificar o mês com maior número de óbitos
# ------------------------------------------------------------------------------

df_csv %>%
  mutate(mes_obito = month(DTOBITO_dt, label = TRUE, abbr = FALSE)) %>%
  count(mes_obito, sort = TRUE) %>%
  slice(1)  # Pega apenas a primeira linha (maior valor)



# Alternativa mostrando os top 3 meses
df_csv %>%
  mutate(mes_obito = month(DTOBITO_dt, label = TRUE, abbr = FALSE)) %>%
  count(mes_obito, sort = TRUE) %>%
  slice(1:3)




# ------------------------------------------------------------------------------
# 3.2 Calcular diferença percentual entre óbitos masculinos e femininos
# ------------------------------------------------------------------------------

# Opção 1: Diferença percentual simples
df_csv %>%
  filter(sexo_p %in% c("Masculino", "Feminino")) %>%
  count(sexo_p) %>%
  mutate(
    percentual = (n / sum(n)) * 100,
    percentual = round(percentual, 2)
  )


# Opção 2: Calculando a diferença absoluta em pontos percentuais
diff_percentual <- df_csv %>%
  filter(sexo_p %in% c("Masculino", "Feminino")) %>%
  count(sexo_p) %>%
  mutate(percentual = (n / sum(n)) * 100) %>%
  summarise(
    diferenca_pp = max(percentual) - min(percentual),  # Diferença em pontos percentuais
    razao = max(n) / min(n)  # Razão entre masculino e feminino
  )

print(diff_percentual)


# ------------------------------------------------------------------------------
# 3.3 Determinar qual faixa etária teve mais óbitos
# ------------------------------------------------------------------------------

df_csv %>%
  count(faixa_etaria, sort = TRUE) %>%
  slice(1)



# Alternativa com percentual
df_csv %>%
  count(faixa_etaria, sort = TRUE) %>%
  mutate(
    percentual = (n / sum(n)) * 100,
    percentual = round(percentual, 2)
  )


# Interpretação: Idosos representam 68.3% de todos os óbitos


# ==============================================================================
# ANÁLISES COMPLEMENTARES (BÔNUS)
# ==============================================================================

# Análise 1: Óbitos por faixa etária e sexo
df_csv %>%
  filter(sexo_p %in% c("Masculino", "Feminino")) %>%
  count(faixa_etaria, sexo_p) %>%
  pivot_wider(
    names_from = sexo_p,
    values_from = n
  ) %>%
  mutate(
    total = Masculino + Feminino,
    prop_masculino = round((Masculino / total) * 100, 1)
  )



# Análise 2: Distribuição de óbitos ao longo do ano
df_csv %>%
  mutate(
    mes = month(DTOBITO_dt, label = TRUE),
    trimestre = case_when(
      month(DTOBITO_dt) %in% 1:3 ~ "1º Tri",
      month(DTOBITO_dt) %in% 4:6 ~ "2º Tri",
      month(DTOBITO_dt) %in% 7:9 ~ "3º Tri",
      month(DTOBITO_dt) %in% 10:12 ~ "4º Tri"
    )
  ) %>%
  group_by(trimestre) %>%
  summarise(
    total = n(),
    idade_media = round(mean(idade_anos, na.rm = TRUE), 1),
    idade_min = min(idade_anos, na.rm = TRUE),
    idade_max = max(idade_anos, na.rm = TRUE)
  )


# Análise 3: Top 5 faixas etárias mais afetadas (detalhado)
df_csv %>%
  mutate(
    faixa_5anos = cut(
      idade_anos,
      breaks = seq(0, 120, by = 5),
      include.lowest = TRUE,
      right = FALSE
    )
  ) %>%
  count(faixa_5anos, sort = TRUE) %>%
  head(5)


# ==============================================================================
# VISUALIZAÇÕES (OPCIONAL - PARA APROFUNDAMENTO)
# ==============================================================================

# Gráfico 1: Óbitos por mês
df_csv %>%
  mutate(mes = month(DTOBITO_dt, label = TRUE)) %>%
  count(mes) %>%
  ggplot(aes(x = mes, y = n)) +
  geom_col(fill = "steelblue") +
  geom_text(aes(label = n), vjust = -0.5, size = 3) +
  labs(
    title = "Distribuição de Óbitos por Mês - Salvador 2023",
    x = "Mês",
    y = "Número de Óbitos"
  ) +
  theme_minimal()

# Gráfico 2: Distribuição de idade por sexo
df_csv %>%
  filter(sexo_p %in% c("Masculino", "Feminino")) %>%
  ggplot(aes(x = idade_anos, fill = sexo_p)) +
  geom_histogram(bins = 30, alpha = 0.6, position = "identity") +
  labs(
    title = "Distribuição de Idade dos Óbitos por Sexo",
    x = "Idade (anos)",
    y = "Frequência",
    fill = "Sexo"
  ) +
  theme_minimal()

# Gráfico 3: Óbitos por faixa etária e sexo
df_csv %>%
  filter(sexo_p %in% c("Masculino", "Feminino")) %>%
  count(faixa_etaria, sexo_p) %>%
  ggplot(aes(x = faixa_etaria, y = n, fill = sexo_p)) +
  geom_col(position = "dodge") +
  geom_text(
    aes(label = n),
    position = position_dodge(width = 0.9),
    vjust = -0.5,
    size = 3
  ) +
  labs(
    title = "Óbitos por Faixa Etária e Sexo - Salvador 2023",
    x = "Faixa Etária",
    y = "Número de Óbitos",
    fill = "Sexo"
  ) +
  theme_minimal()


# ==============================================================================
# EXPORTAR RESULTADOS (OPCIONAL)
# ==============================================================================

# Salvar resumo das análises em CSV
resumo_atividades <- df_csv %>%
  group_by(trimestre, faixa_etaria, sexo_p) %>%
  summarise(
    total_obitos = n(),
    idade_media = round(mean(idade_anos, na.rm = TRUE), 1),
    .groups = "drop"
  )

write_csv(resumo_atividades, "resumo_atividades_aula2.csv")

# Mensagem final
cat("\n✓ Gabarito das atividades concluído!\n")
cat("✓ Todas as análises foram executadas com sucesso.\n")
cat("✓ Explore os gráficos e resultados gerados.\n")






