# ==============================================================================
# AULA 2 - INTRODUÇÃO À LINGUAGEM DE PROGRAMAÇÃO R
# Curso: Análise de Dados para Pesquisa no SUS
# Script: Iniciando no R
# ==============================================================================

# ------------------------------------------------------------------------------
# SEÇÃO 1: OPERAÇÕES BÁSICAS EM R
# ------------------------------------------------------------------------------

# 1.1 Operações aritméticas simples
1 + 2
# [1] 3

#Teste trocar o + por -, *, /, para subtrair, multiplicar ou dividir respectivamente. 

# 1.2 Criando variáveis (objetos)
a <- 10
b <- 20

# Visualizar valores
print(a)
# [1] 10

print(b)
# [1] 20

# 1.3 R é case-sensitive (diferencia maiúsculas e minúsculas)
d <- 35
D <- 55

print(d)

print(D)

# ------------------------------------------------------------------------------
# SEÇÃO 2: TIPOS DE DADOS
# ------------------------------------------------------------------------------

# 2.1 Tipos básicos
numero_inteiro <- 18              # Inteiro
numero_real <- 18.5                # Numérico (real)
texto <- "Saúde Pública"           # Caractere (string)
logico <- TRUE                     # Lógico (booleano)

# Verificar tipo de dado
class(numero_inteiro)
# [1] "integer"

class(numero_real)
# [1] "numeric"

class(texto)
# [1] "character"

class(logico)
# [1] "logical"

# 2.2 Convertendo tipos de dados
# Converter número real para inteiro (o valor é arredondado)
a <- 15.6
class(a)

a <- as.integer(a)
class(a) # Nota: 15.6 foi arredondado para 15

# Converter texto para numérico
b <- "25"

class(b) ## Verifique a classe de b

as.numeric(b)

class(b) ## Após rodar o comando acima, verifique novamente a classe

# 2.3 Outros tipos de objetos
# Vetores - armazenam múltiplos valores do mesmo tipo
idades <- c(25, 30, 45, 52, 68)
print(idades)
# [1] 25 30 45 52 68

# Fatores - usados para variáveis categóricas
sexo <- factor(c("Masculino", "Feminino", "Feminino", "Masculino"))
print(sexo)
# [1] Masculino Feminino  Feminino  Masculino
# Levels: Feminino Masculino

# Matrizes - estruturas bidimensionais (linhas e colunas)
matriz <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2, ncol = 3)
print(matriz)
#      [,1] [,2] [,3]
# [1,]    1    3    5
# [2,]    2    4    6


# ------------------------------------------------------------------------------
# SEÇÃO 3: FUNÇÕES BÁSICAS
# ------------------------------------------------------------------------------

# 3.1 Função sum() - somar valores
sum(1, 3)
# [1] 4

sum(10, 20, 30)
# [1] 60

# 3.2 Função sqrt() - raiz quadrada
sqrt(16)
# [1] 4

# 3.3 Verificar se um valor é numérico
is.numeric(10)
# [1] TRUE

is.numeric("texto")
# [1] FALSE


# ------------------------------------------------------------------------------
# FIM DO SCRIPT
# ------------------------------------------------------------------------------

