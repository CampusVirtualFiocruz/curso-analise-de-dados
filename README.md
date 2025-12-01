# MÓDULO 1 - AULA 2 - INTRODUÇÃO À LINGUAGEM R

Este repositório contém os materiais práticos da Aula 2 do curso de Introdução à Análise de Dados para pesquisa no SUS

---

## 📁 CONTEÚDO

### SCRIPTS R

- `modulo1aula2_script_1.R` - Primeiro script da aula introduzindo operações básicas em R
- `modulo1aula2_script_2.R` - Segundo script da aula introduzindo manipulação de dados em R
- `modulo1aula2_atividades.R` - Atividades práticas para fixação do conteúdo e conceitos adicionais

### ATIVIDADES

- `modulo1aula2_atividades.pdf` - Descrição das atividades propostas 
- `modulo1aula2_gabarito_atividade.pdf` - Gabarito das atividades práticas

> **OBSERVAÇÃO:** Tente criar o seu código para encontrar as respostas da atividade, mas caso tenha dificuldades o gabarito em R (`modulo1aula2_atividades.R`) encontra-se na pasta.

### DADOS

A pasta `dados/` contém os arquivos de dados utilizados nas atividades:

- `sim_salvador_2023.csv` - Dados do Sistema de Informações sobre Mortalidade de Salvador (2023)
- `sim_salvador_2023.parquet` - Mesmo dataset em formato Parquet (otimizado)
- `sim_salvador_2023.xlsx` - Mesmo dataset em formato Excel
- `sim_salvador_2023_processado.csv` - Dataset processado e transformado
- `dicionario_sim.pdf` - Dicionário de variáveis do SIM

---

## 🎯 OBJETIVOS DA AULA

As atividades práticas abordam:

### 1. CONCEITOS BÁSICOS
- Operações básicas no R

### 2. EXPLORAÇÃO E TRANSFORMAÇÃO DE DADOS
- Criação de variáveis categóricas (faixa etária)
- Contagem e agregação de dados
- Manipulação com tidyverse

### 3. ANÁLISE ESTATÍSTICA
- Estatísticas descritivas
- Agrupamento de dados
- Uso de funções do pacote dplyr

### 4. MANIPULAÇÃO DE DATAS
- Trabalho com o pacote lubridate
- Transformação de variáveis temporais

---

## 📊 FONTE DOS DADOS

Os dados utilizados são provenientes do **Sistema de Informações sobre Mortalidade (SIM)** de Salvador, referentes ao ano de 2023. Para entender as variáveis disponíveis, consulte o arquivo `dicionario_sim.pdf` na pasta `dados/`.

### ARQUIVOS PRINCIPAIS:
- `sim_salvador_2023.csv` (dados brutos do SIM)
- `sim_salvador_2023.xlsx` (formato alternativo)
- `sim_salvador_2023.parquet` (formato otimizado)

### ARQUIVO PROCESSADO:
- `sim_salvador_2023_processado.csv` (gerado pelo Script 2, para realizar o script 3 e 4)

### ESTRUTURA DO DATASET:

**Variáveis principais:**
- **SEXO:** categórica (0=Ignorado, 1=Masculino, 2=Feminino)
- **DTOBITO:** data do óbito (formato ddmmyyyy)
- **IDADE:** idade codificada do DATASUS
  - 1º dígito: tipo (0-3: menos de 1 ano, 4: anos, 5: centenários)
  - Demais dígitos: quantidade
- **DTNASC:** data de nascimento
- **CAUSABAS:** causa básica do óbito (CID-10)
- **CODMUNRES:** código IBGE do município de residência

---

## 🚀 COMO UTILIZAR

1. Certifique-se de ter o R e o RStudio instalados
2. Instale os pacotes necessários
3. Defina o diretório de trabalho para esta pasta
4. Execute os scripts na ordem sugerida
5. Consulte o gabarito após tentar resolver as atividades

```r
setwd("caminho/para/repositório")
```

> **OBSERVAÇÃO:** Lembre-se de ajustar o caminho do diretório de trabalho (`setwd()`) nos scripts para corresponder à localização dos arquivos no seu computador.

---

## 📝 ESTRUTURA DAS ATIVIDADES

As atividades práticas incluem exercícios de:
- Criação de variáveis derivadas usando `mutate()` e `case_when()`
- Contagem e sumarização de dados com `count()` e `group_by()`
- Análise exploratória de dados de mortalidade
- Transformação e limpeza de dados
- Explorando bibliotecas de visualização de dados (material bônus)

---

## 📚 RECURSOS ADICIONAIS

- Pratique R respondendo as perguntas da atividade complementar opcional (`aula2_gabarito_atividade.pdf`) 
- Consulte o gabarito (`aula2_gabarito_atividade.pdf`) para verificar suas respostas
- O dicionário de dados explica cada variável do dataset SIM

---

## 🔗 MATERIAL DE APOIO

### DOCUMENTAÇÃO OFICIAL
- [R Project](https://www.r-project.org/)
- [RStudio/Posit](https://posit.co/)
- [Tidyverse](https://www.tidyverse.org/)
- [ggplot2](https://ggplot2.tidyverse.org/)
- [plotly R](https://plotly.com/r/)
- [dplyr](https://dplyr.tidyverse.org/)

### LIVROS GRATUITOS
- [R for Data Science (Hadley Wickham)](https://r4ds.hadley.nz/)
- [ggplot2: Elegant Graphics for Data Analysis](https://ggplot2-book.org/)
- [Fundamentals of Data Visualization (Claus Wilke)](https://clauswilke.com/dataviz/)

### TUTORIAIS INTERATIVOS
- [RStudio Primers](https://posit.cloud/learn/primers)
- Swirl (pacote R): aprender R dentro do R
- DataCamp: cursos introdutórios gratuitos

### COMUNIDADES
- [RStudio Community](https://community.rstudio.com/)
- [Stack Overflow - R](https://stackoverflow.com/questions/tagged/r)
- [R-Bloggers](https://www.r-bloggers.com/)
- Twitter: #RStats, #TidyTuesday

### GALERIAS DE VISUALIZAÇÃO
- [R Graph Gallery](https://r-graph-gallery.com/)
- [plotly Gallery](https://plotly.com/r/)
- [From Data to Viz](https://www.data-to-viz.com/)
- [Top 50 ggplot2 Visualizations](http://r-statistics.co/)

### DADOS DE SAÚDE PÚBLICA
- [DATASUS](https://datasus.saude.gov.br/)
- TabNet: estatísticas interativas
- [OpenDataSUS](https://opendatasus.saude.gov.br/)
- Observatório COVID-19 Fiocruz
- Portal Brasileiro de Dados Abertos

### CURSOS ONLINE
- Coursera: R Programming (Johns Hopkins)
- edX: Data Science with R
- Curso-R: cursos em português
- YouTube: canais educacionais em R

### CHEAT SHEETS (FOLHAS DE COLA)
- RStudio IDE Cheat Sheet
- dplyr Data Transformation
- ggplot2 Data Visualization
- lubridate Dates and Times
- stringr String Manipulation

Disponíveis em: [https://posit.co/resources/cheatsheets/](https://posit.co/resources/cheatsheets/)

### DATASETS DE EXEMPLO
- Dados do SIM Salvador 2023 (fornecidos)
- Datasets do pacote datasets (incluído no R)
- tidyverse: mpg, diamonds, etc.

---

## 🔧 SOLUÇÃO DE PROBLEMAS COMUNS

### PROBLEMA: Pacote não instala
**SOLUÇÃO:**
- Verificar conexão com internet
- Usar `install.packages("nome", dependencies = TRUE)`
- Atualizar o R e RStudio
- Verificar permissões do sistema

### PROBLEMA: Erro ao importar dados
**SOLUÇÃO:**
- Verificar caminho do arquivo com `getwd()`
- Usar `setwd()` para mudar diretório
- Verificar separador (vírgula vs ponto-e-vírgula)
- Verificar encoding do arquivo

### PROBLEMA: Gráfico não aparece
**SOLUÇÃO:**
- Usar `print()` para objetos ggplot
- Verificar se RStudio está atualizado
- Limpar painel de gráficos
- Salvar e reabrir o script

### PROBLEMA: Erro "object not found"
**SOLUÇÃO:**
- Verificar nome do objeto (case-sensitive)
- Executar linhas anteriores que criam o objeto
- Verificar se pacote está carregado (`library()`)
- Reiniciar sessão R se necessário

### PROBLEMA: Lentidão ao processar dados
**SOLUÇÃO:**
- Filtrar dados desnecessários
- Usar formatos eficientes (parquet, arrow)
- Aumentar memória disponível
- Trabalhar com amostras durante desenvolvimento

---

## ✨ BOAS PRÁTICAS DE PROGRAMAÇÃO

### ORGANIZAÇÃO DE CÓDIGO
- Comentar código explicando "por quê", não "o quê"
- Usar nomes descritivos de variáveis
- Dividir código em seções lógicas
- Usar pipe `%>%` para encadear operações
- Limitar linhas a ~80 caracteres

### ESTILO DE CÓDIGO
- Seguir guia de estilo tidyverse
- Usar `snake_case` para nomes
- Espaços ao redor de operadores
- Indentação consistente (2 espaços)
- Uma linha por pipe `%>%`

### REPRODUTIBILIDADE
- Salvar versão dos pacotes usados
- Documentar sessão R (`sessionInfo()`)
- Usar projetos do RStudio (`.Rproj`)
- Controle de versão com Git (avançado)
- Compartilhar código e dados

### GERENCIAMENTO DE DADOS
- Nunca modificar dados originais
- Salvar dados processados separadamente
- Documentar transformações realizadas
- Usar formatos abertos (CSV, não XLS)
- Fazer backup regular

---
### COMO CITAR
```
Introdução a Análise de dados para Pesquisa no SUS. (2025)- Scripts de R. Rio de Janeiro: Campus Virtual.
```

---

## 🙏 AGRADECIMENTOS E CRÉDITOS

Este material foi desenvolvido para o curso "Introdução a Análise de dados para Pesquisa no SUS" com o objetivo de capacitar profissionais de saúde pública em análise de dados usando R.

**INSPIRAÇÕES:**
- R for Data Science (Hadley Wickham & Garrett Grolemund)
- Tidyverse style guide
- Carpentries workshops
- Comunidade R brasileira

**DADOS:**
Sistema de Informações de Mortalidade (SIM) - DATASUS/Ministério da Saúde

---

**Última Atualização:** Novembro 2025  
**Versão:** 2.0 - Atualizado com 3 scripts CAMPUS VIRTUAL

*Desenvolvido com ❤️ para a comunidade de saúde pública brasileira*

