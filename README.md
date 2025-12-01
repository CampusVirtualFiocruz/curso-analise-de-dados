# Introdução à Análise de Dados para Pesquisa no SUS

Este repositório contém os materiais práticos do curso **Introdução à Análise de Dados para Pesquisa no SUS**, oferecido pelo Campus Virtual Fiocruz.

---

## 📚 ESTRUTURA DO CURSO

O curso está organizado em 3 módulos:

| Módulo | Tema | Status |
|--------|------|--------|
| **Módulo 1** | Introdução à Lógica de Programação | ✅ Disponível |
| **Módulo 2** | Estatística Descritiva e Comunicação de Resultados | 🔜 Em breve |
| **Módulo 3** | Modelos estatísticos | 🔜 Em breve |

> **Nota:** Os materiais práticos dos Módulos 2 e 3 serão disponibilizados em breve neste repositório.

---

## 📁 CONTEÚDO DISPONÍVEL

### MÓDULO 1: Lógica e Linguagem de Programação

**Aula 1:** Introdução à Lógica de Programação

**Aula 2:** Introdução à Linguagem de Programação

| Tipo | Arquivo | Descrição |
|------|---------|-----------|
| Script | `modulo1aula2_script_1.R` | Operações básicas em R |
| Script | `modulo1aula2_script_2.R` | Manipulação de dados com tidyverse |
| Script | `modulo1aula2_atividades.R` | Gabarito das atividades práticas |
| PDF | `modulo1aula2_atividades.pdf` | Descrição das atividades propostas |
| PDF | `modulo1aula2_gabarito_atividade.pdf` | Gabarito comentado |

> **OBSERVAÇÃO:** Para responder as perguntas que estão em `modulo1aula2_atividades.pdf`, tente criar o seu código para encontrar as respostas, mas caso tenha dificuldades o gabarito em R (`modulo1aula2_atividades.R`) encontra-se na pasta.

---

## 📊 DADOS UTILIZADOS

A pasta `dados/` contém os arquivos utilizados nas atividades:

| Arquivo | Formato | Descrição |
|---------|---------|-----------|
| `sim_salvador_2023.csv` | CSV | Dados do Sistema de Informações sobre Mortalidade |
| `sim_salvador_2023.parquet` | Parquet | Mesmo dataset em formato otimizado |
| `sim_salvador_2023.xlsx` | Excel | Mesmo dataset em formato Excel |
| `sim_salvador_2023_processado.csv` | CSV | Dataset processado pelo Script 2 |
| `dicionario_sim.pdf` | PDF | Dicionário de variáveis do SIM |

### Estrutura do Dataset SIM

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

## 🎯 OBJETIVOS DE APRENDIZAGEM

### Módulo 1 - Introdução à Linguagem R

- Operações básicas no R
- Criação de variáveis categóricas com `mutate()` e `case_when()`
- Contagem e agregação de dados com `count()` e `group_by()`
- Manipulação de datas com `lubridate`
- Importação e exportação de dados

---

## 🚀 COMO UTILIZAR

1. Certifique-se de ter o **R** e o **RStudio** instalados
2. Instale os pacotes necessários (veja seção abaixo)
3. Defina o diretório de trabalho para esta pasta
4. Execute os scripts na ordem sugerida
5. Consulte o gabarito após tentar resolver as atividades

```r
# Definir diretório de trabalho
setwd("caminho/para/repositório")

# Instalar pacotes necessários
install.packages("tidyverse")
install.packages("lubridate")
install.packages("readxl")
install.packages("arrow")
```

> **OBSERVAÇÃO:** Lembre-se de ajustar o caminho do diretório de trabalho (`setwd()`) nos scripts para corresponder à localização dos arquivos no seu computador.

---

## 📝 ESTRUTURA DAS ATIVIDADES

As atividades práticas do Módulo 1 incluem exercícios de:

- Criação de variáveis derivadas usando `mutate()` e `case_when()`
- Contagem e sumarização de dados com `count()` e `group_by()`
- Análise exploratória de dados de mortalidade
- Transformação e limpeza de dados

> **DICA:** Tente criar seu próprio código antes de consultar o gabarito!

---

## 🔗 MATERIAL DE APOIO

### Documentação Oficial

- [R Project](https://www.r-project.org/)
- [RStudio/Posit](https://posit.co/)
- [Tidyverse](https://www.tidyverse.org/)
- [ggplot2](https://ggplot2.tidyverse.org/)
- [dplyr](https://dplyr.tidyverse.org/)

### Livros Gratuitos

- [R for Data Science](https://r4ds.had.co.nz/) (Hadley Wickham)
- [ggplot2: Elegant Graphics for Data Analysis](https://ggplot2-book.org/)
- [Fundamentals of Data Visualization](https://clauswilke.com/dataviz/) (Claus Wilke)

### Tutoriais Interativos

- [RStudio Primers](https://posit.cloud/learn/primers)
- [Swirl](https://swirlstats.com/) - aprender R dentro do R
- [DataCamp](https://www.datacamp.com/) - cursos introdutórios gratuitos

### Galerias de Visualização

- [R Graph Gallery](https://r-graph-gallery.com/)
- [From Data to Viz](https://www.data-to-viz.com/)

### Dados de Saúde Pública

- [DATASUS](https://datasus.saude.gov.br/)
- [TabNet](http://tabnet.datasus.gov.br/)
- [OpenDataSUS](https://opendatasus.saude.gov.br/)
- [Portal Brasileiro de Dados Abertos](https://dados.gov.br/)

### Cheat Sheets

- [RStudio IDE](https://posit.co/resources/cheatsheets/)
- [dplyr - Data Transformation](https://posit.co/resources/cheatsheets/)
- [ggplot2 - Data Visualization](https://posit.co/resources/cheatsheets/)
- [lubridate - Dates and Times](https://posit.co/resources/cheatsheets/)

---

## 🔧 SOLUÇÃO DE PROBLEMAS COMUNS

### Pacote não instala

- Verificar conexão com internet
- Usar `install.packages("nome", dependencies = TRUE)`
- Atualizar o R e RStudio
- Verificar permissões do sistema

### Erro ao importar dados

- Verificar caminho do arquivo com `getwd()`
- Usar `setwd()` para mudar diretório
- Verificar separador (vírgula vs ponto-e-vírgula)
- Verificar encoding do arquivo

### Gráfico não aparece

- Usar `print()` para objetos ggplot
- Verificar se RStudio está atualizado
- Limpar painel de gráficos
- Salvar e reabrir o script

### Erro "object not found"

- Verificar nome do objeto (case-sensitive)
- Executar linhas anteriores que criam o objeto
- Verificar se pacote está carregado (`library()`)
- Reiniciar sessão R se necessário

---

## ✨ BOAS PRÁTICAS DE PROGRAMAÇÃO

### Organização de Código

- Comentar código explicando "por quê", não "o quê"
- Usar nomes descritivos de variáveis
- Dividir código em seções lógicas
- Usar pipe `%>%` para encadear operações

### Estilo de Código

- Seguir [guia de estilo tidyverse](https://style.tidyverse.org/)
- Usar `snake_case` para nomes
- Espaços ao redor de operadores
- Indentação consistente (2 espaços)

### Reprodutibilidade

- Salvar versão dos pacotes usados
- Documentar sessão R (`sessionInfo()`)
- Usar projetos do RStudio (`.Rproj`)
- Nunca modificar dados originais

---

## 📖 COMO CITAR

> Introdução à Análise de Dados para Pesquisa no SUS. (2025). Scripts de R. Rio de Janeiro: Campus Virtual Fiocruz.

---

## 🙏 AGRADECIMENTOS E CRÉDITOS

Este material foi desenvolvido para o curso **"Introdução à Análise de Dados para Pesquisa no SUS"** com o objetivo de capacitar profissionais de saúde pública em análise de dados usando R.

**Inspirações:**
- R for Data Science (Hadley Wickham & Garrett Grolemund)
- Tidyverse style guide
- Comunidade R brasileira

**Dados:** Sistema de Informações de Mortalidade (SIM) - DATASUS/Ministério da Saúde

---

**Última Atualização:** Dezembro 2025  
**Versão:** 1.0 - Atualizado com 3 scripts CAMPUS VIRTUAL

---

*Desenvolvido com ❤️ para a comunidade de saúde pública brasileira*

