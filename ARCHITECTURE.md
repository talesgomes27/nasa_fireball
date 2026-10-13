# Arquitetura e Especificação Técnica: NASA Fireballs & Bolides Command Center

## 1. Resumo do Entendimento (Understanding Summary)
- **Objetivo**: Modernização completa do painel em R Shiny de monitoramento de bólidos e bolas de fogo (fireballs) da NASA/JPL CNEOS.
- **Público-Alvo**: Cientistas de dados, entusiastas de astronomia, avaliadores e visitantes do portfólio.
- **Direção Visual**: *Deep Space Telemetry* (Dark-First com toggle nativo para Light Mode via `bslib::input_dark_mode()`, acentos em laranja plasma `#F97316` e ciano orbital `#38BDF8`, tipografia `Space Grotesk` + `Inter` + `JetBrains Mono`).
- **Idioma da Interface**: Português (PT-BR).

## 2. Premissas e Requisitos Não-Funcionais (Assumptions)
- **Eliminação de Dependências Pesadas (Design Enxuto)**:
  - Removidos: `shinydashboard`, `dashboardthemes`, `shinycustomloader`, `sf`, `rnaturalearth`, `rnaturalearthdata`, `ggthemes`, `parzer`, `janitor` e o metapacote `tidyverse`.
  - Stack enxuta mantida: `shiny`, `bslib` (Bootstrap 5), `plotly` (nativo), `reactable`, `jsonlite` e `dplyr` / R base.
- **Resiliência e Cache**:
  - Ingestão via `fetch_fireball_data()` com timeout de 5 segundos e fallback transparente para cache local em `data/fireball_cache.rds`.
  - Indicador de status da conexão no cabeçalho (*Live API NASA* vs. *Cache Local Offline*).
- **Tratamento de Dados Incompletos**:
  - Filtros e gráficos lidam graciosamente com valores nulos (`NA`) em coordenadas, altitude e velocidade sem quebrar a sessão do usuário.

## 3. Decision Log (Registro de Decisões)
| Decisão | Alternativas Consideradas | Motivo da Escolha |
| :--- | :--- | :--- |
| **Stack `bslib` (Bootstrap 5)** | Manter `shinydashboard` ou migrar para Python/JS | Moderniza o visual com suporte nativo a cards/temas, elimina 3 pacotes legados e mantém deploy no `shinyapps.io`. |
| **Arquitetura Modular em `R/`** | Monolito único em `app.R` ou Módulos `NS()` / Golem | Separa Dados, Gráficos e UI em arquivos curtos (<150 linhas) usando autoloading nativo do Shiny, sem boilerplate. |
| **Mapa via `plotly::plot_geo` (3D/2D)** | `ggplot2` + `sf` + `rnaturalearth` + `ggplotly` | Corta 4 dependências pesadas, acelera a renderização e adiciona o Globo 3D interativo com código mínimo. |
| **Remoção dos 4 arquivos `.html`** | Limpar o CSS inline nos HTMLs antigos | Textos estruturados diretamente em componentes nativos de UI (`R/ui_components.R`) respeitam Dark/Light mode e tipografia. |
| **Cache Local `.rds` de Fallback** | Requisição direta bloqueante na API | Garante abertura instantânea e funcionamento offline caso a API da NASA oscile. |

## 4. Estrutura do Projeto
```text
nasa_fireball/
├── app.R                     # Orquestrador da aplicação, UI navbar e servidor reativo
├── R/
│   ├── data_fireball.R       # Ingestão resiliente, sanitização em R base e fallback
│   ├── plots_fireball.R      # Visualizações nativas em Plotly (Globo 3D, Timeline, Atmosfera)
│   └── ui_components.R       # Cards de KPIs, tabela reactable e textos da missão
├── data/
│   └── fireball_cache.rds    # Snapshot local de fallback para resiliência offline
├── ARCHITECTURE.md           # Este documento de especificação
└── README.md                 # Visão geral do projeto e instruções de execução
```

## 5. Especificação Funcional dos Componentes
### Aba 1: Centro de Comando (Command Center)
- **Filtros Globais (Sidebar)**:
  - Intervalo de Datas (`dateRangeInput`).
  - Energia Mínima de Impacto (`selectInput`).
  - Projeção Espacial: Alternância entre **Globo 3D (Ortográfico)** e **Mapa 2D (Equirretangular)**.
  - Indicador numérico de eventos ativos.
- **4 Value Boxes Reativos**:
  1. *Total de Detecções*: Contagem reativa de bólidos no filtro.
  2. *Maior Energia de Impacto*: Valor máximo em kilotons (kt) + equivalência com bombas de Hiroshima (`15 kt`).
  3. *Velocidade Máxima*: Pico de velocidade pré-impacto em `km/s`.
  4. *Altitude Média*: Altitude média no ponto de brilho máximo em `km`.
- **Painel Central**:
  - Globo 3D / Mapa 2D interativo com escala logarítmica de energia e tooltips ricos.
- **Painel Analítico Lateral (`navset_card_underline`)**:
  - *Série Temporal*: Eventos por ano e energia acumulada/máxima.
  - *Perfil Atmosférico*: Gráfico de dispersão Altitude × Velocidade com demarcação das camadas atmosféricas (Troposfera, Estratosfera, Mesosfera).
  - *Distribuição & Correlação*: Gráfico de dispersão com referências de eventos históricos (Chelyabinsk 2013, Hiroshima).

### Aba 2: Explorador de Dados (Data Explorer)
- Tabela interativa com `reactable`:
  - Barra de visualização para energia de impacto.
  - Formatação refinada de datas, coordenadas e altitudes.
  - Paginação rápida, ordenação e busca integrada.
- Botão de download de dados filtrados em CSV.
- Dicionário de variáveis integrado em card colapsável.

### Aba 3: Briefing da Missão (Mission Brief)
- Contextualização física e astronômica da detecção de meteoros e bólidos.
- Descrição da equação empírica de Peter Brown et al. (2002) para cálculo de energia de impacto.
- Informações sobre sensores governamentais e satélites GOES/GLM.
- Créditos do projeto e links oficiais da NASA JPL CNEOS.
