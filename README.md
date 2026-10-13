# ☄️ NASA Fireballs & Bolides — Centro de Comando Analítico

Um painel analítico interativo e moderno em **R Shiny** para monitoramento, exploração científica e análise espacial de impactos de bólidos e bolas de fogo (*fireballs*) registrados na atmosfera terrestre pela rede de sensores governamentais da **NASA/JPL CNEOS**.

---

## 🚀 Principais Funcionalidades

- **Globo Terrestre 3D & Mapa 2D**: Visualização espacial interativa via `plotly::plot_geo` com alternância instantânea entre projeção ortográfica 3D e mapa plano 2D, com escala logarítmica de energia e tooltips detalhados.
- **Linha do Tempo Histórica**: Série temporal anual relacionando a frequência de detecções e os picos de energia de impacto desde 1988.
- **Perfil Atmosférico de Entrada**: Gráfico cruzando velocidade pré-impacto (km/s) e altitude de fragmentação (km), com demarcação das camadas da atmosfera (Troposfera, Estratosfera e Mesosfera).
- **Escala Comparativa de Energia (TNT)**: Equivalência direta com a bomba de Hiroshima (~15 kt) e referência ao superbólido de Chelyabinsk (2013, ~440 kt).
- **KPIs Reativos & Resumo Dinâmico**: Indicadores de impacto máximo, velocidade de pico, contagem filtrada e altitude média sincronizados em tempo real com os filtros de data e energia.
- **Tabela Chronológica Interativa**: Paginação, busca instantânea, ordenação e exportação em formato CSV.
- **Design System *Deep Space Telemetry***: Tema escuro aeroespacial com alternância nativa para Modo Claro via `bslib::input_dark_mode()`.
- **Resiliência e Funcionamento Offline**: Ingestão com timeout de requisição e fallback automático em cache local `.rds` caso a API da NASA oscile.

---

## 🏛️ Arquitetura do Software

O projeto segue os princípios de **Clean Architecture** e **Programação Minimalista (`ponytail`)**, desacoplando responsabilidades em módulos na pasta `R/`:

```text
nasa_fireball/
├── app.R                     # Orquestrador da aplicação, UI navbar e servidor reativo
├── R/
│   ├── data_fireball.R       # Ingestão resiliente, sanitização em R base e cache local
│   ├── plots_fireball.R      # Gráficos em Plotly nativo (Globo 3D, Timeline, Atmosfera)
│   └── ui_components.R       # Cards, tema bslib, tabela reactable e documentação técnica
├── data/
│   └── fireball_cache.rds    # Snapshot local para garantia de operação offline
├── ARCHITECTURE.md           # Especificação arquitetural e de design do sistema
└── README.md                 # Visão geral e guia de execução
```

### Otimizações e Redução de Dependências
- **Eliminados**: `shinydashboard`, `dashboardthemes`, `shinycustomloader`, `sf`, `rnaturalearth`, `rnaturalearthdata`, `ggthemes`, `parzer`, `janitor`, metapacote `tidyverse` e 4 arquivos HTML legados do Word.
- **Stack Enxuta**: `shiny`, `bslib` (Bootstrap 5), `plotly`, `reactable`, `jsonlite` e `dplyr` / R base.

---

## 💻 Como Executar Localmente

### Pré-requisitos
No R (versão ≥ 4.1):

```r
install.packages(c("shiny", "bslib", "plotly", "reactable", "jsonlite", "dplyr", "scales"))
```

### Executar a Aplicação
No terminal ou console do R dentro da pasta do projeto:

```r
shiny::runApp()
```

---

## 🛰️ Fonte dos Dados

Os dados brutos são fornecidos publicamente pela API da NASA:
- **API**: [NASA/JPL CNEOS Fireball API v1.2](https://ssd-api.jpl.nasa.gov/fireball.api)
- **Portal**: [Center for Near Earth Object Studies (CNEOS)](https://cneos.jpl.nasa.gov/fireballs/)
- **Cálculo de Energia**: Baseado na relação empírica de Peter Brown et al. (*Nature*, 2002):  
  $$E = 8.2508 \times E_0^{0.885}$$
