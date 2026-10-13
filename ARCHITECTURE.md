# Arquitetura do Software: NASA Fireballs (R Shiny)

## 1. Visão Geral do Sistema
Aplicação analítica interativa em R Shiny para monitoramento, análise física e visualização geoespacial dos dados de impactos de bólidos e meteoros catalogados pela NASA/JPL CNEOS (*Center for Near Earth Object Studies*).

### Stack Tecnológica
* **Linguagem**: R (versão $\ge 4.1$)
* **Framework Web**: `shiny` + `bslib` (Bootstrap 5)
* **Motor Gráfico**: `plotly` (nativo WebGL/SVG)
* **Tabelas Interativas**: `reactable`
* **Parsing & Requisições**: `jsonlite` + R base
* **Estratégia de Deploy**:
  - Cloud Linux Container (`shinyapps.io`)
  - Serverless WebAssembly client-side via Shinylive (`GitHub Pages`)

---

## 2. Requisitos Técnicos e Restrições de Plataforma

1. **Compatibilidade com WebAssembly (webR)**:
   - Bibliotecas R com dependências de compilação em C/C++ vinculadas ao sistema operacional (como `sf`, `gdal`, `geos`, `rnaturalearth`) não são suportadas no ambiente webR do Shinylive sem portas específicas.
   - O mapa de impactos utiliza `plotly::plot_geo` com projeção ortográfica 3D e equirretangular 2D, eliminando drivers espaciais pesados do bundle.

2. **Tolerância a Falhas de Conexão**:
   - A API pública da NASA (`ssd-api.jpl.nasa.gov`) pode apresentar oscilações ou latência.
   - A função `fetch_fireball_data()` define timeout e cabeçalho `HTTPUserAgent`. Caso a conexão falhe, o sistema carrega o snapshot serializado local (`data/fireball_cache.rds`) sem interromper o boot do aplicativo.

3. **Design System & Acessibilidade**:
   - Tema *Dark-First* construído via `bslib::bs_theme` com suporte a alternância dinâmica claro/escuro (`bslib::input_dark_mode`).
   - Contraste visual de textos e componentes conforme as diretrizes WCAG AA.

---

## 3. Registro de Decisões de Arquitetura (ADR)

| Decisão | Alternativas Avaliadas | Justificativa Técnica |
| :--- | :--- | :--- |
| **Adoção de `bslib` (Bootstrap 5)** | Manter `shinydashboard` | Suporte nativo a cards, temas dinâmicos e eliminação de 3 dependências obsoletas (`shinydashboard`, `dashboardthemes`, `shinycustomloader`). |
| **Modularização em `R/`** | Monólito em `app.R` ou framework `golem` | Separação de responsabilidades (dados, gráficos e UI) sem adicionar o overhead estrutural de empacotamento do golem. |
| **Mapa em `plotly::plot_geo`** | `leaflet`, `ggplot2` estático ou `mapview` | Oferece rotação tridimensional de globo (ortográfica) com aceleração gráfica no navegador e sem dependências GIS pesadas. |
| **Cache Local Serializado (`.rds`)** | Requisição síncrona sem contingência | Previne telas de erro na inicialização caso a API da NASA oscile. |
| **Deploy Dual (shinyapps.io + Pages)** | Hospedagem única no shinyapps.io | Garante disponibilidade ininterrupta via WebAssembly estático no GitHub Pages, contornando o limite de 25 horas mensais da conta gratuita do shinyapps.io. |

---

## 4. Estrutura de Diretórios e Componentes

```text
nasa_fireball/
├── app.R                     # Ponto de entrada: UI de navegação, layout reativo e servidor Shiny
├── R/
│   ├── data_fireball.R       # Ingestão HTTP, cache local, parse do JSON e filtros reativos
│   ├── plots_fireball.R      # Funções de renderização em Plotly (Globo 3D, timeline, atmosfera, sazonalidade)
│   └── ui_components.R       # Tema visual, cards de indicadores, tabela reactable e textos explicativos
├── data/
│   └── fireball_cache.rds    # Snapshot local de segurança para execução offline
├── .github/workflows/
│   └── deploy-shinylive.yml  # Pipeline de compilação WebAssembly para GitHub Pages
├── ARCHITECTURE.md           # Este documento de especificação
├── LICENSE                   # Licença MIT
└── README.md                 # Apresentação do projeto e guia de uso
```

---

## 5. Fluxo de Dados Reativo

1. **Boot**: `fetch_fireball_data()` consulta a API do CNEOS com User-Agent customizado. Se bem-sucedido, atualiza `fireball_cache.rds`; se falhar, lê o RDS local.
2. **Filtros Globais**: A expressão reativa `fireballs_r()` em `app.R` filtra os dados em memória por intervalo de datas e energia mínima de impacto.
3. **Casos de Referência**: A seleção no `selectInput("case_study")` atualiza programaticamente os inputs de data e energia via `shiny::updateDateRangeInput()` e `shiny::updateSelectInput()`.
4. **Renderização**: Os dados filtrados alimentam simultaneamente o mapa 3D, os quatro gráficos analíticos, os KPIs e a tabela interativa do `reactable`.
