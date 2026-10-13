# NASA Fireballs — Monitoramento de Bólidos e Meteoros em R Shiny

[![R Version](https://img.shields.io/badge/R-%E2%89%A5_4.1-276DC3?logo=r&logoColor=white)](https://www.r-project.org/)
[![Shiny](https://img.shields.io/badge/Shiny-bslib_Bootstrap_5-38BDF8?logo=rstudio&logoColor=white)](https://shiny.posit.co/)
[![Live App](https://img.shields.io/badge/shinyapps.io-Online-10B981?logo=cloud&logoColor=white)](https://talesgomes27.shinyapps.io/nasa_fireball/)
[![Serverless Shinylive](https://img.shields.io/badge/GitHub_Pages-Shinylive_WebAssembly-F97316?logo=github&logoColor=white)](https://talesgomes27.github.io/nasa_fireball/)
[![Data Source](https://img.shields.io/badge/NASA_JPL-CNEOS_Fireball_API-EF4444?logo=nasa&logoColor=white)](https://cneos.jpl.nasa.gov/fireballs/)
[![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)](LICENSE)

Aplicação interativa desenvolvida em R com `bslib` (Bootstrap 5) e `Plotly` para exploração e análise física dos dados de impacto de bólidos e meteoros catalogados pela NASA/JPL CNEOS (*Center for Near Earth Object Studies*).

---

## Acesso Online

A aplicação está publicada em dois ambientes complementares:

| Ambiente | Plataforma | Execução | Link de Acesso |
| :--- | :--- | :--- | :--- |
| **shinyapps.io** | Servidor na nuvem | Container Linux tradicional | [talesgomes27.shinyapps.io/nasa_fireball](https://talesgomes27.shinyapps.io/nasa_fireball/) |
| **GitHub Pages** | Servidor estático | WebAssembly client-side (`shinylive` / webR) | [talesgomes27.github.io/nasa_fireball](https://talesgomes27.github.io/nasa_fireball/) |

A versão hospedada no GitHub Pages compila o código R em **WebAssembly**, executando diretamente no navegador do usuário via webR. Isso viabiliza hospedagem estática gratuita contínua, sem limites mensais de horas ativas de servidor.

---

## Motivação da Refatoração

Este projeto teve origem em uma aplicação acadêmica em R Shiny. A base de código anterior apresentava desafios práticos de manutenção e portabilidade:

1. **Monólito e dependências C++ pesadas**: Arquivo único com mais de 600 linhas utilizando pacotes de geoprocessamento C++ (`sf`, `gdal`, `rnaturalearth`), que inviabilizavam a compilação para WebAssembly e tornavam a inicialização lenta.
2. **Fragilidade de rede**: O carregamento dependia exclusivamente de chamadas síncronas à API da NASA. Oscilações no serviço externo impediam a inicialização da aplicação.
3. **Interface fragmentada**: Uso de temas obsoletos (`shinydashboard`), contraste inadequado de cores e arquivos HTML estáticos legados com formatação conflitante.

### Melhorias Implementadas

* **Modularização em `R/`**: Separação clara entre ingestão e tratamento (`R/data_fireball.R`), gráficos (`R/plots_fireball.R`) e componentes visuais (`R/ui_components.R`), mantendo o `app.R` como orquestrador conciso.
* **Redução de dependências externas**: Substituição dos pacotes de GIS pelo motor nativo de mapas do `plotly::plot_geo` (Globo 3D e projeção plana), reduzindo o tamanho do bundle e viabilizando o build WebAssembly no GitHub Actions.
* **Resiliência e cache offline**: Ingestão HTTP com timeout e fallback silencioso para snapshot serializado local (`data/fireball_cache.rds`).
* **Design com Bootstrap 5**: Migração para `bslib` com suporte nativo a modo claro e escuro, tipografia proporcional e contraste em conformidade com as diretrizes WCAG AA.

---

## Funcionalidades do Painel

* **Globo Terrestre 3D & Mapa 2D**: Visualização espacial dos impactos com rotação tridimensional, filtros temporais e escala de cores por energia de impacto.
* **Casos Históricos de Referência**: Seleção contextual rápida dos eventos mais significativos (Chelyabinsk 2013, Mar de Bering 2018, Viñales 2019 e o pico de velocidade de 2015).
* **Perfil Atmosférico**: Relação entre altitude de pico de brilho e velocidade de entrada, com demarcação das camadas atmosféricas (Troposfera, Estratosfera e Mesosfera).
* **Sazonalidade & Chuvas de Meteoros**: Distribuição mensal dos eventos correlacionada aos períodos de pico das chuvas de meteoros (Quadrantídeas, Perseidas e Geminídeas).
* **Correlação Logarítmica**: Gráfico de dispersão entre variáveis físicas (energia de impacto, energia radiada, altitude e velocidade) com linhas de referência históricas.
* **Tabela de Dados Interativa**: Catálogo completo construído com `reactable`, busca textual, ordenação por coluna e exportação para CSV.

---

## Física de Impacto & Cálculo de Energia

Os bólidos catalogados pelo CNEOS são detectados por sensores ópticos e infravermelhos a bordo de satélites governamentais dos EUA e detectores GLM dos satélites GOES.

### Relação Empírica de Brown et al. (Nature, 2002)

A energia óptica radiada $E_0$ (em $10^{10}\text{ Joules}$) é convertida na **energia total de impacto $E$** (em kilotons de TNT) pela equação empírica calibrada com dados de satélite e infrassom:

$$E = 8{,}2508 \times E_0^{0{,}885}$$

Onde:
* $E_0$: energia óptica integrada detectada pelos sensores ($10^{10}\text{ J}$).
* $E$: energia total de impacto estimada ($1\text{ kt TNT} = 4{,}184 \times 10^{12}\text{ Joules}$).

### Referência Nuclear

Para contextualizar a escala de grandeza física perante eventos históricos, o sistema calcula a razão em relação à arma detonada em Hiroshima (*Little Boy*, $\approx 15\text{ kt TNT}$):

$$\text{Razão} = \frac{E}{15\text{ kt}}$$

---

## Estrutura do Repositório

```text
nasa_fireball/
├── .github/
│   └── workflows/
│       └── deploy-shinylive.yml  # Pipeline CI/CD para compilação WebAssembly no GitHub Pages
├── app.R                         # Ponto de entrada, navegação Bootstrap 5 e servidor reativo
├── R/
│   ├── data_fireball.R           # Ingestão HTTP, cache local e tratamento de variáveis
│   ├── plots_fireball.R          # Funções de renderização gráfica em Plotly (3D, séries, perfil)
│   └── ui_components.R           # Componentes de interface, tema bslib, tabela e textos
├── data/
│   └── fireball_cache.rds        # Snapshot local serializado para operação offline
├── ARCHITECTURE.md               # Documentação técnica e registro de decisões arquiteturais
├── LICENSE                       # Licença MIT
└── README.md                     # Documentação geral do projeto
```

---

## Instalação e Execução Local

### Pré-requisitos
Requer **R (versão $\ge 4.1$)**.

```bash
git clone git@github.com:talesgomes27/nasa_fireball.git
cd nasa_fireball
```

### Instalar Pacotes

No console do R:
```r
install.packages(c(
  "shiny",
  "bslib",
  "plotly",
  "reactable",
  "jsonlite",
  "dplyr",
  "scales"
))
```

### Executar

```r
shiny::runApp()
```

---

## Fontes de Dados & Referências

* **Fonte de Dados**: [NASA/JPL CNEOS Fireball API](https://ssd-api.jpl.nasa.gov/fireball.api)
* **Center for Near Earth Object Studies**: [cneos.jpl.nasa.gov/fireballs/](https://cneos.jpl.nasa.gov/fireballs/)
* **Artigo de Referência**: Brown, P. et al. (2002). *The flux of small near-Earth objects colliding with the Earth*. Nature, 420, 294–296.
* **Shinylive para R**: [posit-dev/r-shinylive](https://github.com/posit-dev/r-shinylive)

---

## Autor

Desenvolvido por **Tales Gomes**.  
* GitHub: [@talesgomes27](https://github.com/talesgomes27)  
* Projeto de análise e engenharia de dados em R, com base inicial concebida no curso de Dashboards da [Curso-R](https://curso-r.com/).
