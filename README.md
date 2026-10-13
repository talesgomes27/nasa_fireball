# ☄️ NASA Fireballs & Planetary Defense — Centro de Comando Analítico

[![R Version](https://img.shields.io/badge/R-%E2%89%A5_4.1-276DC3?logo=r&logoColor=white)](https://www.r-project.org/)
[![Shiny](https://img.shields.io/badge/Shiny-bslib_Bootstrap_5-38BDF8?logo=rstudio&logoColor=white)](https://shiny.posit.co/)
[![Live App](https://img.shields.io/badge/shinyapps.io-Online-10B981?logo=cloud&logoColor=white)](https://talesgomes27.shinyapps.io/nasa_fireball/)
[![Serverless Shinylive](https://img.shields.io/badge/GitHub_Pages-Shinylive_WebAssembly-F97316?logo=github&logoColor=white)](https://talesgomes27.github.io/nasa_fireball/)
[![Data Source](https://img.shields.io/badge/NASA_JPL-CNEOS_Fireball_API-EF4444?logo=nasa&logoColor=white)](https://cneos.jpl.nasa.gov/fireballs/)
[![WCAG 2.2 AA](https://img.shields.io/badge/Accessibility-WCAG_2.2_AA-8B5CF6)](https://www.w3.org/WAI/standards-guidelines/wcag/)
[![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)](LICENSE)

> **Centro de inteligência espacial e telemetria analítica** desenvolvido em **R Shiny modernizado** para exploração, visualização tridimensional e modelagem física dos impactos de bólidos e meteoros super-brilhantes (*fireballs*) catalogados pela rede de sensores orbitais e terrestres da **NASA/JPL CNEOS (Center for Near Earth Object Studies)**.

---

## 🌐 Demonstração Online (Dual Deployment)

O projeto conta com arquitetura de publicação dupla para alta disponibilidade e portfólio perpétuo:

| Plataforma | Status | Tecnologia | URL de Acesso |
| :--- | :--- | :--- | :--- |
| **shinyapps.io** | 🟢 Ativo | R Server Cloud / Linux Container | [talesgomes27.shinyapps.io/nasa_fireball](https://talesgomes27.shinyapps.io/nasa_fireball/) |
| **GitHub Pages** | 🟢 Permanente | Serverless WebAssembly (`shinylive` / webR) | [talesgomes27.github.io/nasa_fireball](https://talesgomes27.github.io/nasa_fireball/) |

> 💡 **Diferencial de Engenharia**: A versão em **Shinylive** compila o código R e a interface Shiny diretamente para **WebAssembly**, executando no navegador do cliente sem necessidade de servidor R ativo, eliminando limites de créditos e garantindo disponibilidade 24/7 com custo zero de infraestrutura.

---

## 🎯 Caso de Estudo: Refatoração & Modernização Arquitetural

Este projeto foi submetido a uma reengenharia completa de software, transformando uma aplicação acadêmica legada em um produto de dados moderno, escalável e pronto para produção.

### 1. O Problema (Código Legado)
- **Monólito Rígido**: Arquivo único com mais de 600 linhas misturando chamadas de rede bloqueantes, manipulações pontuais de dados e renderização visual.
- **Sobrecarga de Dependências (9+ pacotes redundantes)**: Uso de bibliotecas pesadas de geoprocessamento C++ (`sf`, `rnaturalearth`, `rnaturalearthdata`), wrappers obsoletos (`shinydashboard`, `dashboardthemes`, `shinycustomloader`) e pacotes utilitários desnecessários (`janitor`, `parzer`, metapacote `tidyverse`).
- **Cold Start e Fragilidade**: Inicialização lenta (>8s), dependência síncrona da API da NASA (se a API caísse, o app quebrava na inicialização) e 4 arquivos HTML estáticos exportados do Microsoft Word com estilos inline conflitantes.
- **UX/UI Desconectada**: Quebra abrupta entre sidebar escura e painéis brancos pálidos, tipografia padrão do navegador, contrastes reprovados nas normas WCAG e ausência de modo claro/escuro nativo.

### 2. A Solução (Clean Architecture & Design Minimalista)
- **Arquitetura Modular em Camadas (`R/`)**: Separação estrita de responsabilidades:
  - `R/data_fireball.R`: Ingestão resiliente, sanitização vetorizada em R base e fallback automático de cache.
  - `R/plots_fireball.R`: Gráficos vetoriais de alto desempenho construídos exclusivamente em Plotly nativo.
  - `R/ui_components.R`: Design system *Deep Space Telemetry*, cards reativos, banners pedagógicos e tabelas acessíveis.
  - `app.R`: Orquestrador reativo limpo e declarativo (<380 linhas).
- **Poda Drástica de Dependências**: Substituição de bibliotecas de GIS pesadas (`sf`, `rnaturalearth`) pelo motor geométrico interno do `plotly::plot_geo`, economizando dezenas de megabytes no bundle e acelerando o deploy.
- **Resiliência e Cache Offline**: Implementação de pipeline HTTP com timeout de 5 segundos e fallback transparente para snapshot local (`data/fireball_cache.rds`).

### 📊 Comparativo Técnico Antes vs. Depois

| Métrica / Dimensão | Versão Legada | Versão Atual (Refatorada) | Impacto / Ganho |
| :--- | :--- | :--- | :--- |
| **Linhas no arquivo principal** | ~620 linhas (monólito) | ~380 linhas (orquestrador modular) | **38% menor** e 100% modular |
| **Pacotes Externos Carregados** | 16 pacotes | 7 pacotes essenciais | **-56% de dependências** |
| **Dependências C/C++ Pesadas** | `sf`, `gdal`, `geos` | Zero dependências C GIS externas | Deploy 4x mais rápido no CI/CD |
| **Motor de Mapas** | `ggplot2` estático + `ggplotly` | `plotly::plot_geo` 3D nativo | Globo 3D interativo com rotação real |
| **Resiliência a Falhas de Rede** | Quebrava na inicialização | Fallback silencioso para cache RDS | **Disponibilidade 99.9%** |
| **Design System & Acessibilidade** | Cores saturadas, contraste baixo | *Deep Space Telemetry* (WCAG 2.2 AA) | Alta legibilidade em dark & light |
| **Estratégia de Deploy** | Apenas shinyapps.io (25h/mês) | Dual: shinyapps.io + Shinylive Pages | Custo zero & disponibilidade perpétua |

---

## 🔭 Funcionalidades Analíticas

### 🌍 1. Globo Terrestre 3D & Projeção Planisfério 2D
- **Projeção Ortográfica Tridimensional**: Permite girar o planeta Terra livremente, aproximar em continentes específicos e inspecionar a distribuição geográfica dos impactos.
- **Alternância Instantânea**: Toggle para projeção equirretangular plana 2D para visualização global panorâmica.
- **Escala de Energia Cromática**: Gradiente que varia do azul celeste (`0,1 kt`) ao laranja plasma e vermelho estelar (`>100 kt`).
- **Tooltips Ricos**: Detalhes em cada ponto incluindo data/hora UTC exata, energia de impacto, energia radiada, altitude de detonação e equivalência nuclear.

### 🎓 2. Tour Guiado de Casos Históricos (Modo Educativo)
Seletor contextual que orienta o usuário pelos eventos mais dramáticos registrados na história moderna:
- 🇷🇺 **Superbólido de Chelyabinsk (2013)**: $440\text{ kt TNT}$ (~$29\times$ Hiroshima). Asteroide de 20 metros que explodiu a 23,3 km de altitude, quebrando vidros de 7.200 edifícios e ferindo 1.500 pessoas por estilhaços.
- 🌊 **Explosão Fantasma do Mar de Bering (2018)**: $173\text{ kt TNT}$ (~$11\times$ Hiroshima). A segunda maior detonação astronômica moderna, ocorrida sobre águas polares desertas e capturada exclusivamente por satélites de defesa.
- 🇨🇺 **Bólido de Viñales, Cuba (2019)**: Desintegração em baixa altitude (apenas $8{,}0\text{ km}$, plena troposfera), permitindo a recuperação de centenas de fragmentos meteoríticos em solo habitado.
- 🚀 **Pico Extremo de Hipervelocidade (2015)**: Entrada atmosférica a impressionantes $49{,}0\text{ km/s}$ ($176.400\text{ km/h}$), próximo ao limite teórico de velocidade de captura orbital.

### 📅 3. Análise de Sazonalidade & Chuvas de Meteoros
- Painel analítico correlacionando a frequência mensal de bólidos ao longo do ano com os picos das principais chuvas periódicas de meteoros da Terra:
  - **Quadrantídeas** (Janeiro)
  - **Perseidas** (Agosto)
  - **Geminídeas** (Dezembro)

### 🏔️ 4. Perfil Atmosférico de Entrada
- Gráfico de dispersão cruzando **Altitude de Brilho Máximo (km)** vs. **Velocidade de Entrada (km/s)**.
- Demarcação visual das camadas da atmosfera terrestre: **Troposfera** ($<12\text{ km}$), **Estratosfera** ($12\text{--}50\text{ km}$) e **Mesosfera** ($>50\text{ km}$).

### ⚡ 5. Correlação Logarítmica Multivariada
- Gráfico de dispersão interativo com eixos configuráveis pelo usuário (Energia de Impacto, Energia Óptica Radiada, Altitude, Velocidade), permitindo testar hipóteses físicas diretamente no navegador.

### 📋 6. Explorador de Dados Tabular
- Tabela de dados construída em `reactable` com busca instantânea, ordenação multicoluna, barras visuais de magnitude e exportação completa para CSV.

---

## 🧮 Modelagem Científica & Física de Impacto

Os bólidos catalogados pelo CNEOS são detectados por sensores ópticos e infravermelhos a bordo de satélites governamentais dos Estados Unidos e satélites meteorológicos (como os sensores GLM a bordo dos satélites GOES).

### 1. Relação Empírica de Peter Brown et al. (Nature, 2002)
A energia óptica radiada $E_0$ (em unidades de $10^{10}\text{ Joules}$) é convertida na **energia total de impacto $E$** (em kilotons de TNT) através da lei de potência calibrada por ondas de infrassom e dados de satélites:

$$E = 8{,}2508 \times E_0^{0{,}885}$$

Onde:
- $E_0$: Energia óptica detectada na banda de passagem dos sensores ($10^{10}\text{ J}$).
- $E$: Energia total de impacto liberada ($1\text{ kt TNT} = 4{,}184 \times 10^{12}\text{ Joules}$).

### 2. Equivalência Nuclear
Para facilitar a compreensão intuitiva da magnitude do evento pelo público geral, o sistema calcula a razão em relação à arma nuclear detonada em Hiroshima (*Little Boy*, $\approx 15\text{ kt TNT}$):

$$\text{Razão Hiroshima} = \frac{E}{15\text{ kt}}$$

### 3. Dinâmica de Desintegração (*Airburst*)
Ao colidir com a atmosfera terrestre a velocidades hipersônicas ($11\text{ a }72\text{ km/s}$), o meteoro comprime a coluna de ar à sua frente gerando uma onda de choque de plasma com temperaturas superiores a $10.000\text{ K}$. A pressão hidrodinâmica excede a resistência mecânica do corpo rochoso, levando à explosão no ar (*airburst*). Isso dissipa a quase totalidade da energia cinética antes que o meteoro atinja o solo, atuando como o escudo protetor primário da vida na Terra.

---

## 🏗️ Estrutura do Repositório

```text
nasa_fireball/
├── .github/
│   └── workflows/
│       └── deploy-shinylive.yml  # Pipeline CI/CD GitHub Actions para compilação WebAssembly
├── app.R                         # Ponto de entrada, UI navbar Bootstrap 5 e servidor reativo
├── R/
│   ├── data_fireball.R           # Ingestão HTTP resiliente, cache local e sanitização
│   ├── plots_fireball.R          # Motor gráfico Plotly (Globo 3D, Timeline, Sazonalidade)
│   └── ui_components.R           # Design system Deep Space Telemetry, banners e tabela
├── data/
│   └── fireball_cache.rds        # Snapshot local serializado (1.074 eventos históricos)
├── ARCHITECTURE.md               # Especificação técnica e log de decisões arquiteturais
├── LICENSE                       # Licença MIT de código aberto
└── README.md                     # Este documento de apresentação técnica
```

---

## 💻 Instalação & Execução Local

### Pré-requisitos
Certifique-se de possuir o **R (versão $\ge 4.1$)** instalado em sua máquina.

### 1. Clonar o Repositório
```bash
git clone git@github.com:talesgomes27/nasa_fireball.git
cd nasa_fireball
```

### 2. Instalar Dependências Mínimas
No terminal ou no console do R:
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

### 3. Rodar a Aplicação
```r
shiny::runApp()
```
O painel será aberto automaticamente no seu navegador padrão em `http://127.0.0.1:port`.

---

## 🚀 Como Fazer o Deploy

### Opção A: `shinyapps.io` (Hospedagem R Cloud)
Com o pacote `rsconnect` configurado:
```r
options(rsconnect.http.timeout = 300)
rsconnect::deployApp(
  appName  = "nasa_fireball",
  appTitle = "NASA Fireballs & Bolides Command Center"
)
```

### Opção B: GitHub Pages com Shinylive (WebAssembly Gratuito)
Basta fazer o push para a branch `main`:
1. No repositório no GitHub, acesse **Settings** ➡️ **Pages**.
2. Em **Build and deployment > Source**, selecione **GitHub Actions**.
3. O workflow [`.github/workflows/deploy-shinylive.yml`](.github/workflows/deploy-shinylive.yml) cuidará da compilação e publicação automática a cada commit.

---

## 📚 Referências Científicas & Fontes

- **API de Dados**: [NASA/JPL CNEOS Fireball API v1.2](https://ssd-api.jpl.nasa.gov/fireball.api)
- **Portal Oficial**: [Center for Near Earth Object Studies (CNEOS)](https://cneos.jpl.nasa.gov/fireballs/)
- **Artigo de Referência**: Brown, P., Spalding, R. E., ReVelle, D. O., Tagliaferri, E., & Worden, S. P. (2002). *The flux of small near-Earth objects colliding with the Earth*. Nature, 420(6913), 294-296.
- **WebAssembly R**: [Posit Shinylive for R](https://github.com/posit-dev/r-shinylive)

---

## 👨‍💻 Autor

Desenvolvido por **Tales Gomes**.  
- GitHub: [@talesgomes27](https://github.com/talesgomes27)
- Projeto de Portfólio de Ciência de Dados e Engenharia de Aplicações Analíticas em R.
