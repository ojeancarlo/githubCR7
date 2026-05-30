# ⚽ O "GitHub" de Cristiano Ronaldo: Mapeando a constância do Robozão

[![R](https://img.shields.io/badge/R-276DC3?style=for-the-badge&logo=r&logoColor=white)](https://www.r-project.org/)
[![D3.js](https://img.shields.io/badge/d3.js-F9A03C?style=for-the-badge&logo=d3.js&logoColor=white)](https://d3js.org/)
[![JavaScript](https://img.shields.io/badge/javascript-F7DF1E?style=for-the-badge&logo=javascript&logoColor=black)](https://developer.mozilla.org/pt-BR/docs/Web/JavaScript)
[![HTML5](https://img.shields.io/badge/html5-E34F26?style=for-the-badge&logo=html5&logoColor=white)](https://developer.mozilla.org/pt-BR/docs/Web/HTML)
[![CSS3](https://img.shields.io/badge/css3-1572B6?style=for-the-badge&logo=css3&logoColor=white)](https://developer.mozilla.org/pt-BR/docs/Web/CSS)

Um projeto de **História com Dados** que visualiza a consistência e a letalidade de Cristiano Ronaldo ao longo de sua carreira. Inspirado no gráfico de contribuições do GitHub, o painel transforma cada dia da trajetória do atleta em um "commit" de gols.

A aplicação foi construída com um fluxo focado na análise de dados: todo o processo de *web scraping*, limpeza e os cálculos estatísticos pesados são resolvidos no **R**, que atua como o motor analítico do projeto. O script exporta tudo "mastigado" em um único arquivo visual. Os gráficos, desenhados com **D3.js**, rodam diretamente no navegador de quem acessa, garantindo uma experiência rápida e fluida sem precisar de bancos de dados ou servidores complexos rodando por trás.

🔗 **[Acesse o Painel Interativo Ao Vivo no GitHub Pages]**  
📖 **[Leia o artigo completo sobre os bastidores no Medium]**

---

## 🛠️ Funcionalidades e Itens Técnicos

* **Web Scraping e ETL:** Extração automatizada e iteração linha a linha do extenso histórico de jogos oficiais a partir do portal Sabino Statistics utilizando `rvest` e `purrr`, com manipulação e tipagem via `tidyverse` e `lubridate`.
* **Integração de dados:** O R condensa os *dataframes* já processados e calculados, convertendo-os para o formato JSON (`jsonlite`), e os injeta diretamente no arquivo visual final através do pacote `glue`.
* **Visualização customizada (D3.js):** Construção "do zero" do *Heatmap* de gols, com renderização condicional de cores (escala de intensidade), bordas especiais (jogos de Copa do Mundo), marcadores de recordes (gols centenários) e *tooltips* responsivas.
* **Storytelling de dados:** Criação de métricas comparativas com os 10 maiores artilheiros da história, incluindo:
    * Evolução cronológica de gols acumulados.
    * Identificação do auge artilheiro da carreira.
    * Gráfico de Radar comparando o desempenho por dia da semana (CR7 vs. Rivais).
    * Distribuição de gols por ciclo de vida (*Início, Auge e Maturidade*).
    * Barra de progresso customizada com a letalidade do jogador por competição.
* **UI/UX inimalista:** Interface responsiva desenhada nativamente com CSS, otimizada para Desktop e Mobile através de *media queries* e *flexbox*.

---

## 💻 Tecnologias utilizadas

* **Linguagens:** R, JavaScript, HTML, CSS
* **Manipulação de Dados e Datas (R):** `dplyr`, `tidyr`, `stringr`, `purrr`, `tibble`, `lubridate`
* **Web Scraping (R):** `rvest`, `httr`
* **Integração e Exportação (R):** `jsonlite`, `glue`
* **Visualização Dinâmica:** `D3.js`

---

## Para executar localmente

1. Clone este repositório em sua máquina:
```bash
git clone [https://github.com/ojeancarlo/github-cr7.git](https://github.com/ojeancarlo/github-cr7.git)
