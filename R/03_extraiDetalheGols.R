
# funcao de extracao ------------------------------------------------------

## construindo a funcão em modo linha a linha
extrair_dados_jogador <- function(nome_jogador, link_atual) {
  Sys.sleep(1)

  tryCatch({

    ## busca a chave configurada no GitHub Secrets
    api_key <- Sys.getenv("SCRAPINGBEE_KEY")

    ## URL do Proxy
    api_url <- paste0(
      "https://app.scrapingbee.com/api/v1/?api_key=", api_key,
      "&url=", URLencode(link_atual, reserved = TRUE),
      "&render_js=false"
    )

    ## requisição via Proxy
    requisicao <- httr::GET(
      api_url,
      httr::timeout(60)
    )

    # lendo a page
    pagina <- rvest::read_html(requisicao)

    ## funcão interna para extrair cada blocao e separar os textos automaticamente
    extrair_coluna <- function(indice) {
      pagina |>
        rvest::html_element(xpath = sprintf("//table//td[%d]", indice)) |>
        ## o .//text() para separar o que está quebrado por <br> ou \n
        rvest::html_nodes(xpath = ".//text()") |>
        rvest::html_text(trim = TRUE) |>
        ## removendo espacos em branco
        purrr::discard(~ .x == "")
    }

    vetor_datas <- extrair_coluna(1)
    vetor_jogos <- extrair_coluna(2)
    vetor_gols <- extrair_coluna(3)
    vetor_comp <- extrair_coluna(4)

    ## aplicando filtros para remover cabeçalhos soltos no meio da tabela
    vetor_datas <- vetor_datas[!stringr::str_detect(vetor_datas, "(?i)^data$")]
    vetor_jogos <- vetor_jogos[!stringr::str_detect(vetor_jogos, "(?i)^partida$")]
    vetor_gols <- vetor_gols[!stringr::str_detect(vetor_gols, "(?i)^gols?$")]
    vetor_comp <- vetor_comp[!stringr::str_detect(vetor_comp, "(?i)^competi")]

    ## encontrando o limite para agregar as tabelas sem erro de dimensão
    tamanho_minimo <- min(length(vetor_datas),
                          length(vetor_jogos),
                          length(vetor_gols),
                          length(vetor_comp))

    if(tamanho_minimo == 0) return(NULL)

    ## organizando a tibble fatiando todos para o mesmo tamanho
    tibble::tibble(
      jogador = nome_jogador,
      data = vetor_datas[1:tamanho_minimo],
      partida = vetor_jogos[1:tamanho_minimo],
      gols = vetor_gols[1:tamanho_minimo],
      competicao = vetor_comp[1:tamanho_minimo]
    )

  }, error = function(e) {
    ## informe de erro caso a busca não funcione
    message(paste("Erro no jogador:", nome_jogador, "-", e$message))
    return(NULL)
  })
}

# execucao em massa -------------------------------------------------------

## rodando a iteração para extrair todos os gols
dbgolsraw <- purrr::map2_dfr(
  tabela_final_limpa$jogador,
  tabela_final_limpa$link_scraping,
  extrair_dados_jogador
)

# tratamento final --------------------------------------------------------

dbgols <- dbgolsraw |>

  ## unindo com a tabela original para resgatar o país de origem
  dplyr::left_join(
    y = tabela_final_limpa |> dplyr::select(jogador, pais_origem),
    by = "jogador"
  ) |>

  ## padronizando as datas e limpeza
  dplyr::mutate(
    data = stringr::str_replace_all(data, pattern = " ", replacement = "/")
  ) |>

  ## garantindo datas com formato válido
  dplyr::filter(stringr::str_detect(data, "\\d{2}/\\d{2}/\\d{4}")) |>

  ## tipando as variáveis
  dplyr::mutate(
    data = lubridate::dmy(data),
    gols = as.integer(stringr::str_extract(gols, "\\d+"))
  ) |>

  ## removendo as entradas com datas erradas
  dplyr::filter(!is.na(data)) |>

  ## primeiro tratamento das competições com nomes iguais e com caracteres especiais
  dplyr::mutate(
    competicao = stringr::str_replace_all(competicao, "^(.+?)(?:\\s+\\1)+$", "\\1")
  ) |>

  ## aplicando algumas regras identificadas ao averiguar a base
  dplyr::mutate(
    gols_acumulado = cumsum(tidyr::replace_na(gols, 0)),
    data = stringr::str_replace(string = data, pattern = "1037", replacement = "1937"),
    ano = lubridate::year(data),

    ## padronizando os textos das competicoes temporariamente
    competicao_temp = stringr::str_to_title(competicao, locale = "pt"),

    ## reclassificando e padronizando as competicoes conforme o de-para de paises
    competicao = dplyr::case_when(

      ## portugal
      stringr::str_detect(competicao_temp, "(?i)Primeira Liga") ~ "Campeonato Português",
      stringr::str_detect(competicao_temp, "(?i)Taça de Portugal") ~ "Copa Portuguesa",

      ## inglaterra
      stringr::str_detect(competicao_temp, "(?i)Premier League") ~ "Campeonato Inglês",
      stringr::str_detect(competicao_temp, "(?i)FA Cup|League Cup") ~ "Copa Inglesa",

      ## espanha
      competicao_temp == "Liga" ~ "Campeonato Espanhol",
      stringr::str_detect(competicao_temp, "(?i)Copa do Rey") ~ "Copa da Espanha",
      stringr::str_detect(competicao_temp, "(?i)Supercopa - Espanha") ~ "Supercopa Espanhola",

      ## italia
      stringr::str_detect(competicao_temp, "(?i)Série A") ~ "Campeonato Italiano",
      stringr::str_detect(competicao_temp, "(?i)Coppa Itália") ~ "Copa Italiana",
      stringr::str_detect(competicao_temp, "(?i)Supercopa - Itália") ~ "Supercopa Italiana",

      ## arabia saudita
      stringr::str_detect(competicao_temp, "(?i)Liga - Arábia Saudita") ~ "Campeonato Árabe",
      stringr::str_detect(competicao_temp, "(?i)King Cup") ~ "Copa Árabe",
      stringr::str_detect(competicao_temp, "(?i)Supercopa - Arábia Saudita") ~ "Supercopa Árabe",

      ## competicoes continentais e mundiais de clubes
      stringr::str_detect(competicao_temp, "(?i)Liga dos Campeões|Liga dos Campeoões") ~ "Champions League",
      stringr::str_detect(competicao_temp, "(?i)Arab Club Championship") ~ "Liga dos Campeões Árabe",
      stringr::str_detect(competicao_temp, "(?i)Supercopa - Europa") ~ "Supercopa Europeia",
      stringr::str_detect(competicao_temp, "(?i)Mundial de Clubes") ~ "Mundial de Clubes",

      ## selecao (agrupando todos os anos e tipos de eliminatorias)
      stringr::str_detect(competicao_temp, "(?i)Eliminatória") ~ "Eliminatórias",

      ## o restante assume os nomes originais padronizados
      ## (Eurocopa, Copa do Mundo, Amistoso, Liga das Nacoes, Europa League, etc.)
      TRUE ~ competicao_temp
    )
  ) |>

  ## removendo a coluna auxiliar
  dplyr::select(-competicao_temp) |>
  dplyr::ungroup()
