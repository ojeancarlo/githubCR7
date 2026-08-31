# setup inicial -----------------------------------------------------------

## definindo a pagina
page <- "https://docs.ufpr.br/~mmsabino/sstatistics/gol_oficial.html"

## resgatando a chave do ambiente (GitHub Secrets ou .Renviron local)
api_key <- Sys.getenv("SCRAPEOPS_KEY")

## montando a url do proxy com bypass de cloudflare
api_url <- paste0(
  "https://proxy.scrapeops.io/v1/?api_key=", api_key,
  "&url=", URLencode(page, reserved = TRUE),
  "&bypass=cloudflare"
)

## fazendo a requisicao
response <- httr::GET(api_url, httr::timeout(120))

## lendo a resposta como binário bruto
raw_content <- httr::content(response, as = "raw")

## convertendo os bytes de ISO-8859-1 para UTF-8 corretamente
utf8_content <- iconv(rawToChar(raw_content), from = "ISO-8859-1", to = "UTF-8")

## agora o read_html lê o texto já corrigido
content <- rvest::read_html(utf8_content)

## buscando a tabela base
table <- rvest::html_table(content)
classart <- table[[1]]


# tratamento de links -----------------------------------------------------

## extraindo os links e textos
links_extraidos <- content |>
  rvest::html_elements(xpath = "//table//tr//td[4]//a") |>
  rvest::html_attr("href")

textos_dos_links <- content |>
  rvest::html_elements(xpath = "//table//tr//td[4]//a") |>
  rvest::html_text()

## construindo o link
base_url <- "https://docs.ufpr.br/~mmsabino/sstatistics/"
links_completos <- paste0(base_url, links_extraidos)

## construindo o join de links
df_links <- tidyr::tibble(
  jogador_info = textos_dos_links,
  link_scraping = links_completos
) |>
  dplyr::mutate(
    jogador_info = stringr::str_replace_all(jogador_info, "\r\n", " "),
    jogador_info = stringr::str_squish(jogador_info)
  ) |>
  dplyr::distinct(jogador_info, .keep_all = TRUE)


# tratamento para coleta dos paises ---------------------------------------

## isolando as linhas que possuem a imagem da bandeira
linhas_com_bandeira <- content |>
  rvest::html_elements(xpath = "//table//tr[td[2]//img]")

## extraindo os paises e textos para cada linha
paises_extraidos <- linhas_com_bandeira |>
  rvest::html_element(xpath = ".//td[2]//img") |>
  rvest::html_attr("alt")

textos_para_paises <- linhas_com_bandeira |>
  rvest::html_element(xpath = ".//td[4]") |>
  rvest::html_text()

## construindo o join de paises
df_paises <- tidyr::tibble(
  jogador_info = textos_para_paises,
  pais_origem = paises_extraidos
) |>
  dplyr::mutate(
    jogador_info = stringr::str_replace_all(jogador_info, "\r\n", " "),
    jogador_info = stringr::str_squish(jogador_info)
  ) |>
  dplyr::distinct(jogador_info, .keep_all = TRUE)


# construindo a tabela final ----------------------------------------------

tabela_final_limpa <- classart |>

  ## selecionando as colunas pelo indice e renomeando para blindar contra alteracoes no html
  dplyr::select(1, 4) |>
  rlang::set_names(c("posicao", "jogador_info")) |>

  ## removendo os ruidos da tabela
  dplyr::filter(!is.na(posicao) & posicao != "Pos.") |>

  ## adequando as variaveis para facilitar o join
  dplyr::mutate(
    jogador_info = stringr::str_replace_all(jogador_info, "\r\n", " "),
    jogador_info = stringr::str_squish(jogador_info)
  ) |>

  ## executando os joins
  dplyr::left_join(df_links, by = "jogador_info") |>
  dplyr::left_join(df_paises, by = "jogador_info") |>

  ## construindo a tipagem e limpando strings
  dplyr::mutate(
    jogador = stringr::str_replace(jogador_info, pattern = " - .*", replacement = ""),
    pais_origem = stringr::str_to_title(pais_origem, locale = "pt"),

    ## construindo a variavel de gol
    gols = stringr::str_replace(jogador_info, pattern = ".* - ", replacement = ""),
    gols = stringr::str_replace(gols, pattern = ".*\\( ", replacement = ""),
    gols = stringr::str_replace(gols, pattern = " gols \\)", replacement = ""),

    ## retomando a tipagem das novas variaveis
    posicao = as.integer(posicao),
    gols = as.numeric(gols)
  ) |>

  ## organizando a visao final
  dplyr::select(posicao, jogador, pais_origem, gols, link_scraping)
