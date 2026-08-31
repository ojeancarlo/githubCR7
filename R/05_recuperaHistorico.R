# SCRIPT DE RESGATE: MÁQUINA DO TEMPO (RODAR SÓ 1 VEZ) -------------------
library(dplyr)
library(rvest)
library(stringr)
library(purrr)
library(tidyr)

## 1. Raspando a página principal do Arquivo da Internet
url_archive <- "https://web.archive.org/web/2/https://docs.ufpr.br/~mmsabino/sstatistics/gol_oficial.html"
content <- rvest::read_html(url_archive)
table <- rvest::html_table(content)
classart <- table[[1]]

## Extraindo links e garantindo o nome do arquivo final
links_extraidos <- content |> rvest::html_elements(xpath = "//table//tr//td[4]//a") |> rvest::html_attr("href")
textos_dos_links <- content |> rvest::html_elements(xpath = "//table//tr//td[4]//a") |> rvest::html_text()
nomes_arquivos <- stringr::str_extract(links_extraidos, "[^/]+\\.html$")
base_url_archive <- "https://web.archive.org/web/2/https://docs.ufpr.br/~mmsabino/sstatistics/"
links_completos <- paste0(base_url_archive, nomes_arquivos)

df_links <- tidyr::tibble(jogador_info = textos_dos_links, link_scraping = links_completos) |>
  dplyr::mutate(jogador_info = stringr::str_squish(stringr::str_replace_all(jogador_info, "\r\n", " "))) |>
  dplyr::distinct(jogador_info, .keep_all = TRUE)

## Extraindo paises
linhas_com_bandeira <- content |> rvest::html_elements(xpath = "//table//tr[td[2]//img]")
paises_extraidos <- linhas_com_bandeira |> rvest::html_element(xpath = ".//td[2]//img") |> rvest::html_attr("alt")
textos_para_paises <- linhas_com_bandeira |> rvest::html_element(xpath = ".//td[4]") |> rvest::html_text()

df_paises <- tidyr::tibble(jogador_info = textos_para_paises, pais_origem = paises_extraidos) |>
  dplyr::mutate(jogador_info = stringr::str_squish(stringr::str_replace_all(jogador_info, "\r\n", " "))) |>
  dplyr::distinct(jogador_info, .keep_all = TRUE)

tabela_final_limpa <- classart |>
  dplyr::select(1, 4) |> rlang::set_names(c("posicao", "jogador_info")) |>
  dplyr::filter(!is.na(posicao) & posicao != "Pos.") |>
  dplyr::mutate(jogador_info = stringr::str_squish(stringr::str_replace_all(jogador_info, "\r\n", " "))) |>
  dplyr::left_join(df_links, by = "jogador_info") |>
  dplyr::left_join(df_paises, by = "jogador_info") |>
  dplyr::mutate(
    jogador = stringr::str_replace(jogador_info, pattern = " - .*", replacement = ""),
    pais_origem = stringr::str_to_title(pais_origem, locale = "pt"),
    gols = stringr::str_replace(stringr::str_replace(stringr::str_replace(jogador_info, pattern = ".* - ", replacement = ""), pattern = ".*\\( ", replacement = ""), pattern = " gols \\)", replacement = ""),
    posicao = as.integer(posicao),
    gols = as.numeric(gols)
  ) |>
  dplyr::select(posicao, jogador, pais_origem, gols, link_scraping)

## 2. Funcão de extração para o Web Archive (sem proxy, mais estavel)
extrair_dados_jogador_archive <- function(nome_jogador, link_atual) {
  Sys.sleep(1)
  message("Resgatando histórico: ", nome_jogador)
  tryCatch({
    pagina <- rvest::read_html(link_atual)
    extrair_coluna <- function(indice) {
      pagina |> rvest::html_element(xpath = sprintf("//table//td[%d]", indice)) |> rvest::html_nodes(xpath = ".//text()") |> rvest::html_text(trim = TRUE) |> purrr::discard(~ .x == "")
    }
    vetor_datas <- extrair_coluna(1)
    vetor_jogos <- extrair_coluna(2)
    vetor_gols <- extrair_coluna(3)
    vetor_comp <- extrair_coluna(4)

    vetor_datas <- vetor_datas[!stringr::str_detect(vetor_datas, "(?i)^data$")]
    vetor_jogos <- vetor_jogos[!stringr::str_detect(vetor_jogos, "(?i)^partida$")]
    vetor_gols <- vetor_gols[!stringr::str_detect(vetor_gols, "(?i)^gols?$")]
    vetor_comp <- vetor_comp[!stringr::str_detect(vetor_comp, "(?i)^competi")]

    tamanho_minimo <- min(length(vetor_datas), length(vetor_jogos), length(vetor_gols), length(vetor_comp))
    if(tamanho_minimo == 0) return(NULL)

    tibble::tibble(jogador = nome_jogador, data = vetor_datas[1:tamanho_minimo], partida = vetor_jogos[1:tamanho_minimo], gols = vetor_gols[1:tamanho_minimo], competicao = vetor_comp[1:tamanho_minimo])
  }, error = function(e) { return(NULL) })
}

## 3. Execução para os 11 jogadores
dbgolsraw_resgate <- purrr::map2_dfr(tabela_final_limpa$jogador, tabela_final_limpa$link_scraping, extrair_dados_jogador_archive)

## 4. Tratamento Final e Exportação
dbgols_resgate <- dbgolsraw_resgate |>
  dplyr::left_join(y = tabela_final_limpa |> dplyr::select(jogador, pais_origem), by = "jogador") |>
  dplyr::mutate(carimbo = "") |>
  dplyr::select(carimbo, data, partida, gols, competicao, jogador, pais_origem)

write.csv(dbgols_resgate, "base_completa_resgate.csv", row.names = FALSE, fileEncoding = "UTF-8")
message("\nPRONTO! O arquivo 'base_completa_resgate.csv' foi salvo na sua pasta do projeto com todos os jogadores.")
