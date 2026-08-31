# leitura dos dados automatizados via google sheets ------------------------

## url gerada pelo comando "publicar na web (csv)" do google sheets
url_planilha <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vSZJrd7w2g2J4Qly05RRUbRRqtnp8FoTC-4mmGoGVbm1oBJcuTlV1DKzzCT4aLBPoYyLTFw7Ia-WDAO/pub?gid=924059358&single=true&output=csv"

## baixando os dados diretamente da nuvem
dbgolsraw <- read.csv(url_planilha, encoding = "UTF-8", stringsAsFactors = FALSE)


# tratamento e padronizacao -----------------------------------------------

dbgols <- dbgolsraw |>
  ## renomeando as colunas para o padrao que o dashboard espera, ignorando o carimbo do forms
  rlang::set_names(c("carimbo", "data", "partida", "gols", "competicao", "jogador", "pais_origem")) |>

  ## tratando as datas que vem da planilha
  dplyr::mutate(
    ## isola apenas a data caso o forms envie informacao de hora
    data = as.Date(data, tryFormats = c("%Y-%m-%d", "%d/%m/%Y")),
    gols = as.numeric(gols)
  ) |>

  ## filtrando ruidos ou linhas vazias
  dplyr::filter(!is.na(data) & !is.na(jogador)) |>

  ## limpando eventuais espacos indesejados nas strings
  dplyr::mutate(
    dplyr::across(dplyr::where(is.character), stringr::str_squish)
  ) |>

  ## ordenando do evento mais antigo para o mais recente para calcular os acumulados corretos
  dplyr::arrange(data)

## construindo a tabela para a aba de listagem no padrao antigo do script 02
tabela_final_limpa <- dbgols |>
  dplyr::select(jogador, pais_origem) |>
  dplyr::distinct()

## mensagem de validacao para os logs do terminal / github actions
message("Sucesso! Base carregada via Google Sheets.")
message("Total de registros: ", nrow(dbgols))
