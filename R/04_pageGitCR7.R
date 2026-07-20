# preparando os dados -----------------------------------------------------

## filtrando os gols do Cristiano Ronaldo e limpando as datas e textos
base_cr7 <- dbgols |>
  dplyr::filter(jogador == "Cristiano Ronaldo", !is.na(data)) |>
  dplyr::mutate(
    data_limpa = as.Date(data),
    time_cr7   = stringr::str_trim(stringr::str_extract(partida, "^(.+?)(?=\\s+\\d+\\s*[xX])")),
    adversario = stringr::str_trim(stringr::str_extract(partida, "(?<=x\\s\\d{1,2}\\s)(.+)$")),
    time_lower = stringr::str_to_lower(time_cr7),

    ## agrupando e padronizando os nomes dos clubes
    clube = dplyr::case_when(
      stringr::str_detect(time_lower, "sporting") ~ "Sporting CP",
      stringr::str_detect(time_lower, "manchester") ~ "Manchester United",
      stringr::str_detect(time_lower, "real madrid") ~ "Real Madrid",
      stringr::str_detect(time_lower, "juventus") ~ "Juventus",
      stringr::str_detect(time_lower, "al.?nassr") ~ "Al Nassr",
      stringr::str_detect(time_lower, "portugal") ~ "Portugal",
      TRUE ~ time_cr7
    )
  )

## preparando os dados da tabela de detalhes (aba Lista de Gols)
tabela_detalhes <- base_cr7 |>
  dplyr::arrange(dplyr::desc(data_limpa)) |>
  dplyr::select(data_str = data_limpa, partida, clube, adversario, competicao, gols) |>
  dplyr::mutate(
    data_str = format(data_str, "%d/%m/%Y"),
    adversario = stringr::str_to_title(adversario)
  )

# construindo as estatísticas ---------------------------------------------

## agrupando a volumetria de gols por dia
dados_dia <- base_cr7 |>
  dplyr::group_by(data = data_limpa) |>
  dplyr::summarise(
    gols_dia    = sum(gols, na.rm = TRUE),
    adversarios = paste(unique(adversario), collapse = ", "),
    competicoes = paste(unique(competicao), collapse = ", "),
    clube       = dplyr::first(clube),
    .groups     = "drop"
  )

## calculando as métricas e agrupando os gols por clube
gols_clube <- base_cr7 |>
  dplyr::filter(clube != "Portugal") |>
  dplyr::group_by(clube) |>
  dplyr::summarise(gols = sum(gols, na.rm = TRUE), jogos = dplyr::n(), .groups = "drop") |>
  dplyr::mutate(media = round(gols / jogos, 2)) |>
  dplyr::arrange(dplyr::desc(gols))

## calculando as métricas e agrupando os gols pela seleção
gols_selecao <- base_cr7 |>
  dplyr::filter(clube == "Portugal") |>
  dplyr::group_by(clube) |>
  dplyr::summarise(gols = sum(gols, na.rm = TRUE), jogos = dplyr::n(), .groups = "drop") |>
  dplyr::mutate(media = round(gols / jogos, 2))


# construindo as estatísticas descritivas ---------------------------------

## encontrando as semanas com ocorrência de gols
semanas_com_gol <- base_cr7 |>
  dplyr::filter(gols > 0) |>
  dplyr::pull(data_limpa) |>
  lubridate::floor_date("week", week_start = 7) |>
  unique() |>
  sort()

## calculando a maior sequência de semanas seguidas com gols e o período
if(length(semanas_com_gol) > 0) {
  diffs_semanas <- as.numeric(diff(semanas_com_gol), units = "days") / 7
  blocos_consecutivos <- cumsum(c(1, diffs_semanas != 1))
  contagem_blocos <- table(blocos_consecutivos)

  max_semanas_seguidas <- max(contagem_blocos)

  ## identificando as datas exatas dessa maior sequência
  bloco_alvo <- as.numeric(names(contagem_blocos[contagem_blocos == max_semanas_seguidas]))[1]
  datas_seq <- semanas_com_gol[blocos_consecutivos == bloco_alvo]

  meses_pt <- c("jan", "fev", "mar", "abr", "mai", "jun", "jul", "ago", "set", "out", "nov", "dez")
  str_inicio <- paste0(meses_pt[lubridate::month(min(datas_seq))], "/", lubridate::year(min(datas_seq)))
  str_fim <- paste0(meses_pt[lubridate::month(max(datas_seq))], "/", lubridate::year(max(datas_seq)))

  periodo_max_seq <- paste0(str_inicio, " - ", str_fim)

} else {
  max_semanas_seguidas <- 0
  periodo_max_seq <- ""
}


# construindo o calendário com todas as datas -----------------------------

## extraindo as datas mínimas e máximas da carreira
ano_inicio <- min(lubridate::year(dados_dia$data))
ano_fim <- max(lubridate::year(dados_dia$data))

## extraindo a data de atualização via Proxy
url_atualizacao <- "https://docs.ufpr.br/~mmsabino/sstatistics/atualizacao.html"
api_key <- Sys.getenv("SCRAPINGBEE_KEY")

api_url_atualizacao <- paste0(
  "https://app.scrapingbee.com/api/v1/?api_key=", api_key,
  "&url=", URLencode(url_atualizacao, reserved = TRUE),
  "&render_js=false"
)

# Fazendo a requisição via Proxy e tratando o encoding
res_atualizacao <- httr::GET(api_url_atualizacao, httr::timeout(60))
raw_atualizacao <- httr::content(res_atualizacao, as = "raw")
utf8_atualizacao <- iconv(rawToChar(raw_atualizacao), from = "ISO-8859-1", to = "UTF-8")

data_atualizacao_raw <- rvest::read_html(utf8_atualizacao) |>
  rvest::html_text() |>
  stringr::str_extract("\\d{1,2}[./-]\\d{1,2}[./-]\\d{2,4}")

# Substitui os pontos por barras para manter o visual bonito no painel (01/06/2026)
data_atualizacao <- format(lubridate::dmy(data_atualizacao_raw), "%d/%m/%Y")

## construindo o dataframe do calendário completo
calendario <- tibble::tibble(
  data = seq(as.Date(paste0(ano_inicio, "-01-01")),
             as.Date(paste0(ano_fim,    "-12-31")),
             by = "day")
) |>
  dplyr::left_join(dados_dia, by = "data") |>
  dplyr::mutate(
    gols_dia = tidyr::replace_na(gols_dia, 0),
    adversarios = tidyr::replace_na(adversarios, ""),
    competicoes = tidyr::replace_na(competicoes, ""),
    clube = tidyr::replace_na(clube, ""),
    ano = lubridate::year(data),
    mes = lubridate::month(data),
    dia_semana = lubridate::wday(data, week_start = 7),
    semana_ano = as.integer(format(data, "%U")),
    data_str = format(data, "%Y-%m-%d")
  )

## tabelando os gols acumulados por semana ao longo da carreira
dados_acumulados <- dados_dia |>
  dplyr::arrange(data) |>
  dplyr::mutate(gols_acumulados = cumsum(gols_dia))

## criando a função para localizar as marcas históricas centenárias
acha_marco <- function(meta) {
  df <- dados_acumulados |> dplyr::filter(gols_acumulados >= meta) |> dplyr::slice(1)
  if(nrow(df) > 0) return(tibble::tibble(
    data_str = format(df$data, "%Y-%m-%d"),
    marco = paste0(meta, "º Gol"),
    tipo = "marca",
    texto_tooltip = paste0(meta, "º gol da carreira")
  ))
  return(NULL)
}

## aplicando a função para os gols centenários
marcos_gols_df <- dplyr::bind_rows(lapply(c(100, 300, 500, 700, 900), acha_marco))

## mapeando as datas de estreia ou retorno em cada momento da carreira
mudancas_clube_df <- base_cr7 |>
  dplyr::filter(clube != "Portugal") |>
  dplyr::arrange(data_limpa) |>
  dplyr::mutate(clube_anterior = dplyr::lag(clube)) |>
  dplyr::filter(clube != clube_anterior | is.na(clube_anterior)) |>
  dplyr::group_by(clube) |>
  dplyr::mutate(passagem = dplyr::row_number()) |>
  dplyr::ungroup() |>
  dplyr::mutate(
    data_str = format(data_limpa, "%Y-%m-%d"),
    marco = clube,
    tipo = dplyr::case_when(passagem > 1 ~ "retorno", TRUE ~ "clube"),
    texto_tooltip = dplyr::case_when(passagem > 1 ~ paste("1º gol no retorno ao", clube), TRUE ~ paste("1º gol pelo", clube))
  ) |>
  dplyr::select(data_str, marco, tipo, texto_tooltip)

## consolidando todos os marcos e estreias/reestreias
todos_marcos <- dplyr::bind_rows(marcos_gols_df, mudancas_clube_df)


# construindo as estatísticas de primeiro gol -----------------------------

## filtrando o primeiro gol marcado em cada torneio disputado e classificando
primeiro_gol_competicao <- base_cr7 |>
  dplyr::arrange(data_limpa) |>
  dplyr::group_by(competicao) |>
  dplyr::slice_min(order_by = data_limpa, n = 1, with_ties = FALSE) |>
  dplyr::ungroup() |>
  dplyr::mutate(
    data_str     = format(data_limpa, "%Y-%m-%d"),
    ano_primeiro = lubridate::year(data_limpa),
    categoria = dplyr::case_when(
      stringr::str_detect(competicao, "(?i)Champions|Europa League|Supercopa Europeia|Campeões Árabe") ~ "Torneios Continentais",
      stringr::str_detect(competicao, "(?i)Eliminatórias|Mundo|Confederações|Nações|Amistoso|Eurocopa") ~ "Seleção e Mundiais",
      stringr::str_detect(competicao, "(?i)Supercopa") ~ "Supercopas",
      stringr::str_detect(competicao, "(?i)Campeonato") ~ "Ligas Nacionais",
      stringr::str_detect(competicao, "(?i)Copa|Taça") ~ "Copas Nacionais",
      TRUE ~ "Outros"
    )
  ) |>
  dplyr::arrange(data_limpa) |>
  dplyr::select(competicao, data_str, ano_primeiro, partida, clube, categoria)


# construindo as estatísticas de vítimas e competicoes --------------------

## calculando os gols totais por competicao para visualizacao no dashboard
gols_por_comp <- base_cr7 |>
  dplyr::group_by(competicao) |>
  dplyr::summarise(total_gols = sum(gols, na.rm = TRUE), .groups = "drop") |>
  dplyr::arrange(dplyr::desc(total_gols))

## calculando as métricas de recorrência e hat-tricks
total_gols <- sum(dados_dia$gols_dia)
dias_gol   <- sum(dados_dia$gols_dia > 0)
datas_com_gol <- dados_dia |> dplyr::filter(gols_dia > 0) |> dplyr::pull(data) |> sort()
media_espera  <- round(mean(as.numeric(diff(datas_com_gol))), 1)
hat_tricks    <- sum(dados_dia$gols_dia >= 3)

## extraindo o ano com mais e menos gols
gols_por_ano <- base_cr7 |>
  dplyr::mutate(ano = lubridate::year(data_limpa)) |>
  dplyr::group_by(ano) |>
  dplyr::summarise(gols = sum(gols, na.rm = TRUE)) |>
  dplyr::arrange(gols)

ano_menos_gols <- gols_por_ano$ano[1]; gols_ano_menos <- gols_por_ano$gols[1]
ano_mais_gols  <- gols_por_ano$ano[nrow(gols_por_ano)]; gols_ano_mais  <- gols_por_ano$gols[nrow(gols_por_ano)]

## mapeando o top 3 de maiores vítimas em números absolutos
vitimas_abs_top3 <- base_cr7 |>
  dplyr::filter(clube != "Portugal") |>
  dplyr::group_by(adversario) |>
  dplyr::summarise(gols = sum(gols, na.rm = TRUE)) |>
  dplyr::arrange(dplyr::desc(gols)) |>
  dplyr::slice(1:3)

vitimas_abs_html <- paste0(
  "<div style='display:flex; flex-direction:column;'>",
  "<span style='font-size: 22px; font-weight: 900; font-family: \"Playfair Display\", serif; color: #000; line-height: 1.1;'>", vitimas_abs_top3$adversario, "</span>",
  "<span style='font-size: 13px; color: var(--text-muted);'>", vitimas_abs_top3$gols, " gols</span>",
  "</div>", collapse = ""
)

## mapeando o top 3 de maiores vítimas em média de gols
vitimas_rel_top3 <- base_cr7 |>
  dplyr::filter(clube != "Portugal") |>
  dplyr::group_by(adversario) |>
  dplyr::summarise(gols = sum(gols, na.rm = TRUE), jogos = dplyr::n()) |>
  dplyr::filter(jogos >= 5) |>
  dplyr::mutate(media = round(gols / jogos, 2)) |>
  dplyr::arrange(dplyr::desc(media)) |>
  dplyr::slice(1:3)

vitimas_rel_html <- paste0(
  "<div style='display:flex; flex-direction:column;'>",
  "<span style='font-size: 22px; font-weight: 900; font-family: \"Playfair Display\", serif; color: #000; line-height: 1.1;'>", vitimas_rel_top3$adversario, "</span>",
  "<span style='font-size: 13px; color: var(--text-muted);'>", vitimas_rel_top3$media, " gols/jogo</span>",
  "</div>", collapse = ""
)

## mapeando o top 3 de maiores vítimas pela seleção
vitimas_selecao_top3 <- base_cr7 |>
  dplyr::filter(clube == "Portugal") |>
  dplyr::group_by(adversario) |>
  dplyr::summarise(gols = sum(gols, na.rm = TRUE)) |>
  dplyr::arrange(dplyr::desc(gols)) |>
  dplyr::slice(1:3)

vitimas_selecao_html <- paste0(
  "<div style='display:flex; flex-direction:column;'>",
  "<span style='font-size: 22px; font-weight: 900; font-family: \"Playfair Display\", serif; color: #000; line-height: 1.1;'>", vitimas_selecao_top3$adversario, "</span>",
  "<span style='font-size: 13px; color: var(--text-muted);'>", vitimas_selecao_top3$gols, " gols</span>",
  "</div>", collapse = ""
)


# construindo o calendário perpétuo (Nova Aba) ----------------------------

## agrupando os gols marcados no mesmo dia e mes (ignorando o ano)
calendario_aniversario <- base_cr7 |>
  dplyr::mutate(
    mes = lubridate::month(data_limpa),
    dia = lubridate::day(data_limpa)
  ) |>
  dplyr::group_by(mes, dia) |>
  dplyr::summarise(
    total_gols = sum(gols, na.rm = TRUE),
    anos_marcados = paste(unique(lubridate::year(data_limpa)), collapse = ", "),
    .groups = "drop"
  )

## criando um grid universal de 365 dias para o mapa de calor
grid_calendario <- expand.grid(mes = 1:12, dia = 1:31) |>
  ## removendo os dias do ano que nao existem no calendario real
  dplyr::filter(!(mes == 2 & dia > 29) &
                  !(mes %in% c(4, 6, 9, 11) & dia == 31)) |>
  dplyr::left_join(calendario_aniversario, by = c("mes", "dia")) |>
  dplyr::mutate(
    marcou = ifelse(!is.na(total_gols) & total_gols > 0, 1, 0),
    total_gols = tidyr::replace_na(total_gols, 0),
    anos_marcados = tidyr::replace_na(anos_marcados, "")
  ) |>
  dplyr::arrange(mes, dia)


# construindo os dados históricos do top 10 -------------------------------

## identificando os 10 maiores artilheiros excluindo o cr7
top10_jogadores_total <- dbgols |>
  dplyr::group_by(jogador) |>
  dplyr::summarise(total_gols = sum(gols, na.rm = TRUE)) |>
  dplyr::arrange(desc(total_gols)) |>
  dplyr::filter(jogador != "Cristiano Ronaldo") |>
  dplyr::slice(1:10) |>
  dplyr::pull(jogador)

## consolidando a lista com os 11 jogadores
lista_11_total <- c("Cristiano Ronaldo", top10_jogadores_total)

## calculando os gols anuais e acumulados por jogador ao longo da carreira
dados_top10 <- dbgols |>
  dplyr::filter(jogador %in% lista_11_total & !is.na(data)) |>
  dplyr::mutate(ano = lubridate::year(data)) |>
  dplyr::group_by(jogador, ano) |>
  dplyr::summarise(gols_anuais = sum(gols, na.rm = TRUE), .groups = 'drop') |>
  dplyr::group_by(jogador) |>
  dplyr::arrange(ano) |>
  dplyr::mutate(temporada_carreira = dplyr::row_number(), gols_acumulados = cumsum(gols_anuais)) |>
  dplyr::ungroup()

## sumarizando as informações gerais de carreira de cada jogador
info_jogadores <- dbgols |>
  dplyr::filter(jogador %in% lista_11_total & !is.na(data)) |>
  dplyr::mutate(ano = lubridate::year(data)) |>
  dplyr::group_by(jogador) |>
  dplyr::summarise(ano_inicio = min(ano, na.rm = TRUE), ano_fim = max(ano, na.rm = TRUE), pais = dplyr::first(pais_origem), total_gols = sum(gols, na.rm = TRUE), .groups = 'drop') |>
  dplyr::arrange(desc(total_gols))

## mapeando as temporadas em que cada jogador atingiu múltiplos de 100 gols
pontos_marcas <- dados_top10 |>
  dplyr::group_by(jogador) |>
  dplyr::arrange(temporada_carreira) |>
  dplyr::mutate(grupo_100 = floor(gols_acumulados / 100)) |>
  dplyr::group_by(jogador, grupo_100) |>
  dplyr::slice(1) |>
  dplyr::ungroup() |>
  dplyr::filter(grupo_100 > 0 & gols_acumulados > 0)

## classificando os gols por fase da carreira (início, auge e maturidade)
dados_fases <- dados_top10 |>
  dplyr::mutate(
    fase = dplyr::case_when(
      temporada_carreira <= 5  ~ "Início (T1-T5)",
      temporada_carreira <= 12 ~ "Auge (T6-T12)",
      TRUE                     ~ "Maturidade (T13+)"
    ),
    fase = factor(fase, levels = c("Início (T1-T5)", "Auge (T6-T12)", "Maturidade (T13+)"))
  ) |>
  dplyr::group_by(jogador, fase) |>
  dplyr::summarise(gols_fase = sum(gols_anuais), .groups = "drop")

## calculando o total de gols anuais de todos os jogadores da base
base_temporadas_todos <- dbgols |>
  dplyr::filter(!is.na(data)) |>
  dplyr::mutate(ano = lubridate::year(data)) |>
  dplyr::group_by(jogador, ano) |>
  dplyr::summarise(gols_anuais = sum(gols, na.rm = TRUE), .groups = "drop") |>
  dplyr::group_by(jogador) |>
  dplyr::arrange(ano) |>
  dplyr::mutate(temporada_carreira = dplyr::row_number()) |>
  dplyr::ungroup()

## isolando a melhor temporada (auge) de cada jogador
auge_todos <- base_temporadas_todos |>
  dplyr::group_by(jogador) |>
  dplyr::filter(gols_anuais == max(gols_anuais)) |>
  dplyr::arrange(ano) |>
  dplyr::slice(1) |>
  dplyr::ungroup()

## separando o top 10 do cr7 para o gráfico de auge
top10_auge_resto <- auge_todos |>
  dplyr::filter(jogador != "Cristiano Ronaldo") |>
  dplyr::arrange(desc(gols_anuais)) |>
  dplyr::slice(1:10)

cr7_auge <- auge_todos |>
  dplyr::filter(jogador == "Cristiano Ronaldo")

auge_top10 <- dplyr::bind_rows(cr7_auge, top10_auge_resto) |>
  dplyr::arrange(desc(gols_anuais))

## contando a quantidade total de hat-tricks por jogador
ht_todos <- dbgols |>
  dplyr::filter(!is.na(data)) |>
  dplyr::group_by(jogador, data) |>
  dplyr::summarise(gols_partida = sum(gols, na.rm = TRUE), .groups = "drop") |>
  dplyr::filter(gols_partida >= 3) |>
  dplyr::group_by(jogador) |>
  dplyr::summarise(qtd_hat_tricks = dplyr::n(), .groups = "drop")

## separando o top 10 do cr7 para o gráfico de hat-tricks
top10_ht_resto <- ht_todos |>
  dplyr::filter(jogador != "Cristiano Ronaldo") |>
  dplyr::arrange(desc(qtd_hat_tricks)) |>
  dplyr::slice(1:10)

cr7_ht <- ht_todos |>
  dplyr::filter(jogador == "Cristiano Ronaldo")

if(nrow(cr7_ht) == 0) cr7_ht <-
  tibble::tibble(jogador = "Cristiano Ronaldo", qtd_hat_tricks = 0)

hat_tricks_top10 <- dplyr::bind_rows(cr7_ht, top10_ht_resto) |>
  dplyr::arrange(desc(qtd_hat_tricks))

## calculando a média de gols apenas nas partidas em que o jogador marcou
intensidade_todos <- dbgols |>
  dplyr::filter(!is.na(data)) |>
  dplyr::group_by(jogador) |>
  dplyr::summarise(total_gols = sum(gols, na.rm = TRUE), partidas_com_gol = dplyr::n_distinct(data), .groups = "drop") |>
  dplyr::filter(partidas_com_gol >= 100 | jogador == "Cristiano Ronaldo") |>
  dplyr::mutate(media_gols_partida = round(total_gols / partidas_com_gol, 2))

## separando o top 10 do cr7 para o gráfico de intensidade
top10_int_resto <- intensidade_todos |>
  dplyr::filter(jogador != "Cristiano Ronaldo") |>
  dplyr::arrange(desc(media_gols_partida)) |>
  dplyr::slice(1:10)

cr7_int <- intensidade_todos |>
  dplyr::filter(jogador == "Cristiano Ronaldo")

intensidade_top10 <- dplyr::bind_rows(cr7_int, top10_int_resto) |>
  dplyr::arrange(desc(media_gols_partida))

## classificando os gols convertidos por dia da semana
gols_dias_todos <- dbgols |>
  dplyr::filter(jogador %in% lista_11_total & !is.na(data)) |>
  dplyr::mutate(
    dia_nome = dplyr::case_when(
      lubridate::wday(data, week_start = 1) == 1 ~ "SEG",
      lubridate::wday(data, week_start = 1) == 2 ~ "TER",
      lubridate::wday(data, week_start = 1) == 3 ~ "QUA",
      lubridate::wday(data, week_start = 1) == 4 ~ "QUI",
      lubridate::wday(data, week_start = 1) == 5 ~ "SEX",
      lubridate::wday(data, week_start = 1) == 6 ~ "SÁB",
      TRUE ~ "DOM"
    )
  ) |>
  dplyr::group_by(jogador, dia_nome) |>
  dplyr::summarise(total_gols = sum(gols, na.rm = TRUE), partidas_no_dia = dplyr::n_distinct(data), .groups = "drop") |>
  dplyr::mutate(media_dia = total_gols / partidas_no_dia)

## calculando a média de gols do cr7 por dia da semana
cr7_dias <- gols_dias_todos |>
  dplyr::filter(jogador == "Cristiano Ronaldo") |>
  dplyr::select(dia_nome, media_cr7 = media_dia)

## calculando a média combinada dos outros jogadores por dia da semana
outros_dias <- gols_dias_todos |>
  dplyr::filter(jogador != "Cristiano Ronaldo") |>
  dplyr::group_by(dia_nome) |>
  dplyr::summarise(media_outros = mean(media_dia, na.rm = TRUE), .groups = "drop")

## mesclando as médias para construir o gráfico de radar
dias_semana_comp <- tibble::tibble(dia = c("SEG", "TER", "QUA", "QUI", "SEX", "SÁB", "DOM")) |>
  dplyr::left_join(cr7_dias, by = c("dia" = "dia_nome")) |>
  dplyr::left_join(outros_dias, by = c("dia" = "dia_nome")) |>
  dplyr::mutate(media_cr7 = round(tidyr::replace_na(media_cr7, 0), 2), media_outros = round(tidyr::replace_na(media_outros, 0), 2))


# transformando as referências em json ------------------------------------

## convertendo os dataframes processados para o formato json
## lista com os dataframes
listas_df <- list(
  dados = calendario,
  gols_clube = gols_clube,
  gols_selecao = gols_selecao,
  marcos = todos_marcos,
  dados_top10 = dados_top10,
  info_jogadores = info_jogadores,
  pontos_marcas = pontos_marcas,
  auge_top10 = auge_top10,
  hat_tricks = hat_tricks_top10,
  fases = dados_fases,
  intensidade = intensidade_top10,
  dias_semana = dias_semana_comp,
  primeiro_gol_competicao = primeiro_gol_competicao,
  gols_por_comp = gols_por_comp,
  calendario_aniversario = grid_calendario,
  tabela_detalhes = tabela_detalhes
)

## converter tudo para json
jsons <- purrr::map(
  listas_df,
  ~ jsonlite::toJSON(.x, dataframe = "rows", auto_unbox = TRUE)
)

# construindo a entrada html ----------------------------------------------

## estruturando o código html, css e javascript do dashboard interativo
html_final <- glue::glue(.open = "<<", .close = ">>", r"---(
<!DOCTYPE html>
<html lang="pt-BR">
<head>
<meta charset="UTF-8"/>
<meta name="viewport" content="width=device-width, initial-scale=1.0"/>
<meta name="referrer" content="no-referrer" />
<title>O "GitHub" de CR7 | Jean Carlo da Silva</title>
<script src="https://cdn.jsdelivr.net/npm/d3@7/dist/d3.min.js"></script>
<link href="https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600;700&family=Playfair+Display:ital,wght@0,700;0,900;1,700&family=Montserrat:wght@400;500;600;700;800;900&display=swap" rel="stylesheet"/>
<style>
  *, *::before, *::after { box-sizing: border-box; margin: 0; padding: 0; }
  :root { --bg: #FDFCF8; --text-main: #111111; --text-muted: #666666; --lines: #E5E5E5; --accent: #B03020; --gold: #C59B27; }

  /* ajustando o item para não empurrar espaço em branco */
  body {
    font-family: "Inter", -apple-system, sans-serif;
    background: var(--bg);
    color: var(--text-main);
    line-height: 1.6;
    -webkit-font-smoothing: antialiased;
    width: 100%;
    overflow-x: hidden;
    display: flex;
    flex-direction: column;
    min-height: 100vh;
  }

  /* footer no fim e tela inteira com padding perfeitamente alinhado */
  main {
    width: 100%;
    padding: 0 48px;
    margin: 0;
    flex: 1;
  }

  .article-container { padding: 60px 0 40px; }
  .header-wrapper { display: flex; align-items: center; justify-content: space-between; gap: 40px; margin-bottom: 32px; flex-wrap: wrap; }
  .header-text { flex: 1; min-width: 300px; }
  .header-photo { width: 160px; height: 160px; border-radius: 50%; object-fit: cover; border: 4px solid var(--bg); box-shadow: 0 0 0 1px var(--lines), 0 10px 25px rgba(0,0,0,0.08); }

  .kicker { font-size: 11px; font-weight: 700; text-transform: uppercase; letter-spacing: 2px; color: var(--accent); margin-bottom: 12px; display: block; }
  .title { font-family: "Playfair Display", Georgia, serif; font-size: clamp(32px, 5vw, 56px); font-weight: 900; line-height: 1.1; letter-spacing: -1px; margin-bottom: 16px; color: #000; }
  .dek { font-size: clamp(15px, 2vw, 18px); line-height: 1.6; color: var(--text-muted); max-width: 800px; }

  .byline { font-size: 13px; font-weight: 500; color: var(--text-main); border-top: 1px solid var(--lines); border-bottom: 1px solid var(--lines); padding: 16px 0; display: flex; justify-content: space-between; align-items: center; flex-wrap: wrap; gap: 12px; }
  .byline strong { font-weight: 700; color: #000; }
  .byline-links { margin-left: 6px; font-size: 12px; color: var(--text-muted); }
  .byline-links a { color: var(--text-muted); text-decoration: none; border-bottom: 1px solid transparent; transition: all 0.2s ease; padding-bottom: 1px; }
  .byline-links a:hover { color: var(--accent); border-bottom-color: var(--accent); }
  .byline .date { color: var(--text-muted); font-weight: 400; font-size: 12px; }

  .viz-container { width: 100%; margin: 0 auto; padding: 20px 0; }

  .tabs-menu { display: flex; gap: 32px; border-bottom: 1px solid var(--lines); margin-bottom: 32px; flex-wrap: wrap; }
  .tab-link { background: none; border: none; padding: 12px 0; font-family: "Inter", sans-serif; font-size: 14px; font-weight: 600; color: var(--text-muted); cursor: pointer; border-bottom: 2px solid transparent; transition: all 0.2s; }
  .tab-link:hover { color: var(--text-main); }
  .tab-link.active { color: var(--accent); border-bottom-color: var(--accent); }
  .tab-panel { display: none; }
  .tab-panel.active { display: block; animation: fadeIn 0.3s ease; }
  @keyframes fadeIn { from { opacity: 0; } to { opacity: 1; } }

  .dashboard { display: flex; flex-direction: column; gap: 40px; padding-bottom: 40px; margin-bottom: 32px; border-bottom: 1px solid var(--lines); }
  .dash-row { display: flex; flex-wrap: wrap; gap: 40px; align-items: flex-start; }

  .stats-numbers { display: flex; flex-wrap: wrap; gap: 40px; width: 100%; }
  .stat-group { display: flex; flex-direction: column; }
  .stat-group .label { font-size: 11px; text-transform: uppercase; letter-spacing: 1px; color: var(--text-muted); margin-bottom: 6px; font-weight: 600; }
  .stat-group .value { font-family: "Playfair Display", serif; font-size: 36px; font-weight: 900; line-height: 1; color: #000; }
  .stat-group .sub-value { font-size: 14px; color: var(--text-muted); font-weight: normal; font-family: "Inter", sans-serif;}
  .stat-group .desc { font-size: 11px; color: var(--text-muted); margin-top: 4px; line-height: 1.4; }

  .team-cards-container { display: flex; flex-wrap: wrap; gap: 24px; width: 100%; }

  /* ajustando o flex bord para adequar o team-card*/
  .team-card { background: transparent; border: 1px solid var(--lines); border-radius: 8px; padding: 24px 32px; flex: 1; min-width: 320px; box-shadow: none; display: flex; flex-direction: column; }

  .team-card-title { font-size: 12px; font-weight: 700; color: #111; text-transform: uppercase; letter-spacing: 1px; margin-bottom: 24px; display: flex; align-items: center; gap: 8px; }
  .title-clube::before { content: ""; display: inline-block; width: 12px; height: 12px; border-radius: 2px; background: var(--accent); }
  .title-selecao::before { content: ""; display: inline-block; width: 12px; height: 12px; border-radius: 2px; background: var(--gold); }

  .clubes-grid { display: flex; flex-wrap: wrap; row-gap: 20px; column-gap: 32px; }
  .clube-item { display: flex; align-items: center; gap: 12px; }
  .clube-logo { width: 36px; height: 36px; object-fit: contain; }
  .clube-logo-fallback { width: 36px; height: 36px; border-radius: 50%; background: #EAE8E3; display: flex; align-items: center; justify-content: center; font-size: 11px; font-weight: 700; color: #333; }
  .clube-info { display: flex; flex-direction: column; line-height: 1.2; }
  .clube-nome { font-size: 15px; font-weight: 700; color: var(--text-main); }
  .clube-gols { font-size: 13px; color: var(--text-muted); }
  .clube-media { font-size: 10px; color: var(--text-muted); opacity: 0.85; margin-top: -1px; }

  .legend-row { display: flex; align-items: center; gap: 16px; margin-bottom: 24px; flex-wrap: wrap; }
  .legend-label { font-size: 11px; font-weight: 600; color: var(--text-muted); text-transform: uppercase; letter-spacing: 1px; }
  .legend-items { display: flex; align-items: center; gap: 8px; }
  .legend-item { display: flex; align-items: center; gap: 6px; font-size: 11px; color: var(--text-muted); }
  .legend-box { width: 12px; height: 12px; border-radius: 2px; }

  /* containers fluídos */
  #heatmap-root, .nexo-box { width: 100%; overflow-x: auto; overflow-y: hidden; padding-bottom: 15px; -webkit-overflow-scrolling: touch; }
  #heatmap-root svg, .nexo-box svg { display: block; width: 100%; height: auto; min-width: 960px; }

  .small-chart-box { overflow: hidden; width: 100%; }
  .small-chart-box svg { display: block; width: 100%; height: auto; min-width: 0; }

  .nexo-title { font-family: "Montserrat", sans-serif; font-weight: 900; font-size: 24px; color: #0F172A; margin-top: 20px; margin-bottom: 6px; }
  .nexo-subtitle { font-family: "Montserrat", sans-serif; font-size: 14px; color: #475569; margin-bottom: 25px; line-height: 1.5; }

  .tooltip { position: absolute; pointer-events: none; opacity: 0; transition: opacity 0.15s ease; background: #ffffff; border: 1px solid var(--lines); box-shadow: 0 12px 32px rgba(0,0,0,0.08); padding: 16px; border-radius: 6px; min-width: 220px; z-index: 999; }
  .tt-header { display: flex; align-items: center; gap: 10px; margin-bottom: 8px; }
  .tt-clube-nome { font-size: 12px; font-weight: 600; color: var(--text-muted); }
  .tt-date { font-size: 11px; color: #888; }
  .tt-gols { font-family: "Playfair Display", serif; font-size: 24px; font-weight: 900; color: var(--accent); line-height: 1.1; margin-bottom: 6px; }
  .tt-info { font-size: 12px; color: var(--text-main); line-height: 1.5; }
  .tt-marco { font-size: 11px; font-weight: 700; color: #111; text-transform: uppercase; letter-spacing: 1px; margin-bottom: 6px; display: none;}
  .tt-copa { font-size: 11px; font-weight: 700; color: var(--gold); text-transform: uppercase; letter-spacing: 1px; margin-bottom: 6px; display: none;}

  /* cards de primeiro gol divididos por linha vertical */
  .pg-item {
    display: flex;
    flex-direction: column;
    align-items: center;
    text-align: center;
    background: transparent;
    padding: 0 10px;
    border-right: 1px solid var(--lines);
    width: 120px;
    flex: 0 0 auto;
  }
  .pg-item:last-child {
    border-right: none;
  }
  .pg-item-logo { width: 22px; height: 22px; object-fit: contain; margin-bottom: 4px; }
  .pg-item-comp { font-size: 8.5px; font-weight: 700; color: #111; text-transform: uppercase; margin-bottom: 2px; line-height: 1.1; width: 100%; word-wrap: break-word; }
  .pg-item-score { font-size: 9.5px; font-weight: 400; color: #333; margin-bottom: 2px; line-height: 1.2; width: 100%; word-wrap: break-word; }
  .pg-item-date { font-size: 8px; color: var(--text-muted); font-family: "Inter", sans-serif; }

  /* margens e tipografia do rodapé final */
  .footer { width: 100%; margin: 20px 0 20px; padding-top: 20px; border-top: 1px solid var(--lines); font-size: 12px; color: var(--text-muted); text-align: center; }
  .footer a { color: var(--text-muted); text-decoration: none; border-bottom: 1px solid var(--lines); }
  .footer a:hover { color: var(--accent); border-bottom-color: var(--accent); }

  /* Estilos para a Aba de Detalhes (Tabela e Exportação) */
  .table-container { width: 100%; max-height: 600px; overflow-y: auto; overflow-x: auto; margin-top: 24px; border: 1px solid var(--lines); border-radius: 8px; }
  .cr7-table { width: 100%; border-collapse: collapse; text-align: left; font-size: 13px; }
  .cr7-table th { background: #F8F7F2; position: sticky; top: 0; padding: 16px; font-weight: 700; color: #111; border-bottom: 2px solid var(--lines); white-space: nowrap; font-family: "Inter", sans-serif; z-index: 10; }
  .cr7-table td { padding: 14px 16px; border-bottom: 1px solid var(--lines); color: var(--text-muted); }
  .cr7-table tbody tr:hover { background-color: #F8F7F2; transition: background 0.2s; }

  .btn-export { display: inline-flex; align-items: center; gap: 8px; background: #111; color: #fff; padding: 12px 24px; border-radius: 6px; font-family: "Inter", sans-serif; font-size: 13px; font-weight: 600; text-decoration: none; border: none; cursor: pointer; transition: background 0.2s; }
  .btn-export:hover { background: var(--accent); }

  @media (max-width: 768px) { .header-wrapper { flex-direction: column-reverse; align-items: flex-start; } .header-photo { width: 100px; height: 100px; } main { padding: 0 20px; } .article-container { padding-top: 40px; } .dash-row { flex-direction: column; gap: 40px; } }
</style>
</head>
<body>

<main>
  <article class="article-container">
    <div class="header-wrapper">

      <div class="header-text">
        <span class="kicker">Visualização de Dados</span>

        <h1 class="title">
          O "GitHub" de Cristiano Ronaldo
        </h1>

        <p class="dek">
          No universo do desenvolvimento tecnológico, o gráfico de contribuições do GitHub é o símbolo definitivo de constância. Adaptamos essa lógica para os gramados: cada quadrado abaixo representa um dia da carreira de CR7. As cores revelam a intensidade de seus gols em mais de duas décadas, provando que sua frequência implacável é, na verdade, uma máquina perfeitamente codificada. Além disso, as Análises Gerais entregam o resumo descritivo da jornada do atleta.
        </p>
      </div>

      <div style="
        width: 220px;
        height: 220px;
        border-radius: 50%;
        overflow: hidden;
        border: 4px solid white;
        flex-shrink: 0;
      ">
        <img
          src="https://mir-s3-cdn-cf.behance.net/project_modules/hd/52a74048376295.589658726148d.gif"
          alt="Cristiano Ronaldo"
          class="header-photo"
          style="
            width: 100%;
            height: 100%;
            object-fit: cover;
            transform: scale(1.6);
            object-position: center top;
          "
        >
      </div>

    </div>

    <div class="byline">
      <span>
        Por <strong>Jean Carlo da Silva</strong>

        <span class="byline-links">
          &nbsp;&bull;&nbsp;
          <a href="https://www.linkedin.com/in/jeancarlonds/" target="_blank">
            LinkedIn
          </a>

          &nbsp;&bull;&nbsp;
          <a href="https://medium.com/@ojeancarlo" target="_blank">
            Medium
          </a>

          &nbsp;&bull;&nbsp;
          <a href="https://github.com/ojeancarlo/" target="_blank">
            GitHub
          </a>

          &nbsp;&bull;&nbsp;
          <a href="mailto:jeancnds@gmail.com">
            E-mail
          </a>
        </span>
      </span>

      <span class="date">
        Data de atualização da base: <<data_atualizacao>>
      </span>
    </div>

  </article>

  <div class="tabs-menu">
    <button class="tab-link active" onclick="switchTab(event, 'aba-cr7')">GitHub do CR7</button>
    <button class="tab-link" onclick="switchTab(event, 'aba-top10')">Análises Gerais</button>
    <button class="tab-link" onclick="switchTab(event, 'aba-calendario')">Calendário CR7</button>
    <button class="tab-link" onclick="switchTab(event, 'aba-lista')">Lista de Gols</button>
  </div>

  <div id="aba-cr7" class="tab-panel active">
    <section class="viz-container">
      <div class="dashboard">

        <div class="dash-row">
          <div class="stats-numbers">
            <div class="stat-group">
              <div class="label">Gols na carreira</div>
              <div class="value"><<total_gols>></div>
            </div>
            <div class="stat-group">
              <div class="label">Dias marcando</div>
              <div class="value"><<dias_gol>></div>
            </div>
            <div class="stat-group">
              <div class="label">Média de espera</div>
              <div class="value"><<media_espera>><span class="sub-value"> dias</span></div>
              <div class="desc">Espera média para fazer um gol</div>
            </div>
            <div class="stat-group">
              <div class="label">Maior Sequência</div>
              <div class="value"><<max_semanas_seguidas>><span class="sub-value"> sem.</span></div>
              <div class="desc">Semanas seguidas com gol<br><span style="font-size: 9.5px; opacity: 0.85;"><<periodo_max_seq>></span></div>
            </div>
            <div class="stat-group">
              <div class="label">Hat-tricks (3+ gols)</div>
              <div class="value"><<hat_tricks>></div>
            </div>
            <div class="stat-group">
              <div class="label">Ano com mais gols</div>
              <div class="value"><<ano_mais_gols>></div>
              <div class="desc"><<gols_ano_mais>> gols marcados</div>
            </div>
          </div>
        </div>

        <div class="dash-row">
          <div class="team-cards-container">
            <div class="team-card">
              <div class="team-card-title title-clube">Desempenho por Clubes</div>
              <div class="clubes-grid" id="clubes-grid" style="margin-bottom: 32px;"></div>
              <div style="display: flex; gap: 40px; flex-wrap: wrap; border-top: 1px solid var(--lines); padding-top: 24px; margin-top: auto;">
                <div class="stat-group">
                  <div class="label" style="margin-bottom: 12px;">Maiores Vítimas (Absoluto)</div>
                  <<vitimas_abs_html>>
                </div>
                <div class="stat-group">
                  <div class="label" style="margin-bottom: 12px;">Vítimas Letais (Média/Jogo)</div>
                  <<vitimas_rel_html>>
                </div>
              </div>
            </div>
            <div class="team-card">
              <div class="team-card-title title-selecao">Desempenho pela Seleção</div>
              <div class="clubes-grid" id="selecao-grid" style="margin-bottom: 32px;"></div>
              <div style="display: flex; gap: 40px; flex-wrap: wrap; border-top: 1px solid var(--lines); padding-top: 24px; margin-top: auto;">
                <div class="stat-group">
                  <div class="label" style="margin-bottom: 12px;">Maiores Vítimas (Seleção)</div>
                  <<vitimas_selecao_html>>
                </div>
              </div>
            </div>
          </div>
        </div>

        <div class="dash-row" style="margin-top: 20px;">
          <div class="team-card" style="width: 100%; padding: 24px 32px;">
            <div class="team-card-title title-clube" style="margin-bottom: 24px;">Primeiro Gol em cada Competição</div>
            <div id="primeiros-gols-grid"></div>
          </div>
        </div>

      </div>

      <div class="team-card-title title-clube" style="margin-top: 40px; margin-bottom: 20px;">Histórico de Contribuições (Mapeamento de Gols)</div>
      <div class="legend-row">
        <span class="legend-label">Gols no dia:</span>
        <div class="legend-items">
          <div class="legend-item"><div class="legend-box" style="background:#F2EFEB; border:1px solid #E5E5E5"></div>Nenhum</div>
          <div class="legend-item"><div class="legend-box" style="background:#B3C0D1"></div>1</div>
          <div class="legend-item"><div class="legend-box" style="background:#E07B6A"></div>2</div>
          <div class="legend-item"><div class="legend-box" style="background:#C95240"></div>3</div>
          <div class="legend-item"><div class="legend-box" style="background:#B03020"></div>4</div>
          <div class="legend-item"><div class="legend-box" style="background:#901A1E"></div>5+</div>
          <div class="legend-item" style="margin-left: 12px;"><div class="legend-box" style="background:transparent; border:1.5px solid #111;"></div>Marcas & Estreias</div>
          <div class="legend-item"><div class="legend-box" style="background:transparent; border:2px solid var(--gold);"></div>Copa do Mundo</div>
        </div>
      </div>

      <div id="heatmap-root"></div>
    </section>
  </div>

  <div id="aba-top10" class="tab-panel">

    <div class="nexo-title">A consistência do Top 10</div>
    <div class="nexo-subtitle">Uma visão cronológica da letalidade dos maiores artilheiros da história. O gráfico alinha as temporadas do ano 1 até a aposentadoria (ou momento atual), onde cores mais quentes revelam o volume de gols marcados em cada período.</div>
    <div class="nexo-box" id="nexo-heatmap"></div>

    <div class="nexo-title" style="margin-top: 50px;">A corrida pelos recordes</div>
    <div class="nexo-subtitle">O ritmo da história: este gráfico acompanha a velocidade com que cada lenda acumulou seus gols ao longo da carreira. A linha de Cristiano Ronaldo (em vermelho) destaca sua consistência e longevidade rumo ao topo.</div>
    <div class="nexo-box" id="nexo-lines"></div>

    <div class="dash-row" style="margin-top: 40px;">
      <div class="team-cards-container">
        <div class="team-card" style="flex: 1; min-width: 45%;">
          <div class="team-card-title title-clube">O Auge (Peak Season)</div>
          <div class="desc" style="font-size: 12px; color: var(--text-muted); margin-bottom: 16px; line-height: 1.5;">Análise da temporada mais letal de cada artilheiro. Comparamos o "pico" técnico e físico de Cristiano Ronaldo com o melhor ano individual de cada membro do seleto Top 10 histórico.</div>
          <div id="nexo-auge" class="small-chart-box"></div>
        </div>
        <div class="team-card" style="flex: 1; min-width: 45%;">
          <div class="team-card-title title-selecao">A Máquina de Hat-tricks</div>
          <div class="desc" style="font-size: 12px; color: var(--text-muted); margin-bottom: 16px; line-height: 1.5;">A capacidade de dominar e decidir um jogo sozinho. A métrica quantifica quantas vezes cada jogador conseguiu a difícil marca de anotar três ou mais gols em uma única partida oficial.</div>
          <div id="nexo-hattricks" class="small-chart-box"></div>
        </div>
      </div>
    </div>

    <div class="dash-row" style="margin-top: 24px;">
      <div class="team-cards-container">
        <div class="team-card" style="flex: 1; min-width: 45%;">
          <div class="team-card-title title-clube">Intensidade ao Marcar</div>
          <div class="desc" style="font-size: 12px; color: var(--text-muted); margin-bottom: 16px; line-height: 1.5;">Quando eles marcam, qual é o impacto? Calculamos a média de gols por jogo excluindo as partidas "em branco", focando na intensidade dos artilheiros apenas quando balançam as redes.</div>
          <div id="nexo-eficiencia" class="small-chart-box"></div>
        </div>
        <div class="team-card" style="flex: 1; min-width: 45%;">
          <div class="team-card-title title-selecao">O Dia do Matador (Média por Jogo)</div>
          <div class="desc" style="font-size: 12px; color: var(--text-muted); margin-bottom: 16px; line-height: 1.5;">Existe um dia da semana em que eles são mais perigosos? O radar sobrepõe o desempenho de CR7 (área vermelha) à média combinada de todos os seus rivais do Top 10 (área cinza).</div>
          <div id="nexo-radar" class="small-chart-box"></div>
        </div>
      </div>
    </div>

    <div class="dash-row" style="margin-top: 24px; margin-bottom: 0;">
      <div class="team-cards-container">
         <div class="team-card" style="width: 100%;">
            <div class="team-card-title title-clube">Em qual fase cada artilheiro brilhou mais?</div>
            <div class="desc" style="font-size: 12px; color: var(--text-muted); margin-bottom: 16px; line-height: 1.5;">O ciclo de vida dos gols. Dividimos as carreiras em três ciclos para entender a distribuição: o Início (T1-T5), durante o Auge (T6-T12) e na fase de Maturidade (T13 em diante).</div>
            <div id="nexo-fases" class="small-chart-box"></div>
         </div>
      </div>
    </div>

    <div class="dash-row" style="margin-top: 24px; margin-bottom: 0;">
      <div class="team-cards-container">
         <div class="team-card" style="width: 100%;">
            <div class="team-card-title title-clube">A Letalidade de Cristiano Ronaldo por Competição</div>
            <div class="desc" style="font-size: 12px; color: var(--text-muted); margin-bottom: 16px; line-height: 1.5;">Uma visão consolidada de onde o artilheiro deixou sua marca. O gráfico quantifica os gols distribuídos por todas as competições oficiais disputadas ao longo de sua trajetória.</div>
            <div id="cr7-gols-comp" class="small-chart-box"></div>
         </div>
      </div>
    </div>

  </div>

  <div id="aba-calendario" class="tab-panel">
    <div class="nexo-title">O Calendário Perpétuo</div>
    <div class="nexo-subtitle">Existe algum dia no ano em que CR7 nunca marcou gol? Mapeamos todos os dias da sua carreira, independentemente do ano. Quadrados vermelhos indicam datas em que ele já balançou as redes pelo menos uma vez em toda a sua história.</div>

    <div class="legend-row" style="margin-top: 24px;">
      <div class="legend-items">
        <div class="legend-item"><div class="legend-box" style="background:#F2EFEB; border:1px solid #E5E5E5"></div>Ainda não marcou</div>
        <div class="legend-item"><div class="legend-box" style="background:var(--accent)"></div>Já marcou gol</div>
      </div>
    </div>

    <div class="nexo-box" id="nexo-calendario"></div>
  </div>

  <div id="aba-lista" class="tab-panel">
    <div class="header-wrapper" style="margin-bottom: 24px; align-items: flex-end;">
      <div>
        <div class="nexo-title">Raio-X Completo de Gols</div>
        <div class="nexo-subtitle" style="margin-bottom: 0;">O registro tabular de todas as vezes em que a rede balançou.<br>Você pode baixar a base de dados completa clicando no botão ao lado.</div>
      </div>
      <button class="btn-export" onclick="exportarCSV('gols_cr7.csv')">
        <svg width="18" height="18" fill="none" stroke="currentColor" viewBox="0 0 24 24" xmlns="http://www.w3.org/2000/svg"><path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M4 16v1a3 3 0 003 3h10a3 3 0 003-3v-1m-4-4l-4 4m0 0l-4-4m4 4V4"></path></svg>
        Baixar Dados (CSV)
      </button>
    </div>

    <div class="table-container">
      <table class="cr7-table" id="detalhes-tabela">
        <thead>
          <tr>
            <th>Data</th>
            <th>Partida</th>
            <th>Clube/Seleção</th>
            <th>Adversário</th>
            <th>Competição</th>
            <th style="text-align: center;">Gols</th>
          </tr>
        </thead>
        <tbody>
          <!-- As linhas serão inseridas dinamicamente pelo Javascript -->
        </tbody>
      </table>
    </div>
  </div>

  <footer class="footer">
    Desenvolvido por <a href="https://github.com/ojeancarlo/" target="_blank">Jean Carlo da Silva</a> usando R e D3.js.<br/>
    Fonte de dados: <a href="https://docs.ufpr.br/~mmsabino/sstatistics/" target="_blank"> Docs UFPr - Sabino Statistics</a>.
  </footer>
</main>

<div class="tooltip" id="tooltip">
  <div class="tt-marco" id="tt-marco"></div>
  <div class="tt-copa" id="tt-copa">&#127942; Jogo de Copa do Mundo</div>
  <div class="tt-header">
    <div id="tt-logo-wrap"></div>
    <div>
      <div class="tt-clube-nome" id="tt-clube"></div>
      <div class="tt-date" id="tt-date"></div>
    </div>
  </div>
  <div class="tt-gols" id="tt-gols"></div>
  <div class="tt-info" id="tt-adv"></div>
  <div class="tt-info" id="tt-comp"></div>
</div>

<div class="tooltip" id="tooltip-nexo" style="font-family: 'Inter', sans-serif; font-size: 12px; background: rgba(255,255,255,0.98); border: 1px solid var(--lines); box-shadow: 0 8px 24px rgba(0,0,0,0.08); padding: 12px; border-radius: 4px; position: absolute; pointer-events: none; opacity: 0; z-index: 999;"></div>

<script>
function switchTab(evt, tabId) {
  document.querySelectorAll('.tab-panel').forEach(function(t) { t.classList.remove('active'); });
  document.querySelectorAll('.tab-link').forEach(function(l) { l.classList.remove('active'); });
  document.getElementById(tabId).classList.add('active');
  evt.currentTarget.classList.add('active');
  }

// variáveis criadas com base nos maps para coletar os jsons
var DADOS = <<jsons$dados>>;
var GOLS_CLUBE = <<jsons$gols_clube>>;
var GOLS_SELECAO = <<jsons$gols_selecao>>;
var MARCOS = <<jsons$marcos>>;
var DADOS_TOP10 = <<jsons$dados_top10>>;
var INFO_JOG= <<jsons$info_jogadores>>;
var PONTOS_MARCAS = <<jsons$pontos_marcas>>;
var DADOS_AUGE = <<jsons$auge_top10>>;
var DADOS_HT = <<jsons$hat_tricks>>;
var DADOS_FASES = <<jsons$fases>>;
var DADOS_EFI = <<jsons$intensidade>>;
var DADOS_RADAR = <<jsons$dias_semana>>;
var PRIMEIROS_GOLS = <<jsons$primeiro_gol_competicao>>;
var GOLS_POR_COMP = <<jsons$gols_por_comp>>;
var DADOS_CALENDARIO = <<jsons$calendario_aniversario>>;
var TABELA_DETALHES = <<jsons$tabela_detalhes>>;

var marcosMap = {};
MARCOS.forEach(function(m) { marcosMap[m.data_str] = m; });

var ESCUDOS = {
  "Sporting CP": "https://upload.wikimedia.org/wikipedia/pt/3/3e/Sporting_Clube_de_Portugal.png",
  "Manchester United": "https://upload.wikimedia.org/wikipedia/pt/b/b6/Manchester_United_FC_logo.png",
  "Real Madrid": "https://upload.wikimedia.org/wikipedia/pt/9/98/Real_Madrid.png",
  "Juventus": "https://upload.wikimedia.org/wikipedia/commons/e/ed/Juventus_FC_-_logo_black_%28Italy%2C_2020%29.svg",
  "Al Nassr": "https://upload.wikimedia.org/wikipedia/pt/2/26/Al-Nassr_FC.png",
  "Portugal": "https://upload.wikimedia.org/wikipedia/pt/7/75/Portugal_FPF.png"
};
var SIGLAS = { "Sporting CP":"SCP","Manchester United":"MNU","Real Madrid":"RMA","Juventus":"JUV","Al Nassr":"ALN","Portugal":"POR" };

function buildTeamGrids(dataList, containerId) {
  var grid = document.getElementById(containerId);
  grid.innerHTML = "";

  dataList.forEach(function(c) {
    var chip = document.createElement("div");
    chip.className = "clube-item";

    var logoUrl = ESCUDOS[c.clube];
    var logoEl;

    if (logoUrl) {
      logoEl = document.createElement("img");
      logoEl.className = "clube-logo";
      logoEl.src = logoUrl;
      logoEl.onerror = function() {
        var fb = makeFallback(c.clube, "clube-logo-fallback");
        this.style.display = "none";
        this.parentNode.insertBefore(fb, this);
      };
    } else {
      logoEl = makeFallback(c.clube, "clube-logo-fallback");
    }

    var info = document.createElement("div");
    info.className = "clube-info";

    info.innerHTML = `
      <div class="clube-nome">${c.clube}</div>
      <div class="clube-stats">
        <span class="clube-gols">${c.gols} gols</span>
        <span class="clube-media">• ${c.media} p/ jogo</span>
      </div>
    `;

    chip.appendChild(logoEl);
    chip.appendChild(info);
    grid.appendChild(chip);
  });
}

function makeFallback(nome, cls) {
  var d = document.createElement("div");
  d.className = cls;
  d.textContent = (SIGLAS[nome] || nome.substring(0,3)).toUpperCase();
  return d;
}

buildTeamGrids(GOLS_CLUBE, "clubes-grid");
buildTeamGrids(GOLS_SELECAO, "selecao-grid");

// listagem de primeiros gols agrupada por categoria
(function() {
  var MESES = ["jan","fev","mar","abr","mai","jun","jul","ago","set","out","nov","dez"];
  var container  = document.getElementById("primeiros-gols-grid");
  if (!container) return;

  // agrupa os dados pela categoria das competições
  var groups = {
     "Ligas Nacionais": [],
     "Copas Nacionais": [],
     "Supercopas": [],
     "Torneios Continentais": [],
     "Seleção e Mundiais": [],
     "Outros": []
  };

  PRIMEIROS_GOLS.forEach(function(d) {
     if(groups[d.categoria]) {
        groups[d.categoria].push(d);
     } else {
        groups["Outros"].push(d);
     }
  });

  // configura o container principal
  container.style.display = "flex";
  container.style.flexDirection = "row";
  container.style.flexWrap = "wrap";
  container.style.gap = "32px 48px";
  container.style.overflowX = "hidden";

  // renderiza cada grupo
  for(var cat in groups) {
     if(groups[cat].length === 0) continue;

     var catBlock = document.createElement("div");
     catBlock.style.display = "flex";
     catBlock.style.flexDirection = "column";
     catBlock.style.flex = "0 1 auto";

     var catTitle = document.createElement("div");
     catTitle.style.fontSize = "11px";
     catTitle.style.fontWeight = "700";
     catTitle.style.color = "var(--text-muted)";
     catTitle.style.textTransform = "uppercase";
     catTitle.style.letterSpacing = "1px";
     catTitle.style.marginBottom = "12px";
     catTitle.style.borderBottom = "1px solid var(--lines)";
     catTitle.style.paddingBottom = "6px";
     catTitle.textContent = cat;
     catBlock.appendChild(catTitle);

     var row = document.createElement("div");
     row.style.display = "flex";
     row.style.flexWrap = "wrap";
     row.style.gap = "16px 0";
     row.style.alignItems = "flex-start";

     groups[cat].forEach(function(d) {
        var p = d.data_str.split("-");
        var dataFmt = p[2] + " " + MESES[parseInt(p[1]) - 1] + " " + p[0];

        var box = document.createElement("div");
        box.className = "pg-item";

        var logoUrl = ESCUDOS[d.clube];
        var imgHtml = "";
        if (logoUrl) {
            imgHtml = `<img src="${logoUrl}" class="pg-item-logo" onerror="this.style.display='none'">`;
        } else {
            var fallbackText = (SIGLAS[d.clube] || d.clube.substring(0,3)).toUpperCase();
            imgHtml = `<div style="width:22px;height:22px;background:#EAE8E3;border-radius:50%;display:flex;align-items:center;justify-content:center;font-size:7px;font-weight:bold;margin-bottom:6px;color:#333;">${fallbackText}</div>`;
        }

        box.innerHTML = `
          ${imgHtml}
          <div class="pg-item-comp" title="${d.competicao}">${d.competicao}</div>
          <div class="pg-item-score">${d.partida}</div>
          <div class="pg-item-date">${dataFmt}</div>
        `;

        row.appendChild(box);
     });

     catBlock.appendChild(row);
     container.appendChild(catBlock);
  }
})();

// geração do heatmap e minigráfico

var CORES = d3.scaleThreshold().domain([1,2,3,4,5]).range(["#F2EFEB","#B3C0D1","#E07B6A","#C95240","#B03020","#901A1E"]);
var CELL = 12, GAP = 3, CELL_STEP = CELL + GAP;
var PAD_LEFT = 48, PAD_TOP = 20, YGAP = 12;
var ROW_H = PAD_TOP + 7 * CELL_STEP;

var porAno = {};
DADOS.forEach(function(d) {
  if (!porAno[d.ano]) porAno[d.ano] = [];
  porAno[d.ano].push(d);
});

var anos = Array.from(new Set(DADOS.map(function(d){ return d.ano; }))).sort();
var SVG_W = PAD_LEFT + 54 * CELL_STEP + 16;
var SVG_H = anos.length * ROW_H + (anos.length - 1) * YGAP + 26;

var svg = d3.select("#heatmap-root").append("svg")
  .attr("viewBox", "0 0 " + SVG_W + " " + SVG_H)
  .attr("preserveAspectRatio", "xMinYMin meet");

var tip = document.getElementById("tooltip"), ttDate = document.getElementById("tt-date"), ttGols = document.getElementById("tt-gols"), ttAdv = document.getElementById("tt-adv"), ttComp = document.getElementById("tt-comp"), ttClube = document.getElementById("tt-clube"), ttLogo = document.getElementById("tt-logo-wrap"), ttMarco = document.getElementById("tt-marco"), ttCopa = document.getElementById("tt-copa");
var tipNexo = document.getElementById("tooltip-nexo");
var MESES_EXT = ["Janeiro","Fevereiro","Março","Abril","Maio","Junho","Julho","Agosto","Setembro","Outubro","Novembro","Dezembro"];

function showTip(event, d) {
  ttLogo.innerHTML = "";
  if (d.clube && d.gols_dia > 0) {
    var url = ESCUDOS[d.clube];
    if (url) {
      var img = document.createElement("img"); img.className = "clube-logo"; img.src = url;
      img.onerror = function() { var fb = makeFallback(d.clube, "tt-logo-fallback"); ttLogo.innerHTML = ""; ttLogo.appendChild(fb); };
      ttLogo.appendChild(img);
    } else { ttLogo.appendChild(makeFallback(d.clube, "tt-logo-fallback")); }
    ttClube.textContent = d.clube;
  } else { ttClube.textContent = ""; }

  var p = d.data_str.split("-");
  ttDate.textContent = p[2] + " de " + MESES_EXT[d.mes - 1] + " de " + d.ano;

  if (marcosMap[d.data_str]) {
    ttMarco.textContent = "★ " + marcosMap[d.data_str].texto_tooltip;
    ttMarco.style.display = "block";
  } else { ttMarco.style.display = "none"; }

  var isCopa = d.competicoes && (d.competicoes.toLowerCase().includes("copa do mundo") || d.competicoes.toLowerCase().includes("world cup"));
  if(isCopa) { ttCopa.style.display = "block"; } else { ttCopa.style.display = "none"; }

  if (d.gols_dia === 0) {
    ttGols.textContent = "Sem gols"; ttGols.style.color = "#999"; ttGols.style.fontSize = "16px"; ttAdv.textContent = ""; ttComp.textContent = "";
  } else {
    ttGols.textContent = d.gols_dia + (d.gols_dia > 1 ? " gols" : " gol"); ttGols.style.color = "#B03020"; ttGols.style.fontSize = "24px";
    ttAdv.textContent   = d.adversarios ? "vs " + d.adversarios : ""; ttComp.textContent = d.competicoes || "";
  }
  tip.style.opacity = "1";
  moveTip(event);
  }

function moveTip(event) {
  var x = event.pageX + 16, y = event.pageY - 16;
  if (x + 240 > window.innerWidth) x = event.pageX - 250;
  tip.style.left = x + "px"; tip.style.top = y + "px";
  }

function hideTip() { tip.style.opacity = "0"; tip.style.top = "-9999px"; }

anos.forEach(function(ano, ai) {
  var baseY = ai * (ROW_H + YGAP);
  svg.append("text").attr("x", PAD_LEFT - 14).attr("y", baseY + PAD_TOP + (3.5 * CELL_STEP)).attr("text-anchor", "end").attr("dominant-baseline", "middle").attr("font-size", 13).attr("font-weight", "700").attr("fill", "#111").attr("font-family", "Inter, sans-serif").text(ano);

  var diasAno = porAno[ano] || [];

  svg.selectAll(".dia-" + ano)
    .data(diasAno)
    .enter()
    .append("rect")
    .attr("class", "dia-" + ano)
    .attr("width", CELL)
    .attr("height", CELL)
    .attr("x", function(d) { return PAD_LEFT + d.semana_ano * CELL_STEP; })
    .attr("y", function(d) { return baseY + PAD_TOP + (d.dia_semana - 1) * CELL_STEP; })
    .attr("fill", function(d) { return d.gols_dia === 0 ? "#F2EFEB" : CORES(d.gols_dia); })
    .attr("stroke", function(d) {
      if (marcosMap[d.data_str]) return "#111";
      var isCopa = d.competicoes && (d.competicoes.toLowerCase().includes("copa do mundo") || d.competicoes.toLowerCase().includes("world cup"));
      if (isCopa) return "#C59B27";
      return "transparent";
    })
    .attr("stroke-width", function(d) {
      if (marcosMap[d.data_str]) return 1.5;
      var isCopa = d.competicoes && (d.competicoes.toLowerCase().includes("copa do mundo") || d.competicoes.toLowerCase().includes("world cup"));
      if (isCopa) return 2;
      return 0;
    })
    .attr("rx", 1.5)
    .attr("ry", 1.5)
    .on("mouseover", function(event, d) { showTip(event, d); })
    .on("mousemove", function(event) { moveTip(event); })
    .on("mouseleave", function() { hideTip(); });

  var diasMarcos = diasAno.filter(function(d) { return marcosMap[d.data_str]; });
  diasMarcos.forEach(function(d) {
     var col = d.semana_ano;
     var row = d.dia_semana - 1;
     var cx = PAD_LEFT + col * CELL_STEP;
     var cy = baseY + PAD_TOP + row * CELL_STEP;

     var isTopHalf = row < 3;
     var gridTop = baseY + PAD_TOP;
     var gridBottom = baseY + PAD_TOP + 7 * CELL_STEP;

     var lineY1 = isTopHalf ? cy : cy + CELL;
     var lineY2 = isTopHalf ? gridTop - 2 : gridBottom + 2;

     svg.append("line").attr("x1", cx + CELL/2).attr("y1", lineY1).attr("x2", cx + CELL/2).attr("y2", lineY2).attr("stroke", "#111").attr("stroke-width", 0.5).attr("stroke-dasharray", "2,2");

     var m = marcosMap[d.data_str];
     if (m.tipo !== 'marca') {
        var logoUrl = ESCUDOS[m.marco];
        if (logoUrl) {
            svg.append("image").attr("x", cx + CELL/2 - 7).attr("y", isTopHalf ? baseY + PAD_TOP - 16 : baseY + PAD_TOP + 7 * CELL_STEP + 4).attr("width", 14).attr("height", 14).attr("href", logoUrl).attr("xlink:href", logoUrl);
        }
     } else {
        var textY = isTopHalf ? gridTop - 4 : gridBottom + 12;
        svg.append("text").attr("x", cx + CELL/2).attr("y", textY).attr("text-anchor", "middle").attr("font-size", "11px").attr("fill", "var(--gold)").text("★");
     }
  });
});

function renderTop10Charts() {
  var jogadoresOrdenados = INFO_JOG.map(function(d) { return d.jogador; });
  var maxTemp = d3.max(DADOS_TOP10, function(d) { return d.temporada_carreira; });

  var marginH = {top: 20, right: 30, bottom: 40, left: 150},
      widthH = 1000 - marginH.left - marginH.right,
      heightH = 380 - marginH.top - marginH.bottom;

  var svgH = d3.select("#nexo-heatmap").append("svg")
      .attr("viewBox", `0 0 ${widthH + marginH.left + marginH.right} ${heightH + marginH.top + marginH.bottom}`)
      .append("g")
      .attr("transform", "translate(" + marginH.left + "," + marginH.top + ")");

  var xH = d3.scaleBand().domain(d3.range(1, maxTemp + 1)).range([0, widthH]).padding(0.05);
  var yH = d3.scaleBand().domain(jogadoresOrdenados).range([0, heightH]).padding(0.1);
  var colorH = d3.scaleSequential(d3.interpolateReds).domain([0, d3.max(DADOS_TOP10, function(d) { return d.gols_anuais; })]);

  svgH.append("g")
      .attr("transform", "translate(0," + heightH + ")")
      .call(d3.axisBottom(xH).tickFormat(function(d) { return "T" + d; }))
      .selectAll("text").style("font-size", "10px");

  svgH.append("g")
      .call(d3.axisLeft(yH))
      .selectAll("text").style("font-size", "12px").style("font-weight", "600");

  svgH.selectAll(".cell")
      .data(DADOS_TOP10)
      .enter().append("rect")
      .attr("x", function(d) { return xH(d.temporada_carreira); })
      .attr("y", function(d) { return yH(d.jogador); })
      .attr("width", xH.bandwidth())
      .attr("height", yH.bandwidth())
      .attr("fill", function(d) { return colorH(d.gols_anuais); })
      .attr("rx", 2)
      .attr("ry", 2)
      .on("mouseover", function(event, d) {
         tipNexo.style.opacity = "1";
         tipNexo.innerHTML = "<strong>" + d.jogador + "</strong><br/>Temporada " + d.temporada_carreira + " (" + d.ano + ")<br/>Gols: <span style='color:var(--accent);font-weight:700;'>" + d.gols_anuais + "</span><br/>Acumulado: " + d.gols_acumulados;
      })
      .on("mousemove", function(event) {
         tipNexo.style.left = (event.pageX + 12) + "px";
         tipNexo.style.top = (event.pageY - 12) + "px";
      })
      .on("mouseleave", function() { tipNexo.style.opacity = "0"; tipNexo.style.top = "-9999px"; });

  svgH.selectAll(".cell-text")
      .data(DADOS_TOP10.filter(function(d) { return d.gols_anuais > 0; }))
      .enter().append("text")
      .attr("x", function(d) { return xH(d.temporada_carreira) + xH.bandwidth()/2; })
      .attr("y", function(d) { return yH(d.jogador) + yH.bandwidth()/2; })
      .attr("text-anchor", "middle")
      .attr("dominant-baseline", "middle")
      .attr("fill", "#fff")
      .style("font-family", "Montserrat, sans-serif")
      .style("font-size", "10px")
      .style("font-weight", "bold")
      .style("pointer-events", "none")
      .text(function(d) { return d.gols_anuais; });

  var marginL = {top: 30, right: 180, bottom: 40, left: 60},
      widthL = 1000 - marginL.left - marginL.right,
      heightL = 450 - marginL.top - marginL.bottom;

  var svgL = d3.select("#nexo-lines").append("svg")
      .attr("viewBox", `0 0 ${widthL + marginL.left + marginL.right} ${heightL + marginL.top + marginL.bottom}`)
      .append("g")
      .attr("transform", "translate(" + marginL.left + "," + marginL.top + ")");

  var xL = d3.scaleLinear().domain([1, maxTemp]).range([0, widthL]);
  var yL = d3.scaleLinear().domain([0, d3.max(DADOS_TOP10, function(d) { return d.gols_acumulados; })]).range([heightL, 0]);

  var colorL = function(jogador) { return jogador === "Cristiano Ronaldo" ? "var(--accent)" : "#cbd5e1"; };

  svgL.append("g")
      .attr("transform", "translate(0," + heightL + ")")
      .call(d3.axisBottom(xL).tickFormat(function(d) { return "Temp " + d; }));

  svgL.append("g").call(d3.axisLeft(yL));

  var lineGen = d3.line()
      .x(function(d) { return xL(d.temporada_carreira); })
      .y(function(d) { return yL(d.gols_acumulados); })
      .curve(d3.curveMonotoneX);

  var dadosAgrupados = d3.groups(DADOS_TOP10, function(d) { return d.jogador; });

  dadosAgrupados.sort(function(a, b) {
      if(a[0] === "Cristiano Ronaldo") return 1;
      if(b[0] === "Cristiano Ronaldo") return -1;
      return 0;
  });

  dadosAgrupados.forEach(function(g) {
    svgL.append("path")
        .datum(g[1])
        .attr("fill", "none")
        .attr("stroke", colorL(g[0]))
        .attr("stroke-width", g[0] === "Cristiano Ronaldo" ? 4 : 2)
        .attr("opacity", g[0] === "Cristiano Ronaldo" ? 1 : 0.6)
        .attr("d", lineGen);

    var last = g[1][g[1].length - 1];
    svgL.append("text")
        .attr("x", xL(last.temporada_carreira) + 8)
        .attr("y", yL(last.gols_acumulados))
        .attr("dominant-baseline", "middle")
        .style("font-family", "Montserrat")
        .style("font-size", "11px")
        .style("font-weight", g[0] === "Cristiano Ronaldo" ? "700" : "500")
        .style("fill", g[0] === "Cristiano Ronaldo" ? "var(--accent)" : "#64748B")
        .text(g[0]);
  });

  svgL.selectAll(".marcadores-100")
      .data(PONTOS_MARCAS)
      .enter().append("circle")
      .attr("cx", function(d) { return xL(d.temporada_carreira); })
      .attr("cy", function(d) { return yL(d.gols_acumulados); })
      .attr("r", function(d) { return d.jogador === "Cristiano Ronaldo" ? 4.5 : 2.5; })
      .attr("fill", "#fff")
      .attr("stroke", function(d) { return colorL(d.jogador); })
      .attr("stroke-width", function(d) { return d.jogador === "Cristiano Ronaldo" ? 2 : 1; })
      .style("cursor", "pointer")
      .on("mouseover", function(event, d) {
         tipNexo.style.opacity = "1";
         tipNexo.innerHTML = "<strong style='color:#111;'> " + d.jogador + "</strong><br/>" +
                             "<span style='color:var(--accent); font-weight:700;'>" + d.gols_acumulados + "</span> gols alcançados na <br/>" +
                             "Temporada <b>" + d.temporada_carreira + "</b> (" + d.ano + ")";
      })
      .on("mousemove", function(event) {
         tipNexo.style.left = (event.pageX + 12) + "px";
         tipNexo.style.top = (event.pageY - 12) + "px";
      })
      .on("mouseleave", function() { tipNexo.style.opacity = "0"; tipNexo.style.top = "-9999px"; });

  var marginA = {top: 10, right: 80, bottom: 20, left: 140},
      widthA = 500 - marginA.left - marginA.right,
      heightA = 320 - marginA.top - marginA.bottom;

  var svgA = d3.select("#nexo-auge").append("svg")
      .attr("viewBox", `0 0 ${widthA + marginA.left + marginA.right} ${heightA + marginA.top + marginA.bottom}`)
      .attr("width", "100%")
      .append("g")
      .attr("transform", "translate(" + marginA.left + "," + marginA.top + ")");

  var yA = d3.scaleBand().domain(DADOS_AUGE.map(function(d) { return d.jogador; })).range([0, heightA]).padding(0.2);
  var xA = d3.scaleLinear().domain([0, d3.max(DADOS_AUGE, function(d) { return d.gols_anuais; })]).range([0, widthA]);

  svgA.append("g").call(d3.axisLeft(yA).tickSize(0)).select(".domain").remove();
  svgA.selectAll(".tick text").style("font-family", "Inter, sans-serif").style("font-size", "11px").style("font-weight", "600");

  svgA.selectAll(".bar")
      .data(DADOS_AUGE)
      .enter().append("rect")
      .attr("class", "bar")
      .attr("y", function(d) { return yA(d.jogador); })
      .attr("x", 0)
      .attr("height", yA.bandwidth())
      .attr("width", function(d) { return xA(d.gols_anuais); })
      .attr("fill", function(d) { return d.jogador === "Cristiano Ronaldo" ? "var(--accent)" : "#cbd5e1"; })
      .attr("rx", 3);

  svgA.selectAll(".label")
      .data(DADOS_AUGE)
      .enter().append("text")
      .attr("class", "label")
      .attr("y", function(d) { return yA(d.jogador) + yA.bandwidth() / 2; })
      .attr("x", function(d) { return xA(d.gols_anuais) + 6; })
      .attr("dy", ".35em")
      .style("font-family", "Inter, sans-serif")
      .style("font-size", "10px")
      .style("font-weight", "700")
      .style("fill", function(d) { return d.jogador === "Cristiano Ronaldo" ? "var(--accent)" : "#475569"; })
      .text(function(d) { return d.gols_anuais + " gols (T" + d.temporada_carreira + ")"; });

  var marginHT = {top: 10, right: 40, bottom: 20, left: 140},
      widthHT = 500 - marginHT.left - marginHT.right,
      heightHT = 320 - marginHT.top - marginHT.bottom;

  var svgHT = d3.select("#nexo-hattricks").append("svg")
      .attr("viewBox", `0 0 ${widthHT + marginHT.left + marginHT.right} ${heightHT + marginHT.top + marginHT.bottom}`)
      .attr("width", "100%")
      .append("g")
      .attr("transform", "translate(" + marginHT.left + "," + marginHT.top + ")");

  var yHT = d3.scaleBand().domain(DADOS_HT.map(function(d) { return d.jogador; })).range([0, heightHT]).padding(1);
  var xHT = d3.scaleLinear().domain([0, d3.max(DADOS_HT, function(d) { return d.qtd_hat_tricks; })]).range([0, widthHT]);

  svgHT.append("g").call(d3.axisLeft(yHT).tickSize(0)).select(".domain").remove();
  svgHT.selectAll(".tick text").style("font-family", "Inter, sans-serif").style("font-size", "11px").style("font-weight", "600");

  svgHT.selectAll("myline")
      .data(DADOS_HT)
      .enter().append("line")
      .attr("x1", 0)
      .attr("x2", function(d) { return xHT(d.qtd_hat_tricks); })
      .attr("y1", function(d) { return yHT(d.jogador); })
      .attr("y2", function(d) { return yHT(d.jogador); })
      .attr("stroke", function(d) { return d.jogador === "Cristiano Ronaldo" ? "var(--accent)" : "#cbd5e1"; })
      .attr("stroke-width", "2px");

  svgHT.selectAll("mycircle")
      .data(DADOS_HT)
      .enter().append("circle")
      .attr("cx", function(d) { return xHT(d.qtd_hat_tricks); })
      .attr("cy", function(d) { return yHT(d.jogador); })
      .attr("r", 5)
      .attr("fill", function(d) { return d.jogador === "Cristiano Ronaldo" ? "var(--accent)" : "#94a3b8"; });

  svgHT.selectAll(".labelHT")
      .data(DADOS_HT)
      .enter().append("text")
      .attr("class", "labelHT")
      .attr("y", function(d) { return yHT(d.jogador); })
      .attr("x", function(d) { return xHT(d.qtd_hat_tricks) + 10; })
      .attr("dy", ".35em")
      .style("font-family", "Inter, sans-serif")
      .style("font-size", "10px")
      .style("font-weight", "700")
      .style("fill", function(d) { return d.jogador === "Cristiano Ronaldo" ? "var(--accent)" : "#475569"; })
      .text(function(d) { return d.qtd_hat_tricks; });

  var marginE = {top: 10, right: 80, bottom: 20, left: 140},
      widthE = 500 - marginE.left - marginE.right,
      heightE = 320 - marginE.top - marginE.bottom;

  var svgE = d3.select("#nexo-eficiencia").append("svg")
      .attr("viewBox", `0 0 ${widthE + marginE.left + marginE.right} ${heightE + marginE.top + marginE.bottom}`)
      .attr("width", "100%")
      .append("g")
      .attr("transform", "translate(" + marginE.left + "," + marginE.top + ")");

  var yE = d3.scaleBand().domain(DADOS_EFI.map(function(d) { return d.jogador; })).range([0, heightE]).padding(0.2);
  var xE = d3.scaleLinear().domain([0, d3.max(DADOS_EFI, function(d) { return d.media_gols_partida; })]).range([0, widthE]);

  svgE.append("g").call(d3.axisLeft(yE).tickSize(0)).select(".domain").remove();
  svgE.selectAll(".tick text").style("font-family", "Inter, sans-serif").style("font-size", "11px").style("font-weight", "600");

  svgE.selectAll(".bar")
      .data(DADOS_EFI)
      .enter().append("rect")
      .attr("class", "bar")
      .attr("y", function(d) { return yE(d.jogador); })
      .attr("x", 0)
      .attr("height", yE.bandwidth())
      .attr("width", function(d) { return xE(d.media_gols_partida); })
      .attr("fill", function(d) { return d.jogador === "Cristiano Ronaldo" ? "var(--accent)" : "#cbd5e1"; })
      .attr("rx", 3);

  svgE.selectAll(".label")
      .data(DADOS_EFI)
      .enter().append("text")
      .attr("class", "label")
      .attr("y", function(d) { return yE(d.jogador) + yE.bandwidth() / 2; })
      .attr("x", function(d) { return xE(d.media_gols_partida) + 6; })
      .attr("dy", ".35em")
      .style("font-family", "Inter, sans-serif")
      .style("font-size", "10px")
      .style("font-weight", "700")
      .style("fill", function(d) { return d.jogador === "Cristiano Ronaldo" ? "var(--accent)" : "#475569"; })
      .text(function(d) { return d.media_gols_partida + " /jogo"; });

  var widthR = 400, heightR = 300, innerR = 20, outerR = 120;
  var svgR = d3.select("#nexo-radar").append("svg")
      .attr("viewBox", `0 0 ${widthR} ${heightR}`)
      .attr("width", "100%")
      .append("g")
      .attr("transform", "translate(" + (widthR/2) + "," + (heightR/2) + ")");

  var xR = d3.scaleBand().range([0, 2 * Math.PI]).domain(DADOS_RADAR.map(function(d) { return d.dia; })).padding(0.1);
  var yR = d3.scaleLinear().range([innerR, outerR]).domain([0, d3.max(DADOS_RADAR, function(d) { return Math.max(d.media_cr7, d.media_outros); })]);

  svgR.append("g").selectAll("path").data(DADOS_RADAR).enter().append("path")
      .attr("fill", "rgba(148, 163, 184, 0.2)")
      .attr("stroke", "#94A3B8")
      .attr("stroke-width", "1.5px")
      .attr("d", d3.arc().innerRadius(innerR).outerRadius(function(d) { return yR(d.media_outros); }).startAngle(function(d) { return xR(d.dia); }).endAngle(function(d) { return xR(d.dia) + xR.bandwidth(); }).padAngle(0.02).padRadius(innerR));

  svgR.append("g").selectAll("path").data(DADOS_RADAR).enter().append("path")
      .attr("fill", "var(--accent)").attr("opacity", 0.85)
      .attr("d", d3.arc().innerRadius(innerR).outerRadius(function(d) { return yR(d.media_cr7); }).startAngle(function(d) { return xR(d.dia); }).endAngle(function(d) { return xR(d.dia) + xR.bandwidth(); }).padAngle(0.02).padRadius(innerR))
      .style("cursor", "pointer")
      .on("mouseover", function(event, d) {
         tipNexo.style.opacity = "1";
         tipNexo.innerHTML = "<b>" + d.dia + "</b><br/>Média CR7: <b>" + d.media_cr7 + " gols/j</b><br/><span style='color:#64748b'>Média Rivais: " + d.media_outros + " gols/j</span>";
      })
      .on("mousemove", function(event) {
         tipNexo.style.left = (event.pageX + 12) + "px";
         tipNexo.style.top = (event.pageY - 12) + "px";
      })
      .on("mouseleave", function() { tipNexo.style.opacity = "0"; tipNexo.style.top = "-9999px"; });

  svgR.append("g").selectAll("g").data(DADOS_RADAR).enter().append("g")
      .attr("text-anchor", function(d) { return (xR(d.dia) + xR.bandwidth()/2 + Math.PI) % (2*Math.PI) < Math.PI ? "end" : "start"; })
      .attr("transform", function(d) { return "rotate(" + ((xR(d.dia) + xR.bandwidth()/2) * 180 / Math.PI - 90) + ")translate(" + (128) + ",0)"; })
      .append("text").text(function(d) { return d.dia; })
      .attr("transform", function(d) { return (xR(d.dia) + xR.bandwidth()/2 + Math.PI) % (2*Math.PI) < Math.PI ? "rotate(180)" : "rotate(0)"; })
      .style("font-size", "10px").style("font-weight", "700").style("font-family", "Inter, sans-serif").style("fill", "#475569").attr("alignment-baseline", "middle");

  var marginF = {top: 30, right: 80, bottom: 20, left: 140},
      widthF = 1000 - marginF.left - marginF.right,
      heightF = 260 - marginF.top - marginF.bottom;

  var svgF = d3.select("#nexo-fases").append("svg")
      .attr("viewBox", `0 0 ${widthF + marginF.left + marginF.right} ${heightF + marginF.top + marginF.bottom}`)
      .append("g")
      .attr("transform", "translate(" + marginF.left + "," + marginF.top + ")");

  var fasesMap = d3.group(DADOS_FASES, function(d) { return d.jogador; });
  var stackData = Array.from(fasesMap, function(d) {
     var o = { jogador: d[0], total: 0 };
     d[1].forEach(function(x) { o[x.fase] = x.gols_fase; o.total += x.gols_fase; });
     return o;
  }).sort(function(a,b) { return b.total - a.total; });

  var kF = ["Início (T1-T5)", "Auge (T6-T12)", "Maturidade (T13+)"];
  var colF = d3.scaleOrdinal().domain(kF).range(["#E5E5E5", "#E07B6A", "#901A1E"]);
  var seriesF = d3.stack().keys(kF)(stackData);

  var yF = d3.scaleBand().domain(stackData.map(function(d) { return d.jogador; })).range([0, heightF]).padding(0.3);
  var xF = d3.scaleLinear().domain([0, d3.max(stackData, function(d) { return d.total; })]).range([0, widthF]);

  svgF.append("g").call(d3.axisLeft(yF).tickSize(0)).select(".domain").remove();

  svgF.selectAll(".tick text")
      .style("font-family", "Inter, sans-serif")
      .style("font-size", "12px")
      .style("font-weight", function(d) { return d === "Cristiano Ronaldo" ? "800" : "500"; })
      .attr("fill", "#1E293B");

  var groupsF = svgF.selectAll("g.layer")
      .data(seriesF)
      .enter().append("g")
      .attr("class", "layer")
      .attr("fill", function(d) { return colF(d.key); });

  groupsF.selectAll("rect")
      .data(function(d) { d.forEach(function(v) { v.key = d.key; }); return d; })
      .enter().append("rect")
      .attr("y", function(d) { return yF(d.data.jogador); })
      .attr("x", function(d) { return xF(d[0]); })
      .attr("width", function(d) { return xF(d[1]) - xF(d[0]); })
      .attr("height", yF.bandwidth())
      .style("cursor", "pointer")
      .on("mouseover", function(event, d) {
          tipNexo.style.opacity = "1";
          var gols = d[1] - d[0];
          tipNexo.innerHTML = "<strong style='color:#111;'>" + d.data.jogador + "</strong><br/>" +
                              "Fase: <b>" + d.key + "</b><br/>" +
                              "Gols no período: <span style='color:var(--accent);font-weight:700;'>" + gols + "</span>";
      })
      .on("mousemove", function(event) {
          tipNexo.style.left = (event.pageX + 12) + "px";
          tipNexo.style.top = (event.pageY - 12) + "px";
      })
      .on("mouseleave", function() { tipNexo.style.opacity = "0"; tipNexo.style.top = "-9999px"; });

  svgF.selectAll(".labelF")
      .data(stackData)
      .enter().append("text")
      .attr("x", function(d) { return xF(d.total) + 8; })
      .attr("y", function(d) { return yF(d.jogador) + yF.bandwidth()/2; })
      .attr("dy", ".35em")
      .style("font-family", "Inter, sans-serif")
      .style("font-size", "11px")
      .style("font-weight", "700")
      .style("fill", "#475569")
      .text(function(d) { return d.total + " gols"; });

  var legF = svgF.selectAll(".legF").data(kF).enter().append("g").attr("transform", function(d,i) { return "translate(" + (i * 150) + ", -15)"; });
  legF.append("rect").attr("width", 12).attr("height", 12).attr("fill", function(d) { return colF(d); }).attr("rx", 2);
  legF.append("text").attr("x", 18).attr("y", 10).style("font-size", "11px").style("font-family", "Inter, sans-serif").style("fill", "#475569").text(function(d) { return d; });


  // GRÁFICO DE LETALIDADE POR COMPETIÇÃO (STYLE PROGRESS TRACK)
  var marginC = {top: 20, right: 60, bottom: 20, left: 160},
      widthC = 1000 - marginC.left - marginC.right,
      heightC = 550 - marginC.top - marginC.bottom;

  var svgC = d3.select("#cr7-gols-comp").append("svg")
      .attr("viewBox", `0 0 ${widthC + marginC.left + marginC.right} ${heightC + marginC.top + marginC.bottom}`)
      .attr("width", "100%")
      .append("g")
      .attr("transform", "translate(" + marginC.left + "," + marginC.top + ")");

  var yC = d3.scaleBand().domain(GOLS_POR_COMP.map(function(d) { return d.competicao; })).range([0, heightC]).padding(0.3);
  var xC = d3.scaleLinear().domain([0, d3.max(GOLS_POR_COMP, function(d) { return d.total_gols; })]).range([0, widthC]);

  var defsC = svgC.append("defs");
  var gradC = defsC.append("linearGradient").attr("id", "gradC").attr("x1", "0%").attr("x2", "100%").attr("y1", "0%").attr("y2", "0%");
  gradC.append("stop").attr("offset", "0%").style("stop-color", "#E07B6A");
  gradC.append("stop").attr("offset", "100%").style("stop-color", "#B03020");

  svgC.append("g")
      .attr("class", "grid")
      .attr("transform", "translate(0," + heightC + ")")
      .call(d3.axisBottom(xC).tickSize(-heightC).tickFormat("").ticks(8))
      .selectAll("line").attr("stroke", "#E2E8F0").attr("stroke-dasharray", "3,3");
  svgC.selectAll(".domain").remove();

  svgC.append("g").call(d3.axisLeft(yC).tickSize(0)).select(".domain").remove();
  svgC.selectAll(".tick text")
      .style("font-family", "Inter, sans-serif")
      .style("font-size", "11px")
      .style("font-weight", "600")
      .attr("fill", "#475569");

  svgC.selectAll(".track")
      .data(GOLS_POR_COMP)
      .enter().append("rect")
      .attr("y", function(d) { return yC(d.competicao) + yC.bandwidth()/2 - 4; })
      .attr("x", 0)
      .attr("height", 8)
      .attr("width", widthC)
      .attr("fill", "#F1F5F9")
      .attr("rx", 4);

  svgC.selectAll(".barC")
      .data(GOLS_POR_COMP)
      .enter().append("rect")
      .attr("class", "barC")
      .attr("y", function(d) { return yC(d.competicao) + yC.bandwidth()/2 - 4; })
      .attr("x", 0)
      .attr("height", 8)
      .attr("width", function(d) { return xC(d.total_gols); })
      .attr("fill", "url(#gradC)")
      .attr("rx", 4)
      .style("cursor", "pointer")
      .on("mouseover", function(event, d) {
         d3.select(this).attr("fill", "#901A1E");
         d3.select(this.parentNode).select(".bullet-" + d.competicao.replace(/[^a-zA-Z0-9]/g, "")).attr("r", 7).attr("fill", "#111");
         tipNexo.style.opacity = "1";
         tipNexo.innerHTML = "<b>" + d.competicao + "</b><br/>Gols totais: <span style='color:var(--accent);font-weight:700;'>" + d.total_gols + "</span>";
      })
      .on("mousemove", function(event) {
         tipNexo.style.left = (event.pageX + 12) + "px";
         tipNexo.style.top = (event.pageY - 12) + "px";
      })
      .on("mouseleave", function(event, d) {
         d3.select(this).attr("fill", "url(#gradC)");
         d3.select(this.parentNode).select(".bullet-" + d.competicao.replace(/[^a-zA-Z0-9]/g, "")).attr("r", 5).attr("fill", "#B03020");
         tipNexo.style.opacity = "0"; tipNexo.style.top = "-9999px";
      });

  svgC.selectAll(".bulletC")
      .data(GOLS_POR_COMP)
      .enter().append("circle")
      .attr("class", function(d) { return "bullet-" + d.competicao.replace(/[^a-zA-Z0-9]/g, ""); })
      .attr("cy", function(d) { return yC(d.competicao) + yC.bandwidth()/2; })
      .attr("cx", function(d) { return xC(d.total_gols); })
      .attr("r", 5)
      .attr("fill", "#B03020")
      .attr("stroke", "#fff")
      .attr("stroke-width", 2)
      .style("pointer-events", "none")
      .style("transition", "all 0.2s ease");

  svgC.selectAll(".labelC")
      .data(GOLS_POR_COMP)
      .enter().append("text")
      .attr("y", function(d) { return yC(d.competicao) + yC.bandwidth()/2; })
      .attr("x", function(d) { return xC(d.total_gols) + 12; })
      .attr("dy", ".35em")
      .style("font-family", "Inter, sans-serif")
      .style("font-size", "11px")
      .style("font-weight", "800")
      .style("fill", "#1E293B")
      .text(function(d) { return d.total_gols; });

  }

function renderCalendario() {
  var marginCal = {top: 40, right: 20, bottom: 20, left: 80},
      widthCal = 1000 - marginCal.left - marginCal.right,
      heightCal = 400 - marginCal.top - marginCal.bottom;

  var svgCal = d3.select("#nexo-calendario").append("svg")
      .attr("viewBox", `0 0 ${widthCal + marginCal.left + marginCal.right} ${heightCal + marginCal.top + marginCal.bottom}`)
      .append("g")
      .attr("transform", "translate(" + marginCal.left + "," + marginCal.top + ")");

  var meses_nomes = ["Janeiro", "Fevereiro", "Março", "Abril", "Maio", "Junho", "Julho", "Agosto", "Setembro", "Outubro", "Novembro", "Dezembro"];
  var dias_labels = d3.range(1, 32);

  var xCal = d3.scaleBand().domain(dias_labels).range([0, widthCal]).padding(0.05);
  var yCal = d3.scaleBand().domain(d3.range(1, 13)).range([0, heightCal]).padding(0.1);

  svgCal.append("g")
      .attr("transform", "translate(0,-10)")
      .call(d3.axisTop(xCal).tickSize(0))
      .select(".domain").remove();

  svgCal.append("g")
      .call(d3.axisLeft(yCal).tickFormat(d => meses_nomes[d-1]).tickSize(0))
      .select(".domain").remove();

  svgCal.selectAll(".tick text")
      .style("font-family", "Inter, sans-serif")
      .style("font-size", "12px")
      .style("font-weight", "600")
      .style("fill", "#475569");

  svgCal.selectAll(".cell-cal")
      .data(DADOS_CALENDARIO)
      .enter().append("rect")
      .attr("class", "cell-cal")
      .attr("x", function(d) { return xCal(d.dia); })
      .attr("y", function(d) { return yCal(d.mes); })
      .attr("width", xCal.bandwidth())
      .attr("height", yCal.bandwidth())
      .attr("fill", function(d) { return d.marcou === 1 ? "var(--accent)" : "#F2EFEB"; })
      .attr("rx", 2)
      .attr("ry", 2)
      .style("cursor", function(d) { return d.marcou === 1 ? "pointer" : "default"; })
      .on("mouseover", function(event, d) {
         if(d.marcou === 1) {
             tipNexo.style.opacity = "1";
             tipNexo.innerHTML = "<strong>" + d.dia + " de " + meses_nomes[d.mes-1] + "</strong><br/>" +
                                 "Gols marcados: <span style='color:var(--accent);font-weight:700;'>" + d.total_gols + "</span><br/>" +
                                 "Anos: <span style='font-size:10px; color:#666;'>" + d.anos_marcados + "</span>";
         } else {
             tipNexo.style.opacity = "1";
             tipNexo.innerHTML = "<strong>" + d.dia + " de " + meses_nomes[d.mes-1] + "</strong><br/><span style='color:#666;'>Nenhum gol na carreira.</span>";
         }
         d3.select(this).attr("stroke", "#111").attr("stroke-width", 1.5);
      })
      .on("mousemove", function(event) {
         tipNexo.style.left = (event.pageX + 12) + "px";
         tipNexo.style.top = (event.pageY - 12) + "px";
      })
      .on("mouseleave", function() {
         tipNexo.style.opacity = "0"; tipNexo.style.top = "-9999px";
         d3.select(this).attr("stroke", "none");
      });
}

function renderTabelaDetalhes() {
  var tbody = document.querySelector("#detalhes-tabela tbody");
  tbody.innerHTML = "";

  TABELA_DETALHES.forEach(function(d) {
    var tr = document.createElement("tr");
    tr.innerHTML = `
      <td style="white-space: nowrap;">${d.data_str}</td>
      <td style="color: #111; font-weight: 500;">${d.partida}</td>
      <td>${d.clube}</td>
      <td>${d.adversario}</td>
      <td>${d.competicao}</td>
      <td style="text-align: center; font-weight: 900; color: var(--accent); font-family: 'Playfair Display', serif; font-size: 16px;">${d.gols}</td>
    `;
    tbody.appendChild(tr);
  });
}

function exportarCSV(filename) {
  var csv = [];
  // Cabeçalho
  csv.push("Data,Partida,Clube,Adversario,Competicao,Gols");

  // Linhas
  TABELA_DETALHES.forEach(function(d) {
    // Escapar aspas duplas caso existam e colocar os textos entre aspas para evitar quebra no CSV por causa de vírgulas
    var row = [
      d.data_str,
      '"' + (d.partida || "").replace(/"/g, '""') + '"',
      '"' + (d.clube || "").replace(/"/g, '""') + '"',
      '"' + (d.adversario || "").replace(/"/g, '""') + '"',
      '"' + (d.competicao || "").replace(/"/g, '""') + '"',
      d.gols
    ];
    csv.push(row.join(","));
  });

  // \uFEFF é o BOM (Byte Order Mark). Ele garante que o Excel identifique o arquivo como UTF-8 e não quebre a acentuação.
  var csvFile = new Blob(["\uFEFF" + csv.join("\n")], { type: "text/csv;charset=utf-8;" });

  var downloadLink = document.createElement("a");
  downloadLink.download = filename;
  downloadLink.href = window.URL.createObjectURL(csvFile);
  downloadLink.style.display = "none";
  document.body.appendChild(downloadLink);
  downloadLink.click();
  document.body.removeChild(downloadLink);
}

renderTop10Charts();
renderCalendario();
renderTabelaDetalhes();
</script>
</body>
</html>
)---")

# output final ------------------------------------------------------------

## exportando o dashboard interativo para um arquivo html
writeLines(html_final, "./docs/index.html", useBytes = TRUE)
message("Relatorio gerado.")
