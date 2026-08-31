# adicionando novo gol manualmente ----------------------------------------

## carregando a base historica local
caminho_base <- "./data/base_gols_historico.csv"
base_historica <- read.csv(caminho_base, encoding = "UTF-8") |>
  dplyr::mutate(data = as.Date(data))

## preenchendo as informacoes da nova partida
novo_gol <- data.frame(
  jogador = "Cristiano Ronaldo",
  data = as.Date("2026-08-31"), ## formato aaaa-mm-dd
  partida = "Al Nassr 2x1 Adversário", ## time_cr7 placar x placar adversario
  gols = 1, ## quantidade de gols que ele fez nesse jogo
  competicao = "Campeonato Árabe", ## nome do torneio
  pais_origem = "Portugal" ## fixo
)

## anexando o novo gol e salvando o arquivo limpo
base_atualizada <- rbind(base_historica, novo_gol)

write.csv(base_atualizada, caminho_base, row.names = FALSE, fileEncoding = "UTF-8")

## emitindo mensagem de confirmacao visual
message("Sucesso! Total de gols do CR7 atualizado para: ",
        sum(base_atualizada$gols[base_atualizada$jogador == "Cristiano Ronaldo"], na.rm = TRUE))
