

# organizando o pipeline do projeto ---------------------------------------


message("Iniciando a esteira do projeto...")

## carregando os pacotes
message("Executando Passo 0: Carregando os pacotes via 01_pacotes")
source("./R/01_pacotes.R", encoding = "UTF-8")

## extraindo a lista de artilheiros
message("Executando Passo 1: Extraindo a lista de artilheiros via 01_extraiDados")
source("./R/02_extraiDados.R", encoding = "UTF-8")

## extraindoos detalhes dos gols
message("Executando Passo 2: Buscando o detalhamento dos gols via 02_extraiDetalheGols")
source("./R/03_extraiDetalheGols.R", encoding = "UTF-8")

## gerando painel interativo
message("Executando Passo 3: Renderizando o relatório via 03_pageGitCR7")
source("./R/04_pageGitCR7.R", encoding = "UTF-8")

## mensagem final
message("Pipeline finalizado com sucesso! O painel gitcr7 foi atualizado.")

