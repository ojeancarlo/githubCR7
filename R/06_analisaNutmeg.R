# 1. Instale o pacote reticulate (caso ainda não tenha)

library(reticulate)

# IMPORTANTE: Você precisa garantir que as bibliotecas Python estejam instaladas.
# Você pode rodar isso no terminal do RStudio ou usar a função abaixo:
# py_install(packages = c("curl_cffi", "pandas", "lxml"), pip = TRUE)

# 2. Execute o código Python encapsulado como uma string
py_run_string("
from curl_cffi import requests
import pandas as pd

url = 'https://fbref.com/en/players/dea698d9/Cristiano-Ronaldo'
response = requests.get(url, impersonate='chrome')

tabelas = pd.read_html(response.text)
df_cr7 = tabelas[0]

# Tratamento do MultiIndex
df_cr7.columns = ['_'.join(col).strip() for col in df_cr7.columns.values]

colunas_interesse = [
    'Unnamed: 0_level_0_Season',
    'Unnamed: 2_level_0_Squad',
    'Unnamed: 3_level_0_Comp',
    'Performance_Gls'
]

df_gols = df_cr7[colunas_interesse].copy()
df_gols.columns = ['Temporada', 'Clube', 'Competicao', 'Gols']

df_gols = df_gols.dropna(subset=['Gols'])
df_gols = df_gols[~df_gols['Temporada'].str.contains('Total|yr|Season', na=False, case=False)]
")

# 3. Transfira o objeto do Python para o R usando o prefixo 'py$'
df_gols_r <- py$df_gols

# 4. Agora você tem um data.frame 100% nativo do R para continuar sua análise
print(head(df_gols_r))
