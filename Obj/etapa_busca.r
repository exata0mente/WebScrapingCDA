# -[ ] Validar se há obras que não possuem campos iguais a todas as outras

rm(list=ls())

library(httr)
library(rvest)

## Etapa de Sessão
url_acesso <- "https://www.catalogodasartes.com.br/inicio"

sessao <- html_session(url_acesso)

listaObrasHome <- sessao %>%
  html_nodes(".card-reveal")
