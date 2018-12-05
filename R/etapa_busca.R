# Referências Úteis
  # https://github.com/yusuzech/r-web-scraping-cheat-sheet
  # https://www.curso-r.com/blog/2017-09-07-pacote-miojo/

rm(list=ls())

library(httr)
library(rvest)
library(dplyr)


## Etapa de Sessão
url_acesso <- "https://www.catalogodasartes.com.br/inicio"
sessao <- html_session(url_acesso)

## Carrega os produtos da home page
listaObrasHome <-sessao %>%
  html_nodes(".card.produto")

c_principal <- c("Tipo",
                 "Artista",
                 "Título",
                 "Técnica",
                 "Dimensões",
                 "Descrição",
                 "Avaliação da Obra",
                 "Data da Pesquisa")

## Define variáveis
df_principal <- data.frame(matrix(nrow = 0, ncol = length(c_principal)))
names(df_principal) <- c_principal
n <- length(listaObrasHome)

# Realiza a mineração retornando uma lista
for(i in 1:n){
  
  c <- listaObrasHome[i] %>%
    html_nodes(".detalhes-titulo") %>%
    html_text()
  c <- combine("Tipo", c)
  
  r <- listaObrasHome[i] %>%
    html_nodes(".card-content span span:last-child, .detalhes-texto, .detalhes-texto-justificado") %>%
    html_text()

  m <- length(c)
  
  for(j in 1:m){
    if(!any(c_principal == c[j])){
      c_principal <- combine(c_principal, c[j])
      df_principal <- mutate(df_principal, nc = NA)
      names(df_principal) <- c_principal
    }
    pos <- which(c_principal == c[j])
    df_principal[i,pos] <- r[j]
  }
    
}

saveRDS(df_principal,"20181201.RDS")
write.csv(df_principal, "20181201_listaObrasHome.csv")
