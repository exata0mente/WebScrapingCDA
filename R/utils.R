numeroPaginas <- function(x){ 
  k <- c()
  for (i in 1:length(x)) {
    if(x[i] != "") 
      k <- c(k, x[i])
  }
  k
}

cardToDf <- function(card,url){
  #' Organiza os cards em data.frames
  #' 
  #' Esta função interna recebe a lista dos elementos dos produtos
  #' e a lista de urls (que não vêm na estrutura direta dos produtos)
  #' e organiza um data.frame utilizando os conteúdos de título (índices
  #' ímpares) em varíaveis e os conteúdos de texto (índices pares) em
  #' regisitros/observações. Retorna uma lista com os produtos prontos
  #' para serem parseados em JSON
  #' 
  #' @param card lista com os elementos de produtos
  #' @param url lista com as urls dos produtos
  
  card[length(card)] <- "Url"
  card <- c(card, url)
  
  dfTmp <- card[c(TRUE, FALSE)] %>%
    t() %>%
    as.data.frame(stringsAsFactors = FALSE)
  
  cabTmp <- c("Tipo",card[c(FALSE, TRUE)])
  names(dfTmp) <- cabTmp
  
  dfTmp
}