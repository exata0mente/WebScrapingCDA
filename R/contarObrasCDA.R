contarObrasCDA <- function(remDr = NULL, expressao = character(), verbose = TRUE){
  #' Pesquisa por assunto
  #'
  #' Recebe um string com assunto livre e utiliza a caixa de texto "Pesquisa" para
  #' contar quantas obras disponíveis há no site.
  #' 
  #' @param remDr objeto remoteDriver
  #' @param expressao texto livre a ser pesquisado
  #' @param verbose para apresentação de mensagens sobre as quantidades encontradas
  #' 
  #' @import utils
  #' @export
  
  # Manipulação dos elementos para efetuar a pesquisa no navegador
  eFormPesquisa <- remDr$findElement(using = "xpath", value = "//input[@name='pesquisa']")
  
  expressao %>%
    list() %>%
    eFormPesquisa$sendKeysToElement()
  
  remDr$findElement(using = "xpath", value = "//button[@data-action='pesquisar']")$clickElement()
  
  # Com o código fonte da página resultante será mais fácil coletar os elementos de interesse
  pageSource <- remDr$getPageSource()
  pgFonte <- pageSource[[1]] %>%
    read_html()
  
  # Índices de páginas
  h5 <-pgFonte %>% 
    html_nodes(xpath = "//h5") %>%
    html_text()
  
  oQntdObras <- as.numeric(gsub( "\\D+", "", h5[1]))
  oPgIndice <- strsplit(h5[2], "\\D") %>%
    lapply(numeroPaginas) %>% 
    unlist()
  
  if(verbose){
    message(paste0("Exrpressão: ", expressao, 
                   "\nQuantidade de Obras: ", oQntdObras,
                   "\nPáginas: ", oPgIndice[2]
    )
    )
  }
  
  list(paginas = oPgIndice, obras = oQntdObras)
  
}
