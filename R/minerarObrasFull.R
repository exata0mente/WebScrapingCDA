minerarObrasFull <- function(remDr = NULL, expressao = character(),pagina = NULL, delay = 60:90){
  #' Minera todas as obras conforme expressão de pesquisa
  #' 
  #' A expressão passada por parâmetro é utilizada como texto de pesquisa.
  #' Extraímos as informações de quantidade de obras e páginas e iniciamos
  #' uma mineração página a página salvando o resultado em arquivos RDS.
  #' 
  #' @param remDr objeto remoteDriver
  #' @param expressao texto livre a ser pesquisado
  #' @param pagina numerico caso queira iniciar a mineração de determinada página
  #' @param delay vetor numérico para definir o tempo entre o acesso a cada página
  
  # Etapa de Pesquisa
  # Nesta etapa contamos quantas obras e página determinada expressão
  # retorna.
  
  main <- contarObrasCDA(remDr, expressao)
  
  # Atribuimos os valores coletados à variáveis que serão
  # utilizadas como controle
  
  if(!is.null(pagina)){
    pgAtual <- pagina
    urlTmp <- gsub(
      paste0("/",1,"/"),
      paste0("/",pgAtual,"/"),
      unlist(remDr$getCurrentUrl()))
  }
  else
    pgAtual <- main[[1]][1]
  
  
  pgTotal <- main[[1]][2]
  pgQntObras <- main[[2]]
  
  nomePasta <- gsub(" ", "", expressao)
  
  if(!dir.exists(nomePasta))
    dir.create(nomePasta)
  
  
  # Etapa de Extração
  # Nesta etapa acessaremos página a página para a mineração
  
  for(i in pgAtual:pgTotal){
    urlTmp <- gsub(
      paste0("/",pgAtual,"/"),
      paste0("/",i,"/"),
      unlist(remDr$getCurrentUrl()))
    pgAtual <- i
    
    remDr$navigate(urlTmp) # Navega até a página
    
    pageSource <- remDr$getPageSource()
    pgFonte <- pageSource[[1]] %>%
      read_html()
    
    # Extrai os elementos das obras do código fonte da página
    obrasTmp <- listDfCardsCDA(pgFonte)
    # Salva-o em um arquivo temporario
    saveRDS(obrasTmp, file = paste0(nomePasta, "/obraList", i, ".RDS"))
    # Espera um tempinho para não dar ban :)
    message("Página ", i, " minerada! Entrando em delay programado")
    sample(delay, size = 1, replace = TRUE) %>%
      Sys.sleep()
  }
  # Transformo
  # Carrego
}