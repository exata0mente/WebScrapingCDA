loginCDA <- function(remDr = NULL,usuario = character(), senha = character(), url_login = character()){
#' Efetua o login no site catálogo das artes
#'
#' Necessário ter uma conexão RSelenium aberta. Recebe dados de acesso (usuário, senha, url)
#' e estabelece conexão através dos métodos do RSelenium.
#' 
#' @param remDr objeto remoteDriver
#' @param usuario string com o usuário de acesso ao site
#' @param senha string com a senha de acesso ao site
#' @param url_login url da página de acesso
#' 
#' @export

  remDr$navigate(url_login)
  
  eFormUsuario <- remDr$findElements(using = "xpath", value = "//input[@id='cliente_email']")
  
  usuario %>% 
    list() %>% 
    eFormUsuario[[1]]$sendKeysToElement() # Envia o usuário
  
  eFormSenha <- remDr$findElements(using = "xpath", value = "//input[@id='cliente_senha']")
  
  senha %>% 
    list() %>% 
    eFormSenha[[1]]$sendKeysToElement() # Envia a senha
  
  remDr$findElement(using = "xpath", value = "//button[@data-action='acesso']")$clickElement() # Clique em enviar
  
}

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
  numeroPaginas <- function(x){ 
    k <- c()
    for (i in 1:length(x)) {
      if(x[i] != "") 
        k <- c(k, x[i])
    }
    k
  }
  
  h5 <-pgFonte %>% 
    html_nodes(xpath = "//h5") %>%
    html_text()
  
  oQntdObras <- as.numeric(gsub( "\\D+", "", h5[1]))
  oPgIndice <- strsplit(h5[2], "\\D") %>%
    lapply(numeroPaginas) %>% 
    unlist()
  
  if(verbose){
    message(paste0("Para o texto ", expressao, 
                 " foram encontradas ", oQntdObras,
                 " obras separadas em ", oPgIndice[2],
                 " páginas"))
    message(paste0("Lembre-se! Cada página retorna 16 elementos sendo então necessários ",
                 oPgIndice[2], " 'cliques'."))
    message(paste0("Página ", oPgIndice[1], " exportada"))
  }
  
  list(paginas = oPgIndice, obras = oQntdObras)
  
}

listDfCardsCDA <- function(pgFonte = NULL){
#' Extrai as obras de uma página html CDA
#'
#' Recebe como argumento o código fonte de uma página CDA
#' possibilitando o uso do pacote rvest para extração dos elementos
#' relacionados aos cards de produto. Retorna uma lista de data frames
#' com os itens coletados da página (geralmente 16 obras)
#' 
#' @param pgFonte objeto xml com código fonte da página
#' 
#' @export
  
  # Carregamento do cabecalho do último data frame salvo. Este procedimento é necessário
  # devido a uma quantidade de títulos não fixa. Exemplo: há obras com o título Século, Ano, Técnica
  # e outras não.
  
  # Extração dos elementos com a estrutura de dados dos produtos
  eCardProduto <- pgFonte %>% 
    html_nodes(xpath = "//*[@class='card produto']")
  
  # Extração dos elementos com os Ids das obras (que não estão aninhados à estrutura dos produtos)
  eIdObras <- pgFonte %>%
   html_nodes(xpath = "//*[@data-id_obra]") %>%
   html_attr("data-id_obra")
  
  # Extração dos elementos com as urls das obras
  eUrlObras <- eCardProduto %>% 
    html_node(xpath = "div/a") %>% 
    html_attr("href")
   
  eIdObras <- eIdObras[eIdObras != ""] # Tratamento de valores inválidos
  
  # Extraí para uma lista a estrutura dos produtos contidos nos elementos de cards
  # aqui temos um vetor onde os índices ímpares são títulos e os pares são conteúdos
  
  eObras <- lapply(eCardProduto, function(x){x %>% 
      html_nodes(xpath = "div[2]/a/span[@class='tipo']/span[2]|
                 div[@class='card-reveal']/p/span/span")
    } %>% html_text())
  
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
  
  mapply(cardToDf, eObras,as.list(eUrlObras)) %>% setNames(eIdObras)
  
}

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
    saveRDS(obrasTmp, file = paste0("obraList", i, ".RDS"))
    # Espera um tempinho para não dar ban :)
    sample(delay, size = 1, replace = TRUE) %>%
      Sys.sleep()
  }
  # Transformo
  # Carrego
}

mesclaObrasDf <- function(x){
  cabecalhoPadrao <- readRDS(file = "Cabecalhos.RDS")
  df_principal <- data.frame(matrix(nrow = 0, ncol = length(cabecalhoPadrao)))
  names(df_principal) <- cabecalhoPadrao
  tam <- length(x)
  cabecalhoTmp <- names(x)
  
  for(i in 1:tam){
    if(!any(cabecalhoTmp[i] == cabecalhoPadrao)){
      cabecalhoPadrao <- combine(cabecalhoPadrao,cabecalhoTmp[i])
      df_principal <- mutate(df_principal, nc = "N/D")
      names(df_principal) <- cabecalhoPadrao
    }
    pos <- which(cabecalhoPadrao == cabecalhoTmp[i])
    df_principal[1,pos] <- x[i]
  }
  saveRDS(cabecalhoPadrao, file = "Cabecalhos.RDS")
  df_principal
}

listaObras2csv <- function(arquivosObras = list.files()){
  cab <- readRDS("Cabecalhos.RDS")
  tam <- length(cab)
  obrasTotal <- data.frame(matrix(nrow = 0, ncol = tam))
  
  for(i in 1:length(arquivosObras)){
    obras <- readRDS(file = arquivosObras[i]) %>% # Lista com os data.frames das obras
      lapply(mesclaObrasDf) %>%
      do.call(what = rbind)
    obrasTotal <- rbind(obrasTotal,obras)
  }
  obrasTotal
}
