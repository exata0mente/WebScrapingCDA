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

pesquisaTextoLivreCDA <- function(remDr = NULL, expressao = character()){
#' Pesquisa por assunto
#'
#' Recebe um string com assunto livre e utiliza a caixa de texto "Pesquisa" para
#' trazer os cards relacionados ao assunto. Retorna o código fonte da página em
#' que a pesquisa foi realizada.
#' 
#' @param remDr objeto remoteDriver
#' @param expressao texto livre a ser pesquisado
#' 
#' @export

  eFormPesquisa <- remDr$findElement(using = "xpath", value = "//input[@name='pesquisa']")
  
  # Efetua clique no botão "Pesquisar"
  expressao %>%
    list() %>%
    eFormPesquisa$sendKeysToElement()
  
  remDr$findElement(using = "xpath", value = "//button[@data-action='pesquisar']")$clickElement()
  
  pageSource <- remDr$getPageSource()
  
  pgFonte <- pageSource[[1]] %>%
    read_html()
  
  oQntdObras <- pgFonte %>%
    html_nodes(xpath = "//*[@class='card produto']") %>%
    length()
  
    
  message(paste0("Feito! ", oQntdObras, " produto(s) retornado(s)"))
  
  # Volta para página inicial para que esta função possa ser chamada novamente
  remDr$navigate(paste0(uBase, "inicio/")) 
  
  pgFonte
  
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
  oCabecalho <- readRDS("Cabecalhos.RDS")
  
  # Inicialização de um data.frame
  dfObras <- data.frame(matrix(nrow = 0, ncol = length(oCabecalho)))
  names(dfObras) <- oCabecalho
  
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
  
  mapply(cardToDf, eObras,as.list(eUrlObras))
  
}
