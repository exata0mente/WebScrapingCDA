loginCDA <- function(remDr = NULL,usuario = character(), senha = character(), url_login = character()){
#' Efetua o login no site catálogo das artes
#
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
#' trazer os cards relacionados ao assunto. Retorna uma lista com os elementos 'card produto'
#' da primeira página de pesquisa além de métodos para ir as próximas, e anteriores, páginas 
#' 
#' @param remDr objeto remoteDriver
#' @param expressao texto livre a ser pesquisado
#' 
#' @export

  eFormPesquisa <- remDr$findElement(using = "xpath", value = "//input[@name='pesquisa']")
  
  expressao %>%
    list() %>%
    eFormPesquisa$sendKeysToElement()
  
  remDr$findElement(using = "xpath", value = "//button[@data-action='pesquisar']")$clickElement()
  
  #eCardProd <- remDr$findElements(using = "xpath", value = "//div[@class='card produto']")
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

cardParaDataFrameCDA <- function(pgFonte = NULL){
#' Extrai as obras de uma página html CDA
#'
#' Recebe como argumento o código fonte de uma página CDA
#' possibilitando o uso do pacote rvest para extração dos elementos
#' relacionados aos cards de produto. Retorna um data frame com
#' os itens coletados da página (geralmente 16 obras)
#' 
#' @param pgFonte objeto xml com código fonte da página
#' 
#' @export
  
  # Carregamento do cabecalho do último data frame salvo. Este procedimento é necessário
  # devido a uma quantidade de títulos não fixa. Exemplo: há obras com o título Século, Ano, Técnica
  # e outras não.
  oCabecalho <- readRDS("Cabecalhos.RDS")
  
  dfObras <- data.frame(matrix(nrow = 0, ncol = length(oCabecalho)))
  names(dfObras) <- oCabecalho
  
  eCardProduto <- pgFonte %>% 
    html_nodes(xpath = "//*[@class='card produto']")
  
  eIdObras <- pgFonte %>%
    html_nodes(xpath = "//*[@data-id_obra]") %>%
    html_attr("data-id_obra")
  
  # Retira um elemento a mais que aparece no código fonte, porém com valor vazio
  eIdObras <- eIdObras[eIdObras != ""]
  
  
  function(x){
    
  }
  
}