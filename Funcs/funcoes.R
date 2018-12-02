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

pesquisaTextoLivre <- function(remDr = NULL, expressao = character()){
#' Pesquisa por assunto
#'
#' Recebe um string com assunto livre e utiliza a caixa de texto "Pesquisa" para
#' trazer os cards relacionados ao assunto. Retorna uma lista com os elementos 'card produto'
#' da primeira página de pesquisa
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
  
  eCardProd <- remDr$findElements(using = "xpath", value = "//div[@class='card produto']")
  
  oQntdObras <- length(eCardProd)
  
  message(paste0("Feito! ", oQntdObras, " produto(s) retornado(s)"))
  eCardProd
}