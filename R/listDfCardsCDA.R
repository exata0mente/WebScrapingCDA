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
  #' @import utils
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
  
  mapply(cardToDf, eObras,as.list(eUrlObras)) %>% setNames(eIdObras)
  
}