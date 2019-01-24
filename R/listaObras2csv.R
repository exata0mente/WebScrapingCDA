listaObras2csv <- function(arquivosObras = list.files(full.names = TRUE), trataColunaAglut = TRUE){
  cab <- readRDS("Cabecalhos.RDS")
  tam <- length(cab)
  obrasTotal <- data.frame(matrix(nrow = 0, ncol = tam))
  
  for(i in 1:length(arquivosObras)){
    obras <- readRDS(file = arquivosObras[i]) %>% # Lista com os data.frames das obras
      lapply(mesclaObrasDf) %>%
      do.call(what = rbind)
    obrasTotal <- rbind(obrasTotal,obras)
  }
  if(trataColunaAglut){
    ## Separação de colunas aglutinadas
    ## Preço/m2
    precosSplit <- obrasTotal$`Preço/m²` %>% 
      strsplit(split = "[|]") %>%
      lapply(unlist) %>%
      do.call(what = rbind) %>%
      gsub(pattern = "[^0-9,.]", replacement = "")
    
    ## Limpa pontuação para facilitar integração
    precosSplit[,1] <- precosSplit[,1] %>% 
      gsub(pattern = "[.]", replacement = "")
    
    ## Limpa pontuação para facilitar integração
    precosSplit[,2] <- precosSplit[,2] %>% 
      gsub(pattern = ",", replacement = "") %>% 
      gsub(pattern = "[.]", replacement = ",")
    
    avaliacaoSplit <- obrasTotal$`Avaliação da Obra` %>%
      strsplit(split = "[:|]") %>%
      lapply(unlist) %>%
      do.call(what = rbind)
  
    avaliacaoSplit[,2:3] <- avaliacaoSplit[,2:3] %>%
      gsub(pattern = "[^0-9,.]", replacement = "")
    
    ## Limpa pontuação para facilitar integração
    avaliacaoSplit[,2] <- avaliacaoSplit[,2] %>%
      gsub(pattern = "[.]", replacement = "")
    
    ## Limpa pontuação para facilitar integração
    avaliacaoSplit[,3] <- avaliacaoSplit[,3] %>%
      gsub(pattern = ",", replacement = "") %>%
      gsub(pattern = "[.]", replacement = ",")
    
    # Limpa lance inicial
    obrasTotal <- obrasTotal %>%
      mutate(`Lance Inicial` = Descrição)
    
    # Identifica as obras que tenham a string "lance inicial"
    indice <- obrasTotal$Descrição %>%
      str_detect(pattern = "inicial")
    
    # Etapas de limpeza
    obrasTotal$`Lance Inicial`[!indice] <- 0              # Os que não possuem a string, recebem o valor 0
    obrasTotal$`Lance Inicial`[is.na(indice)] <- 0        # NA também recebem o valor 0
    obrasTotal$`Lance Inicial` <- obrasTotal$`Lance Inicial` %>%
      gsub(pattern = "(.*)*.inicial", replacement =  "") %>% # Limpa tudo que estiver antes da string
      gsub(pattern = "[^0-9,.]", replacement = "") %>%       # Deixa apenas numeros, vírgula e ponto
      gsub(pattern = "(,00).*", replacement = "\\1")         # Limpa tudo que estiver depois da casa decimal (como ponto final)
  
    obrasTotal <- obrasTotal %>%
      select(-c("Preço/m²", "Avaliação da Obra")) %>%
      mutate("Preço/m2(BRL)" = precosSplit[,1],
             "Preço/m2(USD)" = precosSplit[,2],
             "Status Lote" = avaliacaoSplit[,1],
             "Valor(BRL)" = avaliacaoSplit[,2],
             "Valor(USD)" = avaliacaoSplit[,3]
      )      
  }
  
  obrasTotal
  
}
