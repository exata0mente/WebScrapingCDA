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
    
    avaliacaoSplit <- obrasTotal$`Avaliação da Obra` %>%
      strsplit(split = "[:|]") %>%
      lapply(unlist) %>%
      do.call(what = rbind)
    
    avaliacaoSplit[,2:3] <- avaliacaoSplit[,2:3] %>%
      gsub(pattern = "[^0-9,.]", replacement = "")
    
    lanceSplit <- obrasTotal$Descrição %>%
      strsplit(split = " ") %>%
      lapply(function(x){x[length(x)]}) %>%
      do.call(what = rbind) %>%
      gsub(pattern = "[^0-9,.]", replacement = "")
    
    obrasTotal <- obrasTotal %>%
      select(-c("Preço/m²", "Avaliação da Obra")) %>%
      mutate("Lance Inicial" = lanceSplit,
             "Preço/m2(BRL)" = precosSplit[,2],
             "Preço/m2(USD)" = precosSplit[,2],
             "Status Lote" = avaliacaoSplit[,1],
             "Valor(BRL)" = avaliacaoSplit[,2],
             "Valor(USD)" = avaliacaoSplit[,3]
      )      
  }
  
  obrasTotal
  
}
