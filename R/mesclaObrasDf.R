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