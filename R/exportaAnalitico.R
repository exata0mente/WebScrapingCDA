exportaAnalitico <- function(expressao = as.character(), formato = c("csv", "xlsx")){
  
  for(artista in expressao ){
    dfTotal <- listaObras2csv(arquivosObras = list.files(gsub(" ", "", artista), full.names = TRUE))
    
    # Exportação CSV
    if(formato == "csv"){
      write.csv2(x = dfTotal, 
                 file = paste0(gsub(" ", "", artista), ".csv"),
                 fileEncoding = "UTF-8"
      )}
      
    
    # Exportaão xlsx
    if(formato == "xlsx"){
      write.xlsx(x = dfTotal
                 ,file = paste0(gsub(" ", "", artista), ".xlsx")
                 ,showNA = FALSE
      )
    }
    
  }
}
