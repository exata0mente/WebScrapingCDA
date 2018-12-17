# Autor: Ricardo Bezeraa
# Data: 01/12/2018
# Descrição: Este script (ou conjunto de funções) realiza uma mineração em um site de obra de artes
#            extraindo dados relevantes para enriquecimento de uma base

rm(list=ls()) 
system('docker run -d -p 4445:4444 selenium/standalone-firefox')

# Carregamento de pacotes
library(RSelenium)
library(rvest)
library(httr)
library(dplyr)
#library(jsonlite)
library(stringr)
list.files("../R", full.names = TRUE) %>% 
  as.list() %>% 
  lapply(source)

# Definição de url base
uBase <- "http://www.catalogodasartes.com.br/"
uLogin <-paste0(uBase, "acesso/")

# Etapa de Sessão
navegDriver <- remoteDriver(remoteServerAddr = "localhost", 
                      port = 4445L, 
                      browserName = "firefox")
navegDriver$open()

loginCDA(remDr = navegDriver,
         usuario = usuario,
         senha = senha,
         url_login = uLogin)

navegDriver$navigate(paste0(uBase, "inicio/"))

# Etapa de coleta de dados das obras

expressao <- c("Abraham Palatnik", "Carlito Carvalhosa", "Clovis Graciano", "Di Cavalcante",
               "Dudu Santos", "Ernesto Di Fiori", "Lasar Segall", "Laura Lima", "Manabu Mabe",
               "Maria Bonomi", "Rosangela Renno", "Tito De Alencastro", "Volpi")

for(artista in expressao ){
  #minerarObrasFull(navegDriver, expressao, 1 , 30:90)
  dfTotal <- listaObras2csv(arquivosObras = list.files(gsub(" ", "", artista), full.names = TRUE))
  write.csv2(x = dfTotal, 
           file = paste0(gsub(" ", "", artista), ".csv"),
           fileEncoding = "UTF-8"
           )
}