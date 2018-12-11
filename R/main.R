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
library(jsonlite)
source("../R/funcoesCDA.R")

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

minerarObrasFull(navegDriver, "Abraham Palatnik", 1 , 50:90)



