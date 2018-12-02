# Autor: Ricardo Bezeraa
# Data: 01/12/2018
# Descrição: Este script (ou conjunto de funções) realiza uma mineração em um site de obra de artes
#            extraindo dados relevantes para enriquecimento de uma base

# Mapa de prefixos dos objetos:
# pg* - Página
# s*  - Sessão
# o*  - Objeto
# u*  - Url
# e*  - Elementos

########## APAGAR ##########
oUsuario <- "profantonioalbuquerque@gmail.com"
oSenha <- "flamengo"

rm(list=ls())
system('docker run -d -p 4445:4444 selenium/standalone-firefox')

# Carregamento de pacotes
library(RSelenium)
library(dplyr)
source("funcoes.R")

# Definição de url base
uBase <- "http://www.catalogodasartes.com.br/"
uLogin <-paste0(uBase, "acesso/")

# Etapa de Sessão
remDr <- remoteDriver(remoteServerAddr = "localhost", 
                      port = 4445L, 
                      browserName = "firefox")
remDr$open()

loginCDA(remDr = remDr,
         usuario = oUsuario,
         senha = oSenha,
         url_login = uLogin)


# Etapa de coleta de dados das obras

oCabecalho <- c("Tipo",
                 "Artista",
                 "Título",
                 "Técnica",
                 "Dimensões",
                 "Descrição",
                 "Avaliação da Obra",
                 "Data da Pesquisa")

dfObras <- data.frame(matrix(nrow = 0, ncol = length(oCabecalho)))

#usar o lapply

