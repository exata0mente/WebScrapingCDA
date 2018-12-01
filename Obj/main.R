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

# Carregamento de pacotes
library(RSelenium)
library(dplyr)

# Definição de url base
uBase <- "http://www.catalogodasartes.com.br/"

# Etapa de Sessão
remDr <- remoteDriver(remoteServerAddr = "localhost", 
                      port = 4445L, 
                      browserName = "firefox")
remDr$open()

# Etapa de Login (os dados úteis estão apenas disponíveis para usuários logados)

uLogin <-paste0(uBase, "acesso/")
remDr$navigate(uLogin)

eFormUsuario <- remDr$findElements(using = "xpath", value = "//input[@id='cliente_email']")

usuario %>% 
  list() %>% 
  eFormUsuario[[1]]$sendKeysToElement() # Envia o usuário


eFormSenha <- remDr$findElements(using = "xpath", value = "//input[@id='cliente_senha']")

senha %>% 
  list() %>% 
  eFormSenha[[1]]$sendKeysToElement() # Envia a senha

remDr$findElement(using = "xpath", value = "//button[@type='button']")$clickElement() # Clique em enviar



