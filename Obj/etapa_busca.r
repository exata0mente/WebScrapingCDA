library(httr)
library(rvest)

url_acesso <- "https://www.catalogodasartes.com.br/acesso"

sessao <- html_session(url_acesso)

login <- sessao %>%
  html_node("form") %>%
  html_form() %>%
  set_values('cliente_email' = 'profantonioalbuquerque@gmail.com', 'cliente_senha' = 'flamengo')

login$url <- url

sessao_logada <- sessao %>%
  submit_form(login)
