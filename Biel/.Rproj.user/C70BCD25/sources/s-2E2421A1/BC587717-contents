

#devtools::install_github("r-dbi/bigrquery")

httr::set_config(httr::config(ssl_verifypeer = 0L))
httr::set_config(httr::config(http_version = 0))
options(httr_oob_default = TRUE)

library(bigrquery)

bq_auth(email = 'tenorioabs@gmail.com', path = "client_secret_713400840481-in4ekt8v479m5mnk7g4979amabp5h5tr.apps.googleusercontent.com.json")

billing <- "auspicious-crow-344115"

library(DBI)

con <- dbConnect(
  bigrquery::bigquery(),
  project = "auspicious-crow-344115",
  dataset = "projeto_dou_sc",
  billing = billing
)
con

project = "auspicious-crow-344115"

## Extração

sql <- "SELECT * FROM auspicious-crow-344115.projeto_dou_sc.tabela_dou_sc"
print(sql)

ocorrencias_total <- dbGetQuery(con, sql)

