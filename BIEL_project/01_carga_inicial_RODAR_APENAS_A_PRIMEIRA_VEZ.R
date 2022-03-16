
library(bigrquery)
library(rvest)
library(tidyverse)
library(janitor)
library(xml2)
library(stringr)

################################################################################
#### captura os parâmetros para compor as URLs de acesso aos repositórios dos editais
#### respositórios contidos nas primeiras 10 páginas
options(warn = -1)

link_base <- 'https://sistemas4.sc.gov.br/sea/portaldecompras/acompanha_licitacao_edital.asp?lstOrgaos=1&optNatureza=&lstAno=2022&lstSituacao=4&txtNuEdital=&lstModalidade=0&txtObjeto=&dataInclusao=&descricaoGrupoClasse=&paginaAtual='
i <- 1
link <- paste0(link_base,i)

base_parametros_final <- data.frame()
while (length(try(busca_parametros <- as.data.frame(read_html(link) %>% html_nodes("img") %>% html_attr("onclick"))) >= 1)) {
  print(paste0('Capturando Parâmetros da Página ', i))
  #try(busca_parametros <- as.data.frame(read_html(link) %>% html_nodes("img") %>% html_attr("onclick")))
  base_parametros_final <- bind_rows(base_parametros_final, busca_parametros)
  #Sys.sleep(time = 15)
  i <- i + 1
  link <- paste0(link_base,i)
}

base_parametros_final <- base_parametros_final %>% distinct_all()
print(paste0('Total de Editais encontrados: ', nrow(base_parametros_final)))

rm(busca_parametros)
rm(link)
rm(link_base)
rm(num_pags)

################################################################################
#### trata a base de parâmetros capturados na rotina anterior

library("splitstackshape")
colnames(base_parametros_final) <- 'parametros'
base_parametros_final <- cSplit(base_parametros_final, "parametros", sep="'")
base_parametros_final <- base_parametros_final[,c(2,4,6,8)]
base_parametros_final <- base_parametros_final %>% rename("portal" = "parametros_2",
                                                          "processo" = "parametros_4",
                                                          "edital" = "parametros_6",
                                                          "cdo" = "parametros_8")
base_parametros_final$processo[is.na(base_parametros_final$processo)] <- ''

################################################################################
#### monta as URLs dos repositórios baseado nos parâmetros

base <- 'https://sistemas4.sc.gov.br/sea/portaldecompras/docs.asp?'
portal <- 'portal='
processo <- '&processo='
cdo <- '&cdo='
edital <- '&edital='

base_links_direto <- data.frame()
for (i in 1:nrow(base_parametros_final)) {
  print(paste0('Montando Link ', i, ' de ', nrow(base_parametros_final)))
  link_repositorio_edital <- paste0(base,
                                    portal, base_parametros_final$portal[i],
                                    processo, base_parametros_final$processo[i],
                                    cdo, base_parametros_final$cdo[i],
                                    edital, base_parametros_final$edital[i])
  busca_links <- as.data.frame(link_repositorio_edital)
  base_links_direto <- bind_rows(base_links_direto, busca_links)
}

base_links_direto <- base_links_direto %>% distinct_all()
base_links_direto <- base_links_direto %>% arrange(link_repositorio_edital)

rm(base)
rm(portal)
rm(processo)
rm(cdo)
rm(edital)
rm(busca_links)
rm(link_repositorio_edital)

################################################################################
#### Junta a base de parâmetros com a base dos links dos respositórios montados

base_final <- bind_cols(base_parametros_final,base_links_direto)

rm(base_parametros_final)
rm(base_links_direto)

################################################################################
#### captura parte final dos links dos arquivos dos editais

links_documentos_final <- data.frame()
for (i in 1:nrow(base_final)) {
  print(paste0('Raspando link ', i, ' de ', nrow(base_final)))
  try(lista <- (read_html(base_final$link_repositorio_edital[i]) %>% html_nodes('a') %>% html_attr('href')))
  link_documento <- as.data.frame(lista[1])
  link_documento$portal <- base_final$portal[i]
  link_documento$processo <- base_final$processo[i]
  link_documento$edital <- base_final$edital[i]
  link_documento$cdo <- base_final$cdo[i]
  link_documento$link_repositorio_edital <- base_final$link_repositorio_edital[i]
  links_documentos_final <- bind_rows(links_documentos_final, link_documento)
  #Sys.sleep(time = 15)
}

links_documentos_final <- links_documentos_final %>% rename('href_link' = colnames(links_documentos_final)[1])

################################################################################
#### cria a coluna link_completo_para_editais
#### a partir da junção da base da url + href_link
#### atualiza a base_final com o dataframe completo criado na rotina anterior

links_documentos_final$link_completo_para_editais <- paste0('https://sistemas4.sc.gov.br/sea/portaldecompras/',links_documentos_final$href_link)

links_documentos_final <- links_documentos_final %>% distinct()
base_final <- links_documentos_final
base_final <- base_final[c(2,3,4,5,1,6,7)]

rm(link_documento)
#rm(links_documentos_final)

################################################################################
#### acessa cada link de arquivo dos editais e salva o conteudo dos editais
### nos Dataframes - SEM TRATAMENTO DAS INFORMAÇÕES

base_final$conteudo_edital <- NA
for (i in 1:nrow(base_final)) {
  print(paste0('Inserindo na tabela o conteúdo do documento: ',i, ' de ',nrow(base_final)))
  if (str_detect(string = try(html_text(read_html(base_final$link_completo_para_editais[i]))), pattern = 'PDF')) {
    print('Documento em PDF')
    base_final$conteudo_edital[i] <- str_flatten(pdftools::pdf_text(pdf = base_final$link_completo_para_editais[i]))
  }else{
    print('Formato de documento desconhecido')
    base_final$conteudo_edital[i] <- 'Não foi possível buscar o conteúdo deste Edital'
  }
}

#### carrega dataset com as palavras chaves








# #########################
# #### rotina para fazer download dos arquivos
# 
# for (i in 1:nrow(base_final)) {
#   download.file(url = base_final$link_completo_para_editais[i],
#                 destfile = paste0('/home/pira/google_drive/ciencia_de_dados/R/DOU_SC/arquivos_download/',
#                                   str_replace(string = base_final$edital[i],pattern = "/",replacement = "_"),
#                                   base_final$processo[i])
#   )
# }
# 
# ou
#  
# for (i in 1:nrow(base_final)) { 
#   xml2::download_html(url = base_final$link_completo_para_editais[i],
#                       file =  paste0('/home/pira/google_drive/ciencia_de_dados/R/DOU_SC/arquivos_download/',
#                                      str_replace(string = base_final$edital[i],pattern = "/",replacement = "_"),
#                                      '_',
#                                      base_final$processo[i],
#                                      '.pdf')
#   )
# }
######## Upload file to Big Query #############

#devtools::install_github("r-dbi/bigrquery")

httr::set_config(httr::config(ssl_verifypeer = 0L))
httr::set_config(httr::config(http_version = 0))
options(httr_oob_default = TRUE)

bq_auth(path = "/home/pira/google_drive/Estudo/json/dou-sc.json")

billing <- "dou-sc"

library(DBI)

con <- dbConnect(
  bigrquery::bigquery(),
  project = "auspicious-crow-344115",
  dataset = "projeto_dou_sc",
  billing = billing
)
con

if(nrow(base_final)>0){
  tablename <- "tabela_dou_sc"
  
  base_final <- base_final %>% mutate_all(as.character)
  
  bigrquery::dbWriteTable(con, tablename, base_final, append=F, verbose = T, row.names=F, overwrite=T, fields=base_final)
  print("Dataframe Ocorrencias Uploaded")
}

if(nrow(itens)>0){
  tablename <- "tabela_itens"
  
  base_final <- base_final %>% mutate_all(as.character)
  
  bigrquery::dbWriteTable(con, tablename, itens, append=F, verbose = T, row.names=F, overwrite=T, fields=itens)
  print("Dataframe Ocorrencias Uploaded")
}

