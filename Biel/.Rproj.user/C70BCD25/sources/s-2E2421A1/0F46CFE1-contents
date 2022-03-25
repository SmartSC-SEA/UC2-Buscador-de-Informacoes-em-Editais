
source(file = '01_instalacao_de_pacotes.R', encoding = 'utf-8')

######################### LISTA DE ÓRGÃOS

source(file = '02_lista_orgaos_e_anos.R', encoding = 'utf-8')

#########################

link_base <- paste0('https://sistemas4.sc.gov.br/sea/portaldecompras/acompanha_licitacao_edital.asp?lstOrgaos=1&optNatureza=&lstAno=',ano,'&lstSituacao=4&txtNuEdital=&lstModalidade=0&txtObjeto=&dataInclusao=&descricaoGrupoClasse=&paginaAtual=')

#num_pags  <- 1:10
i <- 1
link <- paste0(link_base,str(i))
base_final <- data.frame()
while (length(try(page <- read_html(link_base) %>% html_table())) >= 1) {
  print(paste0('Raspando ano: ', ano))
  print(paste0('Link: ', link_base))
  tabela <- as.data.frame(page[2])
  base_final <- bind_rows(base_final,
                          tabela)
  i <- i + 1
  link <- paste0(link_base,str(i))
}

colnames(base_final) <- base_final[1,]
base_final <- base_final[-1,]
base_final <- clean_names(base_final) 
base_final <- base_final %>% arrange(processo)

#########################





