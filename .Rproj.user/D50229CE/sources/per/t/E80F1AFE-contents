# "BIEL - Robô Buscador de Informações em Editais de Licitação"

print(paste('Data e hora do início do script:',format(Sys.time(), "%d/%m/%Y"), 'às',format(Sys.time(), "%H:%M:%S")))

options(warn = -1)

retry <- function(a, max = Inf, init = 0){
  suppressWarnings(tryCatch({
    if(init<max) a
  }, error = function(e){retry(a, max, init = init+1)}))}

## Instalação e carregamento dos pacotes necessários

# Carregamento dos pacotes necessários ao funcionamento do script.
# Caso algum dos pacotes não esteja instalado, a rotina abaixo fará a instalação
# e o carregamento automaticamente.

pacotes <- c("bigrquery",
             "rvest",
             "tidyverse",
             "janitor",
             "xml2",
             "splitstackshape",
             "pdftools",
             "docxtractr",
             "textclean",
             "readxl",
             "plyr")

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}

rm(pacotes)

## Raspagem 1 

# A rotina abaixo irá acesar o código html do site onde estão hospedados os editais,
# retornando duas bases: a lista com os nomes e códigos dos órgãos do governo do
# Estado de Santa Catarina e a lista dos anos disponíveis para consulta.

url <- 'https://sistemas4.sc.gov.br/sea/portaldecompras/pesquisa_editais_p.asp?cdo=**CDO**'

lista_orgao <- read_html(url) %>% html_nodes('option')
orgao_cod <- lista_orgao %>% html_attr('value')
orgao_nome <- lista_orgao %>% html_text()

df_orgaos <- data.frame(orgao_cod, orgao_nome)
df_orgaos <- df_orgaos[3:75,]

lista_anos <- read_html(url) %>% html_nodes('select')
anos <- lista_anos[2]
anos <- anos %>% html_nodes('option') %>% html_text()
df_anos <- data.frame(anos)

df_anos <- df_anos[17,]

rm(lista_orgao)
rm(orgao_cod)
rm(orgao_nome)
rm(lista_anos)
rm(anos)
rm(url)

## Raspagem 2

# Com base na composição programática de uma lista de URLs, o script irá raspar o
# código html e irá capturar os parâmetros que, na sequência, mediante nova composição
# das URLs, irá acessar as páginas específicas de cada edital.
# Nesta mesma rotina, haverá como saída uma base de dados contento informações
# específicas dos editais, como datas, órgãos e status.

tabela_url <- data.frame()
base_parametros_final <- data.frame()

for (a in 1:length(df_anos)) {
  ano <- df_anos[a]
  i <- 1
  link_base <- paste0('https://sistemas4.sc.gov.br/sea/portaldecompras/acompanha_licitacao_edital.asp?lstOrgaos=1&optNatureza=&lstAno=',ano,'&lstSituacao=4&txtNuEdital=&lstModalidade=0&txtObjeto=&dataInclusao=&descricaoGrupoClasse=&paginaAtual=')
  link <- paste0(link_base,i)
  time_limit <- 1
  setTimeLimit(cpu = time_limit, elapsed = time_limit, transient = TRUE)
  on.exit({setTimeLimit(cpu = Inf, elapsed = Inf, transient = FALSE)})
  while (
    
    tryCatch({
      length(retry(busca_parametros <- as.data.frame(read_html(link) %>% html_nodes("img") %>% html_attr("onclick"))) >= 1)
    }, error = function(e) {
      if (grepl("reached elapsed time limit|reached CPU time limit", e$message)) {
        print('Chamando função secundária - Tempo limite atingido')
        print('Dormindo 2 segundos')
        Sys.sleep(2)
      }
    })
    
  )
    {
    #### captura parametros
    print(paste0('Capturando Parâmetros da Página ', i))
    base_parametros_final <- bind_rows(base_parametros_final, busca_parametros)
    #### captura tabelas de url
    retry(page <- read_html(link) %>% html_table()) 
    print(paste0('Raspando ano: ', ano))
    print(paste0('Link: ', link))
    tabela <- as.data.frame(page[2])
    tabela_url <- bind_rows(tabela_url, tabela)
    i <- i + 1
    link <- paste0(link_base,i)
  } 
}

## Tratamento de dados

# As bases, resultado da rotina acima, terão seus valores convertidos para caracteres
# e a função distinct() removerá as linhas repetidas, se houverem.
# Ao final será impresso na tela o número de editais encontrados.

base_parametros_final <- base_parametros_final %>% mutate_all(as.character)
tabela_url <- tabela_url %>% mutate_all(as.character)

base_parametros_final <- base_parametros_final %>% distinct()
tabela_url <- tabela_url %>% distinct()

print(paste0('Total de Editais encontrados: ', nrow(base_parametros_final)))

rm(busca_parametros)
rm(link)
rm(link_base)
rm(num_pags)
rm(page)
rm(tabela)
rm(a)
rm(i)
rm(df_anos)
rm(ano)

## Tratamento de dados

# A base de parâmetros é composta por uma série de quatro informações: "portal",
# "processo", "edital" e "cdo". Como resultado da busca, as quatro informações
# são capturadas dentro da mesma string. A rotina abaixo irá será separar cada uma
# delas em uma coluna correspondente.
# Para a base de tabela com as informações dos editais é feito um tratemento no
# nome das colunas.

colnames(base_parametros_final) <- 'parametros'
base_parametros_final <- cSplit(base_parametros_final, "parametros", sep="'")
base_parametros_final <- base_parametros_final[,c(2,4,6,8)]
colnames(base_parametros_final) <- c("portal",
                                   "processo",
                                   "edital",
                                   "cdo")

base_parametros_final$processo[is.na(base_parametros_final$processo)] <- ''
base_parametros_final <- base_parametros_final %>% arrange(edital)

colnames(tabela_url) <- tabela_url[1,]
tabela_url <- tabela_url[-1,]
tabela_url <- clean_names(tabela_url) 
tabela_url <- tabela_url %>% arrange(processo)
lista_colunas <- colnames(tabela_url)
lista_colunas[1] <- "edital"
lista_colunas[2] <- "orgao_sigla"
colnames(tabela_url) <- lista_colunas
rm(lista_colunas)

## Tratamento de dados

# Cria, via REGEX, na base de parâmetros, a coluna com o nomde dos órgaos.

base_parametros_final$orgao <- case_when(
  str_detect(string = base_parametros_final$cdo, pattern = '\\b2300\\b')==TRUE ~ 'Agência de Desenvolvimento do Turismo - SANTUR',
  str_detect(string = base_parametros_final$cdo, pattern = '\\b302\\b')==TRUE ~ 'Agência de Fomento do Estado de Santa Catarina - BADESC',
  str_detect(string = base_parametros_final$cdo, pattern = '\\b2729\\b')==TRUE ~ 'Agência de Regulação de Serviços Públicos de Santa Catarina - ARESC',
  str_detect(string = base_parametros_final$cdo, pattern = '\\b4101\\b')==TRUE ~ 'Casa Civil - CC',
  str_detect(string = base_parametros_final$cdo, pattern = '\\b301\\b')==TRUE ~ 'Centro de Informática e Automação de Santa Catarina - CIASC',
  str_detect(string = base_parametros_final$cdo, pattern = '\\b2622\\b')==TRUE ~ 'Companhia de Habitação do Estado de Santa Catarina S/A - COHAB/SC',
  str_detect(string = base_parametros_final$cdo, pattern = '\\b4422\\b')==TRUE ~ 'Companhia Integrada de Desenv. Agricola de Santa Catarina - CIDASC',
  str_detect(string = base_parametros_final$cdo, pattern = '\\b4107\\b')==TRUE ~ 'Controladoria Geral do Estado',
  str_detect(string = base_parametros_final$cdo, pattern = '\\b1685\\b')==TRUE ~ 'Corpo de Bombeiros Militar - CBM/SC - Fundo de Melhoria',
  str_detect(string = base_parametros_final$cdo, pattern = '\\b1501\\b')==TRUE ~ 'Defensoria Pública do Estado de Santa Catarina - DPE',
  str_detect(string = base_parametros_final$cdo, pattern = '\\b5501\\b')==TRUE ~ 'Defesa Cívil - DC',
  str_detect(string = base_parametros_final$cdo, pattern = '\\b5325\\b')==TRUE ~ 'Departamento Estadual de Infraestrutura - DEINFRA',
  str_detect(string = base_parametros_final$cdo, pattern = '\\b4112\\b')==TRUE ~ 'Departamento Estadual de Trânsito - DETRAN/SC',
  str_detect(string = base_parametros_final$cdo, pattern = '\\b9\\b')==TRUE ~ 'Empresa de Pesquisa Agropecuária e Extensão Rural de Santa Catarina - EPAGRI',
  str_detect(string = base_parametros_final$cdo, pattern = '\\b2322\\b')==TRUE ~ 'Fundação Catarinense de Cultura - FCC',
  str_detect(string = base_parametros_final$cdo, pattern = '\\b4521\\b')==TRUE ~ 'Fundação Catarinense de Educação Especial - FCEE',
  str_detect(string = base_parametros_final$cdo, pattern = '\\b2006\\b')==TRUE ~ 'Fundação Catarinense de Esporte - FCE',
  str_detect(string = base_parametros_final$cdo, pattern = '\\b4524\\b')==TRUE ~ 'Fundação de Amparo á Pesquisa  e Inovação do Estado de Santa Catarina - FAPESC',
  str_detect(string = base_parametros_final$cdo, pattern = '\\b200\\b')==TRUE ~ 'Fundação de Previdência Complementar do Estado de Santa Catarina - SCPREV',
  str_detect(string = base_parametros_final$cdo, pattern = '\\b5230\\b')==TRUE ~ 'Fundação Escola de Governo - ENA',
  str_detect(string = base_parametros_final$cdo, pattern = '\\b4522\\b')==TRUE ~ 'Fundação Universidade do Estado de Santa Catarina - UDESC',
  str_detect(string = base_parametros_final$cdo, pattern = '\\b4201\\b')==TRUE ~ 'Gabinete do Vice-Governador - GVG',
  str_detect(string = base_parametros_final$cdo, pattern = '\\b1504\\b')==TRUE ~ 'Imprensa Oficial do Estado de Santa Catarina - IOESC',
  str_detect(string = base_parametros_final$cdo, pattern = '\\b4722\\b')==TRUE ~ 'Instituto de Previdência do Estado de Santa Catarina - IPREV',
  str_detect(string = base_parametros_final$cdo, pattern = '\\b2721\\b')==TRUE ~ 'Instituto do Meio Ambiente - IMA',
  str_detect(string = base_parametros_final$cdo, pattern = '\\b5222\\b')==TRUE ~ 'Junta Comercial do Estado de Santa Catarina - JUCESC',
  str_detect(string = base_parametros_final$cdo, pattern = '\\b4301\\b')==TRUE ~ 'Ministério Público de Contas do Estado de Santa Catarina - MPC',
  str_detect(string = base_parametros_final$cdo, pattern = '\\b4001\\b')==TRUE ~ 'Ministério Público do Estado de Santa Catarina Procuradoria Geral de Justiça - MPSC',
  str_detect(string = base_parametros_final$cdo, pattern = '\\b1699\\b')==TRUE ~ 'Polícia Científica de Santa Catarina',
  str_detect(string = base_parametros_final$cdo, pattern = '\\b1605\\b')==TRUE ~ 'Polícia Civil - PC',
  str_detect(string = base_parametros_final$cdo, pattern = '\\b1606\\b')==TRUE ~ 'Polícia Militar do Estado de Santa Catarina - PM/SC',
  str_detect(string = base_parametros_final$cdo, pattern = '\\b201\\b')==TRUE ~ 'Procuradoria Geral do Estado de Santa Catarina - PGE',
  str_detect(string = base_parametros_final$cdo, pattern = '\\b111\\b')==TRUE ~ 'Santa Catarina Participação e Investimentos S.A.',
  str_detect(string = base_parametros_final$cdo, pattern = '\\b2323\\b')==TRUE ~ 'Santa Catarina Turismo S.A. - SCTUR',
  str_detect(string = base_parametros_final$cdo, pattern = '\\b1823\\b')==TRUE ~ 'SC Participações e Parcerias S.A. - SCPar',
  str_detect(string = base_parametros_final$cdo, pattern = '\\b4126\\b')==TRUE ~ 'SCPAR Porto de Imbituba S/A',
  str_detect(string = base_parametros_final$cdo, pattern = '\\b4130\\b')==TRUE ~ 'PSFS Porto de São Francisco do Sul S/A',
  str_detect(string = base_parametros_final$cdo, pattern = '\\b1700\\b')==TRUE ~ 'Secretaria de Estado da Administração - SEA',
  str_detect(string = base_parametros_final$cdo, pattern = '\\b801\\b')==TRUE ~ 'Secretaria de Estado da Administração Prisional e Socioeducativa - SAP',
  str_detect(string = base_parametros_final$cdo, pattern = '\\b4401\\b')==TRUE ~ 'Secretaria de Estado da Agricultura e da Pesca - SAR',
  str_detect(string = base_parametros_final$cdo, pattern = '\\b4501\\b')==TRUE ~ 'Secretaria de Estado da Educação - SED',
  str_detect(string = base_parametros_final$cdo, pattern = '\\b5201\\b')==TRUE ~ 'Secretaria de Estado da Fazenda - SEF',
  str_detect(string = base_parametros_final$cdo, pattern = '\\b5301\\b')==TRUE ~ 'Secretaria de Estado da Infraestrutura e Mobilidade - SIE',
  str_detect(string = base_parametros_final$cdo, pattern = '\\b4891\\b')==TRUE ~ 'Secretaria de Estado da Saúde - SES',
  str_detect(string = base_parametros_final$cdo, pattern = '\\b1601\\b')==TRUE ~ 'Secretaria de Estado da Segurança Pública - SSP',
  str_detect(string = base_parametros_final$cdo, pattern = '\\b2700\\b')==TRUE ~ 'Secretaria de Estado do Desenvolvimento Econômico Sustentável - SDE',
  str_detect(string = base_parametros_final$cdo, pattern = '\\b2601\\b')==TRUE ~ 'Secretaria de Estado do Desenvolvimento Social - SDS',
  str_detect(string = base_parametros_final$cdo, pattern = '\\b1801\\b')==TRUE ~ 'Secretaria de Estado do Planejamento - SPG',
  str_detect(string = base_parametros_final$cdo, pattern = '\\b4103\\b')==TRUE ~ 'Secretaria Executiva de Articulação Nacional - SAI',
  str_detect(string = base_parametros_final$cdo, pattern = '\\b5801\\b')==TRUE ~ 'Secretaria Executiva de Comunicação - SEC',
  str_detect(string = base_parametros_final$cdo, pattern = '\\b1802\\b')==TRUE ~ 'Superintendência de Desenvolvimento da Região Metropolitana da Grande Florianópolis',
  str_detect(string = base_parametros_final$cdo, pattern = '\\b4002\\b')==TRUE ~ 'Tribunal de Contas do Estado de Santa Catarina - TCE',
  str_detect(string = base_parametros_final$cdo, pattern = '\\b9101\\b')==TRUE ~ 'Agência de Desenvolvimento Regional - Araranguá',
  str_detect(string = base_parametros_final$cdo, pattern = '\\b8401\\b')==TRUE ~ 'Agência de Desenvolvimento Regional - Blumenau',
  str_detect(string = base_parametros_final$cdo, pattern = '\\b7701\\b')==TRUE ~ 'Agência de Desenvolvimento Regional - Campos Novos',
  str_detect(string = base_parametros_final$cdo, pattern = '\\b7301\\b')==TRUE ~ 'Agência de Desenvolvimento Regional - Chapecó',
  str_detect(string = base_parametros_final$cdo, pattern = '\\b7501\\b')==TRUE ~ 'Agência de Desenvolvimento Regional - Concórdia',
  str_detect(string = base_parametros_final$cdo, pattern = '\\b9001\\b')==TRUE ~ 'Agência de Desenvolvimento Regional - Criciúma',
  str_detect(string = base_parametros_final$cdo, pattern = '\\b8001\\b')==TRUE ~ 'Agência de Desenvolvimento Regional - Curitibanos',
  str_detect(string = base_parametros_final$cdo, pattern = '\\b8601\\b')==TRUE ~ 'Agência de Desenvolvimento Regional - Itajaí',
  str_detect(string = base_parametros_final$cdo, pattern = '\\b9301\\b')==TRUE ~ 'Agência de Desenvolvimento Regional - Jaraguá do Sul',
  str_detect(string = base_parametros_final$cdo, pattern = '\\b7601\\b')==TRUE ~ 'Agência de Desenvolvimento Regional - Joaçaba',
  str_detect(string = base_parametros_final$cdo, pattern = '\\b9201\\b')==TRUE ~ 'Agência de Desenvolvimento Regional - Joinville',
  str_detect(string = base_parametros_final$cdo, pattern = '\\b9601\\b')==TRUE ~ 'Agência de Desenvolvimento Regional - Lages',
  str_detect(string = base_parametros_final$cdo, pattern = '\\b9401\\b')==TRUE ~ 'Agência de Desenvolvimento Regional - Mafra',
  str_detect(string = base_parametros_final$cdo, pattern = '\\b7101\\b')==TRUE ~ 'Agência de Desenvolvimento Regional - Maravilha',
  str_detect(string = base_parametros_final$cdo, pattern = '\\b8101\\b')==TRUE ~ 'Agência de Desenvolvimento Regional - Rio do Sul',
  str_detect(string = base_parametros_final$cdo, pattern = '\\b7201\\b')==TRUE ~ 'Agência de Desenvolvimento Regional - São Lourenço do Oeste',
  str_detect(string = base_parametros_final$cdo, pattern = '\\b7001\\b')==TRUE ~ 'Agência de Desenvolvimento Regional - São Miguel do Oeste',
  str_detect(string = base_parametros_final$cdo, pattern = '\\b8901\\b')==TRUE ~ 'Agência de Desenvolvimento Regional - Tubarão',
  str_detect(string = base_parametros_final$cdo, pattern = '\\b7801\\b')==TRUE ~ 'Agência de Desenvolvimento Regional - Videira',
  str_detect(string = base_parametros_final$cdo, pattern = '\\b7401\\b')==TRUE ~ 'Agência de Desenvolvimento Regional - Xanxerê',
  str_detect(string = base_parametros_final$cdo, pattern = '\\b5323\\b')==TRUE ~ 'Departamento de Transportes e Terminais - DETER',
  TRUE ~ 'Outros'
)

## Tratamento de dados

# Cria as coluna cdo e orgaos na base da tabela com as informações dos editais.
# Nestas colunas são setados os códigos de cada órgão e o nome por extenso,
# para uso posterior.

tabela_url$cdo <- case_when(
  str_detect(string = tabela_url$orgao, pattern = '\\bSANTUR\\b')==TRUE ~ '2300',
  str_detect(string = tabela_url$orgao, pattern = '\\bBADESC\\b')==TRUE ~ '302',
  str_detect(string = tabela_url$orgao, pattern = '\\bARESC\\b')==TRUE ~ '2729',
  str_detect(string = tabela_url$orgao, pattern = '\\bCC\\b')==TRUE ~ '4101',
  str_detect(string = tabela_url$orgao, pattern = '\\bCIASC\\b')==TRUE ~ '301',
  str_detect(string = tabela_url$orgao, pattern = '\\bCOHAB\\b')==TRUE ~ '2622',
  str_detect(string = tabela_url$orgao, pattern = '\\bCIDASC\\b')==TRUE ~ '4422',
  str_detect(string = tabela_url$orgao, pattern = '\\bControladoria Geral do Estado\\b')==TRUE ~ '4107',
  str_detect(string = tabela_url$orgao, pattern = 'CBM')==TRUE ~ '1685',
  str_detect(string = tabela_url$orgao, pattern = '\\bDPE\\b')==TRUE ~ '1501',
  str_detect(string = tabela_url$orgao, pattern = '\\bDC\\b')==TRUE ~ '5501',
  str_detect(string = tabela_url$orgao, pattern = '\\bDEINFRA\\b')==TRUE ~ '5325',
  str_detect(string = tabela_url$orgao, pattern = '\\bDETRAN\\b')==TRUE ~ '4112',
  str_detect(string = tabela_url$orgao, pattern = '\\bEPAGRI\\b')==TRUE ~ '9',
  str_detect(string = tabela_url$orgao, pattern = '\\bFCC\\b')==TRUE ~ '2322',
  str_detect(string = tabela_url$orgao, pattern = '\\bFCEE\\b')==TRUE ~ '4521',
  str_detect(string = tabela_url$orgao, pattern = '\\bFESPORTE\\b')==TRUE ~ '2006',
  str_detect(string = tabela_url$orgao, pattern = '\\bFAPESC\\b')==TRUE ~ '4524',
  str_detect(string = tabela_url$orgao, pattern = '\\bSCPREV\\b')==TRUE ~ '200',
  str_detect(string = tabela_url$orgao, pattern = '\\bENA\\b')==TRUE ~ '5230',
  str_detect(string = tabela_url$orgao, pattern = 'UDESC')==TRUE ~ '4522',
  str_detect(string = tabela_url$orgao, pattern = '\\bGVG\\b')==TRUE ~ '4201',
  str_detect(string = tabela_url$orgao, pattern = '\\bIOESC\\b')==TRUE ~ '1504',
  str_detect(string = tabela_url$orgao, pattern = '\\bIPREV\\b')==TRUE ~ '4722',
  str_detect(string = tabela_url$orgao, pattern = '\\bIMA\\b')==TRUE ~ '2721',
  str_detect(string = tabela_url$orgao, pattern = '\\bJUCESC\\b')==TRUE ~ '5222',
  str_detect(string = tabela_url$orgao, pattern = '\\bMPC\\b')==TRUE ~ '4301',
  str_detect(string = tabela_url$orgao, pattern = '\\bMPSC\\b')==TRUE ~ '4001',
  str_detect(string = tabela_url$orgao, pattern = '\\bPolícia Científica de Santa Catarina\\b')==TRUE ~ '1699',
  str_detect(string = tabela_url$orgao, pattern = '\\bPCI\\b')==TRUE ~ '1605',
  str_detect(string = tabela_url$orgao, pattern = '\\bPC\\b')==TRUE ~ '1605',
  str_detect(string = tabela_url$orgao, pattern = '\\bPMSC\\b')==TRUE ~ '1606',
  str_detect(string = tabela_url$orgao, pattern = '\\bPGE\\b')==TRUE ~ '201',
  str_detect(string = tabela_url$orgao, pattern = '\\bSanta Catarina Participação e Investimentos S.A.\\b')==TRUE ~ '111',
  str_detect(string = tabela_url$orgao, pattern = '\\bSCTUR\\b')==TRUE ~ '2323',
  str_detect(string = tabela_url$orgao, regex(pattern = '\\bSCPar\\b', ignore_case = FALSE))==TRUE ~ '1823',
  str_detect(string = tabela_url$orgao, regex(pattern = '\\bSCPAR\\b', ignore_case = FALSE))==TRUE ~ '4126',
  str_detect(string = tabela_url$orgao, pattern = '\\bPSFS\\b')==TRUE ~ '4130',
  str_detect(string = tabela_url$orgao, pattern = '\\bSEA-DGLC\\b')==TRUE ~ '1700',
  str_detect(string = tabela_url$orgao, pattern = 'SAP')==TRUE ~ '801',
  str_detect(string = tabela_url$orgao, pattern = '\\bSAR\\b')==TRUE ~ '4401',
  str_detect(string = tabela_url$orgao, pattern = '\\bSED\\b')==TRUE ~ '4501',
  str_detect(string = tabela_url$orgao, pattern = '\\bSEF\\b')==TRUE ~ '5201',
  str_detect(string = tabela_url$orgao, pattern = '\\bSIE\\b')==TRUE ~ '5301',
  str_detect(string = tabela_url$orgao, pattern = '\\bSES\\b')==TRUE ~ '4891',
  str_detect(string = tabela_url$orgao, pattern = '\\bSSP\\b')==TRUE ~ '1601',
  str_detect(string = tabela_url$orgao, pattern = '\\bSDE\\b')==TRUE ~ '2700',
  str_detect(string = tabela_url$orgao, pattern = '\\bSDS\\b')==TRUE ~ '2601',
  str_detect(string = tabela_url$orgao, pattern = '\\bSPG\\b')==TRUE ~ '1801',
  str_detect(string = tabela_url$orgao, pattern = '\\bSAI\\b')==TRUE ~ '4103',
  str_detect(string = tabela_url$orgao, pattern = '\\bSEC\\b')==TRUE ~ '5801',
  str_detect(string = tabela_url$orgao, pattern = '\\bSuperintendência de Desenvolvimento da Região Metropolitana da Grande Florianópolis\\b')==TRUE ~ '1802',
  str_detect(string = tabela_url$orgao, pattern = '\\bTCE\\b')==TRUE ~ '4002',
  str_detect(string = tabela_url$orgao, pattern = '\\bAgência de Desenvolvimento Regional - Araranguá\\b')==TRUE ~ '9101',
  str_detect(string = tabela_url$orgao, pattern = '\\bAgência de Desenvolvimento Regional - Blumenau\\b')==TRUE ~ '8401',
  str_detect(string = tabela_url$orgao, pattern = '\\bAgência de Desenvolvimento Regional - Campos Novos\\b')==TRUE ~ '7701',
  str_detect(string = tabela_url$orgao, pattern = '\\bAgência de Desenvolvimento Regional - Chapecó\\b')==TRUE ~ '7301',
  str_detect(string = tabela_url$orgao, pattern = '\\bAgência de Desenvolvimento Regional - Concórdia\\b')==TRUE ~ '7501',
  str_detect(string = tabela_url$orgao, pattern = '\\bAgência de Desenvolvimento Regional - Criciúma\\b')==TRUE ~ '9001',
  str_detect(string = tabela_url$orgao, pattern = '\\bAgência de Desenvolvimento Regional - Curitibanos\\b')==TRUE ~ '8001',
  str_detect(string = tabela_url$orgao, pattern = '\\bAgência de Desenvolvimento Regional - Itajaí\\b')==TRUE ~ '8601',
  str_detect(string = tabela_url$orgao, pattern = '\\bAgência de Desenvolvimento Regional - Jaraguá do Sul\\b')==TRUE ~ '9301',
  str_detect(string = tabela_url$orgao, pattern = '\\bAgência de Desenvolvimento Regional - Joaçaba\\b')==TRUE ~ '7601',
  str_detect(string = tabela_url$orgao, pattern = '\\bAgência de Desenvolvimento Regional - Joinville\\b')==TRUE ~ '9201',
  str_detect(string = tabela_url$orgao, pattern = '\\bAgência de Desenvolvimento Regional - Lages\\b')==TRUE ~ '9601',
  str_detect(string = tabela_url$orgao, pattern = '\\bAgência de Desenvolvimento Regional - Mafra\\b')==TRUE ~ '9401',
  str_detect(string = tabela_url$orgao, pattern = '\\bAgência de Desenvolvimento Regional - Maravilha\\b')==TRUE ~ '7101',
  str_detect(string = tabela_url$orgao, pattern = '\\bAgência de Desenvolvimento Regional - Rio do Sul\\b')==TRUE ~ '8101',
  str_detect(string = tabela_url$orgao, pattern = '\\bAgência de Desenvolvimento Regional - São Lourenço do Oeste\\b')==TRUE ~ '7201',
  str_detect(string = tabela_url$orgao, pattern = '\\bAgência de Desenvolvimento Regional - São Miguel do Oeste\\b')==TRUE ~ '7001',
  str_detect(string = tabela_url$orgao, pattern = '\\bAgência de Desenvolvimento Regional - Tubarão\\b')==TRUE ~ '8901',
  str_detect(string = tabela_url$orgao, pattern = '\\bAgência de Desenvolvimento Regional - Videira\\b')==TRUE ~ '7801',
  str_detect(string = tabela_url$orgao, pattern = '\\bAgência de Desenvolvimento Regional - Xanxerê\\b')==TRUE ~ '7401',
  str_detect(string = tabela_url$orgao, pattern = '\\bDETER\\b')==TRUE ~ '5323',
  TRUE ~ 'Outros'
)

tabela_url$orgao <- case_when(
  str_detect(string = tabela_url$cdo, pattern = '\\b2300\\b')==TRUE ~ 'Agência de Desenvolvimento do Turismo - SANTUR',
  str_detect(string = tabela_url$cdo, pattern = '\\b302\\b')==TRUE ~ 'Agência de Fomento do Estado de Santa Catarina - BADESC',
  str_detect(string = tabela_url$cdo, pattern = '\\b2729\\b')==TRUE ~ 'Agência de Regulação de Serviços Públicos de Santa Catarina - ARESC',
  str_detect(string = tabela_url$cdo, pattern = '\\b4101\\b')==TRUE ~ 'Casa Civil - CC',
  str_detect(string = tabela_url$cdo, pattern = '\\b301\\b')==TRUE ~ 'Centro de Informática e Automação de Santa Catarina - CIASC',
  str_detect(string = tabela_url$cdo, pattern = '\\b2622\\b')==TRUE ~ 'Companhia de Habitação do Estado de Santa Catarina S/A - COHAB/SC',
  str_detect(string = tabela_url$cdo, pattern = '\\b4422\\b')==TRUE ~ 'Companhia Integrada de Desenv. Agricola de Santa Catarina - CIDASC',
  str_detect(string = tabela_url$cdo, pattern = '\\b4107\\b')==TRUE ~ 'Controladoria Geral do Estado',
  str_detect(string = tabela_url$cdo, pattern = '\\b1685\\b')==TRUE ~ 'Corpo de Bombeiros Militar - CBM/SC - Fundo de Melhoria',
  str_detect(string = tabela_url$cdo, pattern = '\\b1501\\b')==TRUE ~ 'Defensoria Pública do Estado de Santa Catarina - DPE',
  str_detect(string = tabela_url$cdo, pattern = '\\b5501\\b')==TRUE ~ 'Defesa Cívil - DC',
  str_detect(string = tabela_url$cdo, pattern = '\\b5325\\b')==TRUE ~ 'Departamento Estadual de Infraestrutura - DEINFRA',
  str_detect(string = tabela_url$cdo, pattern = '\\b4112\\b')==TRUE ~ 'Departamento Estadual de Trânsito - DETRAN/SC',
  str_detect(string = tabela_url$cdo, pattern = '\\b9\\b')==TRUE ~ 'Empresa de Pesquisa Agropecuária e Extensão Rural de Santa Catarina - EPAGRI',
  str_detect(string = tabela_url$cdo, pattern = '\\b2322\\b')==TRUE ~ 'Fundação Catarinense de Cultura - FCC',
  str_detect(string = tabela_url$cdo, pattern = '\\b4521\\b')==TRUE ~ 'Fundação Catarinense de Educação Especial - FCEE',
  str_detect(string = tabela_url$cdo, pattern = '\\b2006\\b')==TRUE ~ 'Fundação Catarinense de Esporte - FCE',
  str_detect(string = tabela_url$cdo, pattern = '\\b4524\\b')==TRUE ~ 'Fundação de Amparo á Pesquisa  e Inovação do Estado de Santa Catarina - FAPESC',
  str_detect(string = tabela_url$cdo, pattern = '\\b200\\b')==TRUE ~ 'Fundação de Previdência Complementar do Estado de Santa Catarina - SCPREV',
  str_detect(string = tabela_url$cdo, pattern = '\\b5230\\b')==TRUE ~ 'Fundação Escola de Governo - ENA',
  str_detect(string = tabela_url$cdo, pattern = '\\b4522\\b')==TRUE ~ 'Fundação Universidade do Estado de Santa Catarina - UDESC',
  str_detect(string = tabela_url$cdo, pattern = '\\b4201\\b')==TRUE ~ 'Gabinete do Vice-Governador - GVG',
  str_detect(string = tabela_url$cdo, pattern = '\\b1504\\b')==TRUE ~ 'Imprensa Oficial do Estado de Santa Catarina - IOESC',
  str_detect(string = tabela_url$cdo, pattern = '\\b4722\\b')==TRUE ~ 'Instituto de Previdência do Estado de Santa Catarina - IPREV',
  str_detect(string = tabela_url$cdo, pattern = '\\b2721\\b')==TRUE ~ 'Instituto do Meio Ambiente - IMA',
  str_detect(string = tabela_url$cdo, pattern = '\\b5222\\b')==TRUE ~ 'Junta Comercial do Estado de Santa Catarina - JUCESC',
  str_detect(string = tabela_url$cdo, pattern = '\\b4301\\b')==TRUE ~ 'Ministério Público de Contas do Estado de Santa Catarina - MPC',
  str_detect(string = tabela_url$cdo, pattern = '\\b4001\\b')==TRUE ~ 'Ministério Público do Estado de Santa Catarina Procuradoria Geral de Justiça - MPSC',
  str_detect(string = tabela_url$cdo, pattern = '\\b1699\\b')==TRUE ~ 'Polícia Científica de Santa Catarina',
  str_detect(string = tabela_url$cdo, pattern = '\\b1605\\b')==TRUE ~ 'Polícia Civil - PC',
  str_detect(string = tabela_url$cdo, pattern = '\\b1606\\b')==TRUE ~ 'Polícia Militar do Estado de Santa Catarina - PM/SC',
  str_detect(string = tabela_url$cdo, pattern = '\\b201\\b')==TRUE ~ 'Procuradoria Geral do Estado de Santa Catarina - PGE',
  str_detect(string = tabela_url$cdo, pattern = '\\b111\\b')==TRUE ~ 'Santa Catarina Participação e Investimentos S.A.',
  str_detect(string = tabela_url$cdo, pattern = '\\b2323\\b')==TRUE ~ 'Santa Catarina Turismo S.A. - SCTUR',
  str_detect(string = tabela_url$cdo, pattern = '\\b1823\\b')==TRUE ~ 'SC Participações e Parcerias S.A. - SCPar',
  str_detect(string = tabela_url$cdo, pattern = '\\b4126\\b')==TRUE ~ 'SCPAR Porto de Imbituba S/A',
  str_detect(string = tabela_url$cdo, pattern = '\\b4130\\b')==TRUE ~ 'PSFS Porto de São Francisco do Sul S/A',
  str_detect(string = tabela_url$cdo, pattern = '\\b1700\\b')==TRUE ~ 'Secretaria de Estado da Administração - SEA',
  str_detect(string = tabela_url$cdo, pattern = '\\b801\\b')==TRUE ~ 'Secretaria de Estado da Administração Prisional e Socioeducativa - SAP',
  str_detect(string = tabela_url$cdo, pattern = '\\b4401\\b')==TRUE ~ 'Secretaria de Estado da Agricultura e da Pesca - SAR',
  str_detect(string = tabela_url$cdo, pattern = '\\b4501\\b')==TRUE ~ 'Secretaria de Estado da Educação - SED',
  str_detect(string = tabela_url$cdo, pattern = '\\b5201\\b')==TRUE ~ 'Secretaria de Estado da Fazenda - SEF',
  str_detect(string = tabela_url$cdo, pattern = '\\b5301\\b')==TRUE ~ 'Secretaria de Estado da Infraestrutura e Mobilidade - SIE',
  str_detect(string = tabela_url$cdo, pattern = '\\b4891\\b')==TRUE ~ 'Secretaria de Estado da Saúde - SES',
  str_detect(string = tabela_url$cdo, pattern = '\\b1601\\b')==TRUE ~ 'Secretaria de Estado da Segurança Pública - SSP',
  str_detect(string = tabela_url$cdo, pattern = '\\b2700\\b')==TRUE ~ 'Secretaria de Estado do Desenvolvimento Econômico Sustentável - SDE',
  str_detect(string = tabela_url$cdo, pattern = '\\b2601\\b')==TRUE ~ 'Secretaria de Estado do Desenvolvimento Social - SDS',
  str_detect(string = tabela_url$cdo, pattern = '\\b1801\\b')==TRUE ~ 'Secretaria de Estado do Planejamento - SPG',
  str_detect(string = tabela_url$cdo, pattern = '\\b4103\\b')==TRUE ~ 'Secretaria Executiva de Articulação Nacional - SAI',
  str_detect(string = tabela_url$cdo, pattern = '\\b5801\\b')==TRUE ~ 'Secretaria Executiva de Comunicação - SEC',
  str_detect(string = tabela_url$cdo, pattern = '\\b1802\\b')==TRUE ~ 'Superintendência de Desenvolvimento da Região Metropolitana da Grande Florianópolis',
  str_detect(string = tabela_url$cdo, pattern = '\\b4002\\b')==TRUE ~ 'Tribunal de Contas do Estado de Santa Catarina - TCE',
  str_detect(string = tabela_url$cdo, pattern = '\\b9101\\b')==TRUE ~ 'Agência de Desenvolvimento Regional - Araranguá',
  str_detect(string = tabela_url$cdo, pattern = '\\b8401\\b')==TRUE ~ 'Agência de Desenvolvimento Regional - Blumenau',
  str_detect(string = tabela_url$cdo, pattern = '\\b7701\\b')==TRUE ~ 'Agência de Desenvolvimento Regional - Campos Novos',
  str_detect(string = tabela_url$cdo, pattern = '\\b7301\\b')==TRUE ~ 'Agência de Desenvolvimento Regional - Chapecó',
  str_detect(string = tabela_url$cdo, pattern = '\\b7501\\b')==TRUE ~ 'Agência de Desenvolvimento Regional - Concórdia',
  str_detect(string = tabela_url$cdo, pattern = '\\b9001\\b')==TRUE ~ 'Agência de Desenvolvimento Regional - Criciúma',
  str_detect(string = tabela_url$cdo, pattern = '\\b8001\\b')==TRUE ~ 'Agência de Desenvolvimento Regional - Curitibanos',
  str_detect(string = tabela_url$cdo, pattern = '\\b8601\\b')==TRUE ~ 'Agência de Desenvolvimento Regional - Itajaí',
  str_detect(string = tabela_url$cdo, pattern = '\\b9301\\b')==TRUE ~ 'Agência de Desenvolvimento Regional - Jaraguá do Sul',
  str_detect(string = tabela_url$cdo, pattern = '\\b7601\\b')==TRUE ~ 'Agência de Desenvolvimento Regional - Joaçaba',
  str_detect(string = tabela_url$cdo, pattern = '\\b9201\\b')==TRUE ~ 'Agência de Desenvolvimento Regional - Joinville',
  str_detect(string = tabela_url$cdo, pattern = '\\b9601\\b')==TRUE ~ 'Agência de Desenvolvimento Regional - Lages',
  str_detect(string = tabela_url$cdo, pattern = '\\b9401\\b')==TRUE ~ 'Agência de Desenvolvimento Regional - Mafra',
  str_detect(string = tabela_url$cdo, pattern = '\\b7101\\b')==TRUE ~ 'Agência de Desenvolvimento Regional - Maravilha',
  str_detect(string = tabela_url$cdo, pattern = '\\b8101\\b')==TRUE ~ 'Agência de Desenvolvimento Regional - Rio do Sul',
  str_detect(string = tabela_url$cdo, pattern = '\\b7201\\b')==TRUE ~ 'Agência de Desenvolvimento Regional - São Lourenço do Oeste',
  str_detect(string = tabela_url$cdo, pattern = '\\b7001\\b')==TRUE ~ 'Agência de Desenvolvimento Regional - São Miguel do Oeste',
  str_detect(string = tabela_url$cdo, pattern = '\\b8901\\b')==TRUE ~ 'Agência de Desenvolvimento Regional - Tubarão',
  str_detect(string = tabela_url$cdo, pattern = '\\b7801\\b')==TRUE ~ 'Agência de Desenvolvimento Regional - Videira',
  str_detect(string = tabela_url$cdo, pattern = '\\b7401\\b')==TRUE ~ 'Agência de Desenvolvimento Regional - Xanxerê',
  str_detect(string = tabela_url$cdo, pattern = '\\b5323\\b')==TRUE ~ 'Departamento de Transportes e Terminais - DETER',
  TRUE ~ 'Outros'
)

## Tratamento de dados

# A raspagem da tabela de informações dos editais, retorna com uma pequena falha 
# em algumas linhas, onde algumas informações estão setadas em colunas erradas.
# A rotina abaixa faz um ajuste nessas falhas.
# É criada uma nova coluna de id único, composta pelo número do edital concatenado
# com o código do órgão.

tabela_url$id <- gsub(pattern = ".*?([a-z]+).*", x = tabela_url$edital, replacement = NA)
tabela_url$id <- gsub(pattern = ".*?([A-Z]+).*", x = tabela_url$id, replacement = NA)
tabela_url <- tabela_url %>% filter(!is.na(id))
tabela_url$id <- gsub(pattern = '/', replacement = '', tabela_url$id)
tabela_url$id <- paste0(tabela_url$id,'_',tabela_url$cdo)

base_parametros_final$id <- gsub(pattern = '/', replacement = '', base_parametros_final$edital)
base_parametros_final$id <- paste0(base_parametros_final$id,'_',base_parametros_final$cdo)

for (i in 1:nrow(tabela_url)) {
  if (str_detect(pattern = ".*?([A-Z]+).*", string = tabela_url$entrega_de_proposta[i])) {
    tabela_url$entrega_de_proposta[i] <- tabela_url$docs[i]
    tabela_url$situacao[i] <- tabela_url$na[i]
  }else if (str_detect(pattern = ".*?([a-z]+).*", string = tabela_url$entrega_de_proposta[i])) {
    tabela_url$entrega_de_proposta[i] <- tabela_url$docs[i]
    tabela_url$situacao[i] <- tabela_url$na[i]
  }
}
tabela_url <- tabela_url[,-c(7,9)]
tabela_url_1 <- tabela_url %>% filter(situacao == 'EM RECEBIMENTO DE PROPOSTA')
tabela_url_2 <- tabela_url %>% filter(situacao == 'EM RECEBIMENTO DE PROPOSTA E-LIC')
tabela_url <- bind_rows(tabela_url_1, tabela_url_2)

rm(tabela_url_1, tabela_url_2)

## Join entre as tabelas de paramêmetros e a tabela de editais

# Este join verifica os editais comuns entre as duas tabelas, gerando como saída 
# uma nova base  de parâmetros.
# Esta verificação é necessária para que, ao final do script, o Dashboard possa
# mostrar informações relativas aos editais, como datas, status e órgãos.

base_parametros_final <- semi_join(x = base_parametros_final, y = tabela_url, by = 'id')

## Montagem de URLs

# A rotina a seguir monta as URLs das páginas específicas de cada edital para, posteriormente,
# capturar o links para download dos documentos.

base <- 'https://sistemas4.sc.gov.br/sea/portaldecompras/docsl.asp?'
portal <- 'portal='
processo <- '&processo='
cdo <- '&cdo='
edital <- '&edital='

base_parametros_final$link_repositorio_edital <- NA
for (i in 1:nrow(base_parametros_final)) {
  print(paste0('Montando Link ', i, ' de ', nrow(base_parametros_final)))
  link_repositorio_edital <- paste0(base,
                                    portal, base_parametros_final$portal[i],
                                    processo, base_parametros_final$processo[i],
                                    cdo, base_parametros_final$cdo[i],
                                    edital, base_parametros_final$edital[i])
  base_parametros_final$link_repositorio_edital[i] <- link_repositorio_edital
}

base_referencia <- readRDS('referencia_webscraping/23_09_2022_webscraping_referencia.rds')
base_referencia <- as.data.frame(base_referencia[,'link_repositorio_edital'])
colnames(base_referencia) <- 'link_repositorio_edital'

rm(base)
rm(portal)
rm(processo)
rm(cdo)
rm(edital)
rm(busca_links)
rm(link_repositorio_edital)

## Cria base final

# A linha abaixo apenas atribui os valores da base de parâmetros a uma nova base
# de dados chamada de "base_final".

base_final <- base_parametros_final

rm(base_parametros_final)
rm(base_links_direto)

## Join com a base de referencia para trabalhar apenas nos dados novos

base_final <- anti_join(x = base_final, y = base_referencia, by = 'link_repositorio_edital')

if (nrow(base_final) > 1) {

  ## Tratamento de dados
  
  # A rotina a seguir cria duas novas colunas na tabela de informações de editais,
  # contendo as datas de abertura e entrega de propostas tratadas no formato de
  # dia/mês/ano.
  # É feito ainda um join para a tabela final, que receberá os dados da tabela de
  # informações de editais.
  
  tabela_url$data_abertura <- str_sub(tabela_url$abertura, 1 , 10)
  tabela_url$data_entrega_proposta <- str_sub(tabela_url$entrega_de_proposta, 1 , 10)
  
  base_final <- base_final %>% mutate_all(as.character)
  tabela_url <- tabela_url %>% mutate_all(as.character)
  
  tabela_url <- tabela_url %>% distinct(id, .keep_all = TRUE)
  
  base_final <- left_join(base_final, tabela_url, by = "id")
  
  base_final <- base_final %>% distinct()
  
  base_final$base_final[is.na(base_final$data_abertura)] <- NA
  base_final$base_final[is.na(base_final$data_entrega_proposta)] <- NA
  
  ## Raspagem 3
  
  # A rotina abaixo faz a raspagem das partes finais das URLs referente aos links
  # dos editais para download.
  
  base_final$final_url_edital <- NA
  i <- b <- 1
  erro_desconhecido <- 0
  controle_pulos_linha <- data.frame()
  while (b < (nrow(base_final)+1)) {
    time_limit <- 1
    setTimeLimit(cpu = time_limit, elapsed = time_limit, transient = TRUE)
    on.exit({setTimeLimit(cpu = Inf, elapsed = Inf, transient = FALSE)})
    tryCatch({
      for (i in i:nrow(base_final)) {
        print(paste0('Raspando link ', i, ' de ', nrow(base_final)))
        lista <- read_html(base_final$link_repositorio_edital[i]) %>% html_nodes('a') %>% html_attr('href')
        base_final$final_url_edital[i] <- lista[1]
        print(base_final$final_url_edital[i])
        b <- i + 1
      }
    }, error = function(e) {
      if (grepl("reached elapsed time limit|reached CPU time limit", e$message)) {
        print('Chamando função secundária - Tempo limite atingido')
        #print('Dormindo 2 segundos')
        #Sys.sleep(2)
        #funcao_read_html_persistente(base, i, tempo) # we reached timeout, apply some alternative method or do something else
      } else {
        print('Erro desconhecido') # error not related to timeout
        print('Dormindo 4 segundos')
        Sys.sleep(4)
        erro_desconhecido <- erro_desconhecido + 1
        #stop(e)
        if (erro_desconhecido >=5) {
          print('Pulando linha de índice ', i)
          controle_pulos_linha[i] <- base_final[i,]
          i <- i + 1
        }
      }
    })
  }
  
  rm(i)
  rm(b)
  rm(lista)
  rm(tmp)
  
  ## Tratamento de dados
  
  # Cria a coluna link_completo_para_editais a partir da junção da URL base
  # com a final_url_edital, capturadas no rotina anterior.
  # Faz ajuste nas colunas, com exclusão e mudança de nomes.
  # Ao final salva o arqriovo base_final em uma paste de backups
  
  base_final$link_completo_para_editais <- paste0('https://sistemas4.sc.gov.br/sea/portaldecompras/',base_final$final_url_edital)
  
  base_final <- base_final[,-c(8,15,16)]
  lista_coluna <- colnames(base_final)
  lista_coluna[3] <- 'edital'
  lista_coluna[4] <- 'cdo'
  lista_coluna[5] <- 'orgao'
  colnames(base_final) <- lista_coluna
  rm(lista_coluna)
  
  data <- str_replace_all(string = Sys.Date(), pattern = '-', replacement = '_')
  #write_rds(base_final, paste0('backup_bases/base_final_sem_conteudo_', data, '.rds'))
  
  rm(link_documento)
  rm(links_documentos_final)
  rm(data)
  
  ## Download dos editais
  
  # A rotina abaixo fará o download dos editais para posterior leitura.
  # Renomeia os arquivos no momento de salvar, setando com os valores da coluna id
  # (número do edital concatenado com o código do órgão).
  # Falhas de rede podem acontecer durante o download e com isso parar o script.
  # Antes de efetuar o download, é verificado quais arquivos já estão na pasta.
  # Com isso, caso o código pare, ao recomeçar não será necessário baixar novamente
  # os arquivos que já estão na máquina (útil no caso de atualização da base).
  
  id <- list.files(path = 'editais/', full.names = TRUE)
  id <- str_remove(string = id, pattern = 'editais/')
  id <- str_remove(string = id, pattern = '.pdf')
  id <- str_remove(string = id, pattern = '.doc')
  id <- str_remove(string = id, pattern = '.docx')
  lista_pdf <- data.frame(id)
  colnames(lista_pdf) <- 'id'
  base_final_lista_download <- base_final
  base_final_lista_download <- anti_join(x = base_final_lista_download, y = lista_pdf, by = 'id')
  
  controle <- 10
  while (controle > 0) {
    id <- list.files(path = 'editais/', full.names = TRUE)
    id <- str_remove(string = id, pattern = 'editais/')
    id <- str_remove(string = id, pattern = '.pdf')
    id <- str_remove(string = id, pattern = '.doc')
    id <- str_remove(string = id, pattern = '.docx')
    lista_pdf <- data.frame(id)
    colnames(lista_pdf) <- 'id'
    base_final_lista_download <- base_final
    base_final_lista_download <- anti_join(x = base_final_lista_download, y = lista_pdf, by = 'id')
    for (i in 1:nrow(base_final_lista_download)) {
      print(paste0('Baixando arquivo ',i, ' de ', nrow(base_final_lista_download)))
      if (str_detect(string = base_final_lista_download$link_completo_para_editais[i], pattern = 'pdf')) {
        tryCatch({
          download.file(mode = "wb", url = base_final_lista_download$link_completo_para_editais[i],
                        destfile = paste0('editais/',
                                          base_final_lista_download$id[i],
                                          '.pdf'))
        }, error = function(e) {
          if (grepl("reached elapsed time limit|reached CPU time limit", e$message)) {
            print('Chamando função secundária - Tempo limite atingido')
            print('Dormindo 2 segundos')
            Sys.sleep(2)
          }
        })     
        
      } else if (str_detect(string = base_final_lista_download$link_completo_para_editais[i], pattern = 'doc\\b')) {
        tryCatch({
          download.file(mode = "wb", url = base_final_lista_download$link_completo_para_editais[i],
                        destfile = paste0('editais/',
                                          base_final_lista_download$id[i],
                                          '.doc'))
        }, error = function(e) {
          if (grepl("reached elapsed time limit|reached CPU time limit", e$message)) {
            print('Chamando função secundária - Tempo limite atingido')
            print('Dormindo 2 segundos')
            Sys.sleep(2)
          }
        })
      
      } else if (str_detect(string = base_final_lista_download$link_completo_para_editais[i], pattern = 'docx\\b')) {
        tryCatch({
          download.file(mode = "wb", url = base_final_lista_download$link_completo_para_editais[i],
                        destfile = paste0('editais/',
                                          base_final_lista_download$id[i],
                                          '.docx'))
        }, error = function(e) {
          if (grepl("reached elapsed time limit|reached CPU time limit", e$message)) {
            print('Chamando função secundária - Tempo limite atingido')
            print('Dormindo 2 segundos')
            Sys.sleep(2)
          }
        })
          }
    }
    controle <- controle - 1
    print(paste0('Faltam ', controle, ' tentativas de baixar os editais com erro'))
    print('Dormindo 2 segundos')
    Sys.sleep(2)
  }
  
  rm(id)
  rm(base_final_lista_download)
  
  # Não há uma padronização no formato dos arquivos disponíveis para download.
  # Assim, o trecho abaixo, usa uma funcionalidade
  # do LibreOffice para conversão de documentos para o formato PDF.
  
  ## Converte arquivos DOCX em PDF
  
  lista_pdf <- list.files(path = 'editais/', pattern = '\\b.pdf\\b', full.names = TRUE)
  lista_docx <- list.files(path = 'editais/', pattern = '\\b.docx\\b', full.names = TRUE)
  lista_doc <- list.files(path = 'editais/', pattern = '\\b.doc\\b', full.names = TRUE)
  
  lista_compara_pdf <- str_remove(string = lista_pdf, pattern = '.pdf')
  lista_compara_docx <- str_remove(string = lista_docx, pattern = '.docx')
  lista_compara_doc <- str_remove(string = lista_doc, pattern = '.doc')
  
  lista_docx <- setdiff(lista_compara_docx, lista_compara_pdf)
  lista_docx <- paste0(lista_docx, '.docx')
  
  lista_doc <- setdiff(lista_compara_doc, lista_compara_pdf)
  lista_doc <- paste0(lista_doc, '.doc')
  
  if (length(lista_docx)>1) {
    for (i in 1:length(lista_docx)) {
      print(paste0('Convertendo arquivo de formato DOCX: ', i, ' de ', length(lista_docx)))
      tmp <- try(docxtractr::convert_to_pdf(path = lista_docx[i], pdf_file = paste0(str_remove(string = lista_docx[i], pattern = '.docx'), '.pdf')))
      # if (!(inherits(tmp,"try-error")))
      #   i <- i + 1
    }
  }else{
    print('Nenhum arquivo a ser convertido no formato DOCX')
  }
  
  ## Converte arquivos DOC em PDF
  if (length(lista_doc)>1) {
    for (i in 1:length(lista_doc)) {
      print(paste0('Convertendo arquivo de formato DOC: ', i, ' de ', length(lista_doc)))
      tmp <- try(docxtractr::convert_to_pdf(path = lista_doc[i], pdf_file = paste0(str_remove(string = lista_doc[i], pattern = '.doc'), '.pdf')))
      # if (!(inherits(tmp,"try-error")))
      #   i <- i + 1
    }
  }else{
    print('Nenhum arquivo a ser convertido no formato DOC')
  }
  
  ## Leitura do conteúdo dos editais
  
  # A rotina abaixo faz a leitura do conteúdos dos editais em PDF e salva na base_final.
  # Ao final salva a base em uma pasta de backup.
  
  lista_pdf <- list.files(path = 'editais/', pattern = '*.pdf', full.names = TRUE)
  
  lista_pdf <- str_remove(string = lista_pdf, pattern = 'editais/')
  lista_pdf <- str_remove(string = lista_pdf, pattern = '.pdf')
  
  lista_pdf <- as.data.frame(lista_pdf)
  
  base_final <- semi_join(x = base_final, y = lista_pdf, by = c('id' = 'lista_pdf'))
  lista_pdf <- semi_join(x = lista_pdf, y = base_final, by = c('lista_pdf' = 'id'))
  
  lista_pdf$lista_pdf <- paste0('editais/',lista_pdf$lista_pdf,'.pdf')
  
  base_final <- base_final %>% arrange(id)
  lista_pdf <- lista_pdf %>% arrange(lista_pdf)
  
  base_final$conteudo_edital <- NA
  base_final$id_arquivo <- NA
  
  for (i in 1:nrow(lista_pdf)) {
    tmp <- try(base_final$conteudo_edital[i] <- str_flatten(pdftools::pdf_text(pdf = lista_pdf$lista_pdf[i])))
    base_final$id_arquivo[i] <- lista_pdf$lista_pdf[i]
    print(paste0("Carregando conteúdo do Edital ",i, ' de ',nrow(lista_pdf)))
    print(paste0("ID: ",base_final$id[i],' - ',str_remove(string = lista_pdf$lista_pdf[i], pattern = 'editais/')))
  }  
  
  base_final <- base_final %>% filter(!is.na(conteudo_edital))
  
  base_final$conteudo_edital <- textclean::replace_white(base_final$conteudo_edital)
  base_final$conteudo_edital <- textclean::add_comma_space(base_final$conteudo_edital)
  base_final$conteudo_edital <- str_to_lower(base_final$conteudo_edital)
  
  base_final$conteudo_edital_original <- base_final$conteudo_edital 
  base_final$conteudo_edital <- iconv(base_final$conteudo_edital,from="UTF-8",to="ASCII//TRANSLIT")
  
  rm(lista_pdf)
  
  ## carrega lista de materiais
  
  source(file = 'listas_materiais/raspagem_portal_classes_grupos.R', encoding = 'utf-8')
  
  ## inicia regex
  
  ## regex na base de materiais
  base_final$item <- 'Diversos'
  base_final$grupo <- 'Diversos'
  base_final$tipo <- 'Diversos'
  regex_termos_final <- data.frame()
  for (v in 1:nrow(base_final)) {
    print(paste0('Executando busca Regex no edital ', v, ' de ', nrow(base_final)))
    for (r in 1:nrow(base_final_grupos_classes_portal)) {
      if (base_final_grupos_classes_portal$tipo_pt_br[r] != 'Diversos') {
        if (grepl(x = base_final$conteudo_edital[v], pattern = base_final_grupos_classes_portal$item_lower[r])) {
          regex_termos_passagem <- base_final[v,]
          regex_termos_passagem$item <- base_final_grupos_classes_portal$item[r]
          regex_termos_passagem$tipo_pt_br <- base_final_grupos_classes_portal$tipo_pt_br[r]
          regex_termos_passagem$grupo <- base_final_grupos_classes_portal$grupo[r]
          regex_termos_passagem$tipo <- base_final_grupos_classes_portal$tipo[r]
          print(paste0('Encontrado: ', base_final_grupos_classes_portal$tipo_pt_br[r]))
          regex_termos_final <- bind_rows(regex_termos_final, regex_termos_passagem)
        }
      }
    }
  }
  
  sem_match_regex <- anti_join(x = base_final, y = regex_termos_final, by = 'final_url_edital')
  base_final <- bind_rows(sem_match_regex, regex_termos_final)
  base_final <- base_final %>% distinct()
  base_final$orgao <- str_to_title(base_final$orgao)
  base_final$descricao_do_objeto <- str_to_title(base_final$descricao_do_objeto)
  base_final$situacao <- str_to_title(base_final$situacao)
  
  base_final <- clean_names(base_final)
  
  ## Sobe dados para o BigQuery da base CLASSE
  
  # Abaixo é feita uma conexão com o BigQuery e os dados são armazenados para
  # posterior conexão no Dashboard do DataStudio.
  # É preciso setar a rotina abaixo com os dados de conexão s dados abaixo 
  
  
  #install.packages('devtools')
  #devtools::install_github("r-dbi/bigrquery")
  
  httr::set_config(httr::config(ssl_verifypeer = 0L))
  httr::set_config(httr::config(http_version = 0))
  options(httr_oob_default = TRUE)
  
  bq_auth(email = 'xxxxxxxxxxxx')
  
  billing <- "xxxxxxxxxxxx"
  
  library(DBI)
  
  con <- dbConnect(
    bigrquery::bigquery(),
    project = "xxxxxxxxxxxx",
    dataset = "xxxxxxxxxxxx",
    billing = billing
  )
  con
  
  if(nrow(base_final)>0){
    tablename <- "xxxxxxxxxxxx"
    
    base_final <- base_final %>% mutate_all(as.character)
    
    bigrquery::dbWriteTable(con, tablename, base_final, append=T, verbose = T, row.names=F, overwrite=F, fields=base_final)
    print("Base Uploaded")
  }
  
  ############### Removendo duplicados #################
  
  project <- "xxxxxxxxxxxx" # put your project ID here
  
  sql <- ("CREATE OR REPLACE TABLE xxxxxxxxxxxx AS SELECT DISTINCT * FROM xxxxxxxxxxxx")
  invisible(capture.output(dbSendQuery(con,sql)))
  }
  ########################################################
  
print(paste('Data e hora do final do script:',format(Sys.time(), "%d/%m/%Y"), 'às',format(Sys.time(), "%H:%M:%S")))