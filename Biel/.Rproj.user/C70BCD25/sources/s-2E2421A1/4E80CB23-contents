
# Instalação e Carregamento de Todos os Pacotes ---------------------------

pacotes <- c("bigrquery",
             "rvest",
             "tidyverse",
             "janitor",
             "xml2",
             "splitstackshape")

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
