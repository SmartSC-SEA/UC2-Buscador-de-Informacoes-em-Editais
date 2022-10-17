# UC2-Buscador-de-informacoes-em-editais
Repositório do projeto de robô para busca de informações sobre materiais e serviços nos editais de licitação do Governo do Estado de Santa Catarina.

# Link para acesso do Dashboard:
https://datastudio.google.com/u/0/reporting/d2e10796-2f70-4179-a762-6dec0fce36b0/page/p_hph188vyyc

## Arquitetura
### **Front-end**
 - Interface do usuário para busca de informações em editais de licitação desenvolvido por meio do Google Data Studio.
 
### **Back-end**
 - Script desenvolvido em linguagem R, baseado nas funções do pacote "rvest".
 - Armazenamento dos dados em cloud na plataforma Google BigQuery, com o uso do pacote "bigrquery".
 
## **Justificativa**
Atualmente os fornecedores do Governo do Estado de Santa Catarina utilizam o Portal de Compras SC para fins de buscar editais para participação. Porém, não é possível realizar uma busca por itens nos editais, o que gera maior trabalho para os fornecedores e risco de diminuição de participação devido à não identificação de editais que venham a conter os itens com os quais dado fornecedor trabalha. Assim, os fornecedores se vêem obrigados a abrir cada um dos editais individualmente para fins de verificação de itens. 

## **Premissas**
- O robô deverá raspar dados dos editais de licitação afim de identificar itens contantes no Plano Anual de Compras nos editais publicados;
- A aplicação deverá apresentar de forma amigável a lista de itens contantes no Plano Anual de Compras para que os fornecedores possam selecionar os itens com os quais trabalham afim de verificar os editais nos quais poderão participar.
- Após selecionar os itens e realizar a busca, a aplicação deverá retornar os editais nos quais constam os itens buscados pelos fornecedores.

## Ajuda
- Para a execução ou modificação dos scripts é preciso que o usuário tenha instalado em sua máquina a linguagem R, que deverá ser baixada no endereço:
https://www.r-project.org/
- Recomenda-se a utilização da ferramenta de software livre RStudio, que deverá ser baixada no endereço:
https://www.rstudio.com/
- Antes de executar o Script "02_carga_inicial_RODAR_APENAS_A_PRIMEIRA_VEZ.R"
- verificar ou instalar os pacotes necessários executando o script "01.instalacao_de_pacotes.R"
