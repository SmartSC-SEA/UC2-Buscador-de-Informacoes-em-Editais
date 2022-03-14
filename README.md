# BIEL-robo-buscador-de-informacoes-em-editais-de-licitacao
Repositório do projeto de robô para busca de informações sobre materiais e serviços nos editais de licitação do Governo do Estado de Santa Catarina.

## Arquitetura
### **Front-end**
 - *Interface do usuário:* Interação com a aplicação, para monitoramento e busca de informações em editais de licitação desenvolvido por meio do Google Data Studio.

### **Back-end**
 - *Motor de workflow:* suporte à automação de processos, regras off-chain, registro e autenticação de informações na blockchain por meio do Camunda BPMS.
 - *contratos inteligentes:* Smart contracts desenvolvidos em Solidity para implementação na rede blockchain Ethereum ou similar.
 - *Banco de dados relacional:* Para armazenamento de dados off-chain (dados de usuários, logs de sistema, regras de negócios relativas ao processo etc.), utiliza-se banco de dados H2, nativo do Camunda BPMS.
 - *Rede Blockchain:* Protótipo construído sobre a rede Ethereum, para armazenamento de dados on-chain e processamento de regras relativas aos contratos inseridos nos smart contracts.

## **Justificativa**
Atualmente os fornecedores do Governo do Estado de Santa Catarina utilizam o Portal de Compras SC para fins de buscar editais para participação. Porém, não é possível realizar uma busca por itens nos editais, o que gera maior trabalho para os fornecedores e risco de diminuição de participação devido à não identificação de editais que venham a conter os itens com os quais dado fornecedor trabalha. Assim, os fornecedores se vêem obrigados a abrir cada um dos editais individualmente para fins de verificação de ites. 

## **Premissas**
- O robô deverá raspar dados dos editais de licitação afim de identificar itens contantes no Plano Anual de Compras nos editais publicados;
- A aplicação deverá apresentar de forma amigável a lista de itens contantes no Plano Anual de Compras para que os fornecedores possam selecionar os itens com os quais trabalham afim de verificar os editais nos quais poderão participar.
- Após selecionar os itens e realizar a busca, a aplicação deverá retornar os editais nos quais constam os itens buscados pelos fornecedores.

# ISRBP - Information Seeker Robot in Publicated Bidding
Repository of the robot project to search for information about materials and services in the bidding documents of the Government of the State of Santa Catarina.
 
## Architecture
### **Front-end**
- *User interface:* Interaction with the application, for monitoring and searching for information in bidding documents developed through Google Data Studio.

### **Backend**

## **Justification**
Currently, Santa Catarina State Government suppliers use the SC Procurement Portal to search for public notices for participation. However, it is not possible to search for items in the notices, which creates more work for suppliers and the risk of reduced participation due to the non-identification of notices that may contain the items with which a given supplier works. Thus, suppliers are obliged to open each one of the notices individually for the purposes of item verification.

## **Assumptions**
- The robot must scrape data from the bidding notices in order to identify items contained in the Annual Purchase Plan in the published notices;
- The application must present in a friendly way the list of items included in the Annual Purchasing Plan so that suppliers can select the items they work with in order to verify the notices in which they can participate.
- After selecting the items and performing the search, the application should return the notices containing the items sought by the suppliers.
