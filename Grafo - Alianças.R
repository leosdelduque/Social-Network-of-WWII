##Instalar os pacotes 'igraph' para construção das redes, o pacote 'networkD3' para visualização dinâmica da mesma e o pacote 'dplyr' para manipulação de dados se necessário
install.packages("igraph")
install.packages("dplyr")


##Carregar os pacotes para o uso. Lembrando que se os pacotes já estiverem instalados na máquina apenas carregá-los usando o 'library'
library(igraph)
library(dplyr)

##Crie o primeiro objeto usando o comando 'read.csv' para carregar os dados (não é o único método mas funciona também). O comando tem três argumentos que vc pode ou não usar: 1) 'file.choose()' para a escolha do arquivo. 2) 'header =' para que você informe se o cabeçalho será carregado ou não. E 3 - 'sep =' para indicação do separador dos dados.  
alliance_v4_1_by_dyad_yearly <- read.csv(file.choose(), sep =",")
---------------------------------------------------------1936---------------------------------------------------
### Faça o Data Wrangling inicial
##Neste caso eliminamos as colunas não necessárias usando o comando 'select' para selecionar apenas as colunas que queremos.
##Depois, na mesma expressão, usamos o comando 'filter' para filtrar as linhas que queremos. Neste caso queremos as linhas que sejam iguais a "1936" pois é nosso ano de interesse.
##Por fim, na mesma expressão, usamos o comando 'mutate' para transformar os dados da forma que queremos.
## IMPORTANTE: os comandos devem ser separados por pipe (%>%) para que o R execute tudo de uma vez no mesmo objeto.
alliance_36_clean <- (alliance_v4_1_by_dyad_yearly %>%
                     select(state_name1, state_name2, year, defense, neutrality, nonaggression, entente) %>%
                     filter(year == '1936') %>%
                     mutate(defense = replace(defense, defense==1, "6"),
                           neutrality = replace(neutrality, neutrality==1, "2"),
                           nonaggression = replace(nonaggression, nonaggression==1, "2"),
                           entente = ifelse(is.na(entente), 0, entente)))

##Transforme as colunas defense, neutrality, nonaggression, entente em número pois elas estão em formato string. Use o 'mutate = as.numeric' para fazer isso.
alliance_36_clean <- alliance_36_clean %>%
  mutate(
    defense = as.numeric(defense),
    neutrality = as.numeric(neutrality), 
    nonaggression = as.numeric(nonaggression),
    entente = as.numeric(entente))

##Adicionamos um nova coluna 'Weight' ao dataset e somamos os pesos dados a cada tipo de aliança (defense, neutrality, nonaggression, entente).
##Cada tipo de aliança tem um peso diferente com base na intensidade do compromisso militar.
alliance_36_clean <- alliance_36_clean %>%
                     mutate(Weight = defense + neutrality + nonaggression + entente)

##Excluímos as colunas dyad_st_year, defense, neutrality, nonaggression, entente e ficamos só com os pesos na coluna 'Weight'.
##Bolívia e Equador já haviam firmado Pactos de Não Agressão e Neutralidade. Apenas atualizamos os valores para 1936. 
alliance_36_clean <- alliance_36_clean %>%
                        select(state_name1, state_name2, Weight) %>%
                        mutate(Weight = ifelse(state_name1 == "Ecuador" & state_name2 == "Bolivia", 5, Weight))

##Equador e Bolivia tinham entradas duplicadas. Excluímos a primeira delas. 
alliance_36_clean <- alliance_36_clean[-2,]


##A partir dos dados carregados e limpos crie um novo objeto, o grafo. 
##Use o comando 'graph_from_data_frame' ou 'graph_from_edgelist(as.matrix)'. 
## O 1º comando terá apenas dois argumentos: 1) o nome da base de dados, definida no passo anterior; 2) o tipo de rede, neste caso 'directed = False' por ser uma rede Indireta. 
## 0 2º comando é mais extenso, temos X argumento: 1 o nmome da base de dados, 2 as colunas que serão source e target, e 3 o tipo de rede, neste caso 'directed = False' por ser uma rede Indireta
alliance_36_clean_grafo <- graph_from_data_frame(alliance_36_clean, directed = FALSE)
alliance_36_clean_grafo_2 <- graph_from_edgelist(as.matrix(alliance_36_clean[, c("state_name1", "state_name2")]), directed = FALSE)

##Podemos colocar os pesos nas arestas manualment utilizando a função 'set_edge_attr'. Precisamso definir: 1 a base;2 qual atributo, nesse caso o peso, e 3 - da onde virão os valores, nesse caso da nossa coluna Weight.
alliance_36_clean_grafo <- set_edge_attr(alliance_36_clean_grafo, "weight", value= alliance_36_clean$Weight)

##Por fim podemos confirmar se a grafo está realmente com pesos e quais são eles usando os comandos abaixo. 
is_weighted(alliance_36_clean_grafo)
E(alliance_36_clean_grafo)$weight

###Plotando a nossa rede social
##Podemos usar o comando 'plot' porém ele dificulta a viz de grandes redes ou podemos usar o 'tkplot' que permite-nos mexer nos objetos e visualisa-los usandos algoritmos.
edge_width_36 <- alliance_36_clean$Weight
tkplot (as.undirected(alliance_36_clean_grafo),
        canvas.height = 250,
        canvas.width = 1000,
        vertex.size = 15,
        vertex.label.dist = 0, vertex.label.color = "darkblue",
        vertex.label.cex = 0.8, vertex.label.family = "sans",
        vertex.shapes="none", vertex.color = ifelse(V(alliance_36_clean_grafo)$name == 'Brazil', "red", "#B0C4DE"),
        edge.curved = FALSE, edge.width = edge_width_36, edge.label = alliance_36_clean$Weight, edge.color = 'grey')

system("convert Rplots.eps Rplots.jpg")
###Podemos tirar algumas medidas descritivas
#metricas dos nós, temos que armazenar as medidas como atributos. É importante notar que as métricas levam em consideração os pesos dos links.
V(alliance_36_clean_grafo)$Degree_Ponderado <- strength(alliance_36_clean_grafo, weights = E(alliance_36_clean_grafo)$weight)
V(alliance_36_clean_grafo)$Degree <- degree(alliance_36_clean_grafo)
V(alliance_36_clean_grafo)$Closeness_Ponderado <- closeness(alliance_36_clean_grafo, mode = "all", weights = E(alliance_36_clean_grafo)$weight, normalized=T)
V(alliance_36_clean_grafo)$Betweenneess_Ponderado <- betweenness(alliance_36_clean_grafo, directed=F, weights = E(alliance_36_clean_grafo)$weight)
V(alliance_36_clean_grafo)$Excentricidade <-eccentricity(alliance_36_clean_grafo)
V(alliance_36_clean_grafo)$Bridge <- 0
V(alliance_36_clean_grafo)$Bridge[articulation_points(alliance_36_clean_grafo) ] <- 1
V(alliance_36_clean_grafo)$Clustering_Coefficient <- transitivity(alliance_36_clean_grafo, type = "local")

#Armazenando todas as medidas e transformando em data frame
tabela_nos_aliança_36 <- igraph::as_data_frame(alliance_36_clean_grafo, what="vertices")

#Como vamos visualizar apenas os países das Américas, vamos criar um objeto com eles para que possamos filtar a nossa tabela criada acima. 
Metricas_Americas <- c("Brazil", "United States of America", "Cuba", "Haiti", "Dominican Republic", 
                       "Mexico", "Guatemala", "Honduras", "El Salvador", "Nicaragua", 
                       "Costa Rica", "Panama", "Colombia", "Venezuela", "Peru", 
                       "Bolivia", "Paraguay", "Chile", "Argentina", "Uruguay", "Ecuador")

#Filtar a tabela usando o objeto criado contendo apenas os países das Américas.
#Renomeando a primeira coluna da tabela usando 'rename'.
#Arrendondando os valores da tabela para 4 casas decimais usando o mutate
tabela_nos_aliança_36_filtrada <- tabela_nos_aliança_36 %>% 
                                  filter(name %in% Metricas_Americas) %>%
                                  rename(País = name) %>%
                                  mutate(
                                    Degree_Ponderado = round(Degree_Ponderado, 4),
                                    Degree = round(Degree, 4),
                                    Closeness_Ponderado = round(Closeness_Ponderado, 4),
                                    Betweenneess_Ponderado = round(Betweenneess_Ponderado, 4),
                                    Excentricidade = round(Excentricidade, 4),
                                    Bridge = round(Bridge, 4))
                                 

## metricas da rede (geral)
#densidade do grafo
edge_density(alliance_36_clean_grafo)

#diametro da rede
diameter(alliance_36_clean_grafo)

#raio da rede
radius(alliance_36_clean_grafo)

#comprimeiro medio do caminho
mean_distance(alliance_36_clean_grafo)

---------------------------------------------------------------1942---------------------------------------------------------------
### Faça o Data Wrangling inicial
##Neste caso eliminamos as colunas não necessárias usando o comando 'select' para selecionar apenas as colunas que queremos.
##Depois, na mesma expressão, usamos o comando 'filter' para filtrar as linhas que queremos. Neste caso queremos as linhas que sejam iguais a "1936" pois é nosso ano de interesse.
##Por fim, na mesma expressão, usamos o comando 'mutate' para transformar os dados da forma que queremos.
## IMPORTANTE: os comandos devem ser separados por pipe (%>%) para que o R execute tudo de uma vez no mesmo objeto.
alliance_42_clean <- (alliance_v4_1_by_dyad_yearly %>%
                          select(state_name1, state_name2, year, defense, neutrality, nonaggression, entente) %>%
                          filter(year == '1942') %>%
                          mutate(defense = replace(defense, defense==1, "6"),
                                 neutrality = replace(neutrality, neutrality==1, "2"),
                                 nonaggression = replace(nonaggression, nonaggression==1, "2")))

##Transforme as colunas defense, neutrality, nonaggression, entente em número pois elas estão em formato string. Use o 'mutate = as.numeric' para fazer isso.
alliance_42_clean <- alliance_42_clean %>%
  mutate(
    defense = as.numeric(defense),
    neutrality = as.numeric(neutrality), 
    nonaggression = as.numeric(nonaggression),
    entente = as.numeric(entente)
  )
  
##Adicionamos um nova coluna 'Weight' ao dataset e somamos os pesos dados a cada tipo de aliança (defense, neutrality, nonaggression, entente).
##Cada tipo de aliança tem um peso diferente com base na intensidade do compromisso militar.
alliance_42_clean <- alliance_42_clean %>%
  mutate(Weight = defense + neutrality + nonaggression + entente)

##Excluímos as colunas dyad_st_year, defense, neutrality, nonaggression, entente e ficamos só com os pesos na coluna 'Weight'.
##Bolívia e Equador já haviam firmado Pactos de Não Agressão e Neutralidade em 1911. Apenas atualizamos os valores para 1942 com o valor da Entente. 
alliance_42_clean <- alliance_42_clean %>%
  select(state_name1, state_name2, Weight) %>%
  mutate(Weight = ifelse(state_name1 == "Ecuador" & state_name2 == "Bolivia", 5, Weight))

##Equador e Bolivia tinham entradas duplicadas. Excluímos a primeira delas. 
alliance_42_clean <- alliance_42_clean[-2,]

##A partir dos dados carregados e limpos crie um novo objeto, o grafo. 
##Use o comando 'graph_from_data_frame' ou 'graph_from_edgelist(as.matrix)'. 
## O 1º comando terá apenas dois argumentos: 1) o nome da base de dados, definida no passo anterior; 2) o tipo de rede, neste caso 'directed = False' por ser uma rede Indireta. 
alliance_42_clean_grafo <- graph_from_data_frame(alliance_42_clean, directed = FALSE)

##Podemos colocar os pesos nas arestas manualment utilizando a função 'set_edge_attr'. Precisamso definir: 1 a base;2 qual atributo, nesse caso o peso, e 3 - da onde virão os valores, nesse caso da nossa coluna Weight.
alliance_42_clean_grafo <- set_edge_attr(alliance_42_clean_grafo, "weight", value= alliance_42_clean$Weight)
is_weighted(alliance_42_clean_grafo)


###Plotando a nossa rede social
##Podemos usar o comando 'plot' porém ele dificulta a viz de grandes redes ou podemos usar o 'tkplot' que permite-nos mexer nos objetos e visualisa-los usandos algoritmos.
edge_width_42 <- alliance_42_clean$Weight
tkplot (as.undirected(alliance_42_clean_grafo),
        layout = layout_with_kk(alliance_42_clean_grafo),
        canvas.height = 500,
        canvas.width = 1000,
        vertex.size = 15,
        vertex.label.dist = 0, vertex.label.color = "darkblue",
        vertex.label.cex = 0.8, vertex.label.family = "sans",
        vertex.shapes="none", vertex.color = ifelse(V(alliance_42_clean_grafo)$name == 'Brazil', "red", "#B0C4DE"),
        edge.curved = FALSE, edge.width = edge_width_42, edge.label = alliance_42_clean$Weight, edge.color = 'grey', edge.label.color = 'black')

###Podemos tirar algumas medidas descritivas
#metricas dos nós, temos que armazenar as medidas como atributos. É importante notar que as métricas levam em consideração os pesos dos links.
V(alliance_42_clean_grafo)$Degree_Ponderado <- strength(alliance_42_clean_grafo, weights = E(alliance_42_clean_grafo)$weight)
V(alliance_42_clean_grafo)$Degree <- degree(alliance_42_clean_grafo)
V(alliance_42_clean_grafo)$Closeness_Ponderado <- closeness(alliance_42_clean_grafo, mode = "all", weights = E(alliance_42_clean_grafo)$weight, normalized=T)
V(alliance_42_clean_grafo)$Betweenneess_Ponderado <- betweenness(alliance_42_clean_grafo, directed=F, weights = E(alliance_42_clean_grafo)$weight)
V(alliance_42_clean_grafo)$Excentricidade <-eccentricity(alliance_42_clean_grafo)
V(alliance_42_clean_grafo)$Bridge <- 0
V(alliance_42_clean_grafo)$Bridge[articulation_points(alliance_42_clean_grafo) ] <- 1
V(alliance_42_clean_grafo)$Clustering_Coefficient <- transitivity(alliance_42_clean_grafo, type = "local")

#Armazenando todas as medidas e transformando em data frame
tabela_nos_aliança_42 <- igraph::as_data_frame(alliance_42_clean_grafo, what="vertices")

#Criar um objeto para filtrar apenas os Estados americanos pois as alianças formam uma rede com dois componenetes que não conversam.
Metricas_Americas <- c("Brazil", "United States of America", "Cuba", "Haiti", "Dominican Republic", 
                       "Mexico", "Guatemala", "Honduras", "El Salvador", "Nicaragua", 
                       "Costa Rica", "Panama", "Colombia", "Venezuela", "Peru", 
                       "Bolivia", "Paraguay", "Chile", "Argentina", "Uruguay", "Ecuador")

#Filtar a tabela usando o objeto criado contendo apenas os países das Américas.
#Renomeando a primeira coluna da tabela usando 'rename'.
#Arrendondando os valores da tabela para 4 casas decimais usando o mutate
tabela_nos_aliança_42_filtrada <- tabela_nos_aliança_42 %>% 
  filter(name %in% Metricas_Americas) %>%
  rename(País = name) %>%
  mutate(
    Degree_Ponderado = round(Degree_Ponderado, 4),
    Degree = round(Degree, 4),
    Closeness_Ponderado = round(Closeness_Ponderado, 4),
    Betweenneess_Ponderado = round(Betweenneess_Ponderado, 4),
    Excentricidade = round(Excentricidade, 4),
    Bridge = round(Bridge, 4))

--------------------------------------------------------------1947---------------------------------------------------------------  
### Faça o Data Wrangling inicial
##Neste caso eliminamos as colunas não necessárias usando o comando 'select' para selecionar apenas as colunas que queremos.
##Depois, na mesma expressão, usamos o comando 'filter' para filtrar as linhas que queremos. Neste caso queremos as linhas que sejam iguais a "1936" pois é nosso ano de interesse.
##Por fim, na mesma expressão, usamos o comando 'mutate' para transformar os dados da forma que queremos.
## IMPORTANTE: os comandos devem ser separados por pipe (%>%) para que o R execute tudo de uma vez no mesmo objeto.
alliance_47_clean <- (alliance_v4_1_by_dyad_yearly %>%
                          select(state_name1, state_name2, year, defense, neutrality, nonaggression, entente) %>%
                          filter(year == '1947') %>%
                          mutate(defense = replace(defense, defense==1, "6"),
                                 neutrality = replace(neutrality, neutrality==1, "2"),
                                 nonaggression = replace(nonaggression, nonaggression==1, "2")))

##Transforme as colunas defense, neutrality, nonaggression, entente em número pois elas estão em formato string. Use o 'mutate = as.numeric' para fazer isso.
alliance_47_clean <- alliance_47_clean %>%
  mutate(
    defense = as.numeric(defense),
    neutrality = as.numeric(neutrality), 
    nonaggression = as.numeric(nonaggression),
    entente = as.numeric(entente)
  )

##Adicionamos um nova coluna 'Weight' ao dataset e somamos os pesos dados a cada tipo de aliança (defense, neutrality, nonaggression, entente).
##Cada tipo de aliança tem um peso diferente com base na intensidade do compromisso militar.
alliance_47_clean <- alliance_47_clean %>%
  mutate(Weight = defense + neutrality + nonaggression + entente)

##Excluímos as colunas dyad_st_year, defense, neutrality, nonaggression, entente e ficamos só com os pesos na coluna 'Weight'.
##Bolívia e Equador já haviam firmado Pactos de Não Agressão e Neutralidade em 1911. Apenas atualizamos os valores para 1947 com o valor da Entente e do Pacto de Defesa. 
alliance_47_clean <- alliance_47_clean %>%
  select(state_name1, state_name2, Weight) %>%
  mutate(Weight = ifelse(state_name1 == "Ecuador" & state_name2 == "Bolivia", 11, Weight))

##Equador e Bolivia tinham entradas triplicadas Excluímos a primeira e a segunda delas. 
alliance_47_clean <- alliance_47_clean[-2,]
alliance_47_clean <- alliance_47_clean[-189, ]

##A partir dos dados carregados e limpos crie um novo objeto, o grafo. 
##Use o comando 'graph_from_data_frame' ou 'graph_from_edgelist(as.matrix)'. 
## O 1º comando terá apenas dois argumentos: 1) o nome da base de dados, definida no passo anterior; 2) o tipo de rede, neste caso 'directed = False' por ser uma rede Indireta. 
alliance_47_clean_grafo <- graph_from_data_frame(alliance_47_clean, directed = FALSE)

##Podemos colocar os pesos nas arestas manualment utilizando a função 'set_edge_attr'. Precisamso definir: 1 a base;2 qual atributo, nesse caso o peso, e 3 - da onde virão os valores, nesse caso da nossa coluna Weight.
alliance_47_clean_grafo <- set_edge_attr(alliance_47_clean_grafo, "weight", value= alliance_47_clean$Weight)


###Plotando a nossa rede social
##Podemos usar o comando 'plot' porém ele dificulta a viz de grandes redes ou podemos usar o 'tkplot' que permite-nos mexer nos objetos e visualisa-los usandos algoritmos.
edge_width_47 <- alliance_47_clean$Weight
tkplot (as.undirected(alliance_47_clean_grafo),
        layout = layout_with_kk(alliance_47_clean_grafo),
        canvas.height = 250,
        canvas.width = 1000,
        vertex.size = 10,
        vertex.label.dist = 0, vertex.label.color = "darkblue",
        vertex.label.cex = 0.8, vertex.label.family = "sans",
        vertex.shapes="none", vertex.color = ifelse(V(alliance_47_clean_grafo)$name == 'Brazil', "red", "#B0C4DE"),
        edge.curved = FALSE, edge.width = edge_width_47, edge.label = alliance_47_clean$Weight, edge.color = 'grey', edge.label.color = 'black')

###Podemos tirar algumas medidas descritivas
#metricas dos nós, temos que armazenar as medidas como atributos. É importante notar que as métricas levam em consideração os pesos dos links.
V(alliance_47_clean_grafo)$Degree_Ponderado <- strength(alliance_47_clean_grafo, weights = E(alliance_47_clean_grafo)$weight)
V(alliance_47_clean_grafo)$Degree <- degree(alliance_47_clean_grafo)
V(alliance_47_clean_grafo)$Closeness_Ponderado <- closeness(alliance_47_clean_grafo, mode = "all", weights = E(alliance_47_clean_grafo)$weight, normalized=T)
V(alliance_47_clean_grafo)$Betweenneess_Ponderado <- betweenness(alliance_47_clean_grafo, directed=F, weights = E(alliance_47_clean_grafo)$weight)
V(alliance_47_clean_grafo)$Excentricidade <-eccentricity(alliance_47_clean_grafo)
V(alliance_47_clean_grafo)$Bridge <- 0
V(alliance_47_clean_grafo)$Bridge[articulation_points(alliance_47_clean_grafo) ] <- 1
V(alliance_47_clean_grafo)$Clustering_Coefficient <- transitivity(alliance_47_clean_grafo, type = "local")

#Armazenando todas as medidas e transformando em data frame
tabela_nos_aliança_47 <- igraph::as_data_frame(alliance_47_clean_grafo, what="vertices")

#Criar um objeto para filtrar apenas os Estados americanos pois as alianças formam uma rede com dois componenetes que não conversam.
Metricas_Americas <- c("Brazil", "United States of America", "Cuba", "Haiti", "Dominican Republic", 
                       "Mexico", "Guatemala", "Honduras", "El Salvador", "Nicaragua", 
                       "Costa Rica", "Panama", "Colombia", "Venezuela", "Peru", 
                       "Bolivia", "Paraguay", "Chile", "Argentina", "Uruguay", "Ecuador")

#Filtar a tabela usando o objeto criado contendo apenas os países das Américas.
#Renomeando a primeira coluna da tabela usando 'rename'.
#Arrendondando os valores da tabela para 4 casas decimais usando o mutate
tabela_nos_aliança_47_filtrada <- tabela_nos_aliança_47 %>% 
  filter(name %in% Metricas_Americas) %>%
  rename(País = name) %>%
  mutate(
    Degree_Ponderado = round(Degree_Ponderado, 4),
    Degree = round(Degree, 4),
    Closeness_Ponderado = round(Closeness_Ponderado, 4),
    Betweenneess_Ponderado = round(Betweenneess_Ponderado, 4),
    Excentricidade = round(Excentricidade, 4),
    Bridge = round(Bridge, 4))
#------------------------------------------------------------------------------------FIM------------------------------------------------------------------------