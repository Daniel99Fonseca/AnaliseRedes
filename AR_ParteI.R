install.packages("igraph")
library(igraph)

#Leitura dos dados e contrução do grafo
dados <- read.table("trab.txt")
g <- graph_from_data_frame(dados, directed = FALSE)
g

<<<<<<< HEAD
=======
"Eu sou gui"
ola <- "eu sou o daniel"
joao <- 13
"É preciso fazer push?"
>>>>>>> d75e878c2ff8b194cd4aeb424ffd1733eac717be

## Parte I


#a)

# Dimensão/Número de nodos -> 113
N_nod <- vcount(g)

# Número de ligações -> 2196
N_lig  <- ecount(g)

# Densidade -> 0.3470291 -> relativamente densa
den <- edge_density(g)


#b)

# Grau e estatísticas
grau <- degree(g)
mean(grau); median(grau); quantile(grau)

# Estatísticas
# Média -> 38.86726
# Mediana -> 37
# Quantis -> Q1:37 ; Q2:37 ; Q3:50

# Distribuição ????????

# Heterogeneidade -> 1.222905 -> Ligeiramente Heterogénia
mean(grau^2)/mean(grau)^2

# Existência de hubs
# Podemos concluir que existem hubs moderados mas nada extremo


#c)

# Associação de Grau -> -0.1225804 -> Ligeiramente Disassortativa (nodos de grau elevado tendem a ligar-se a nodos de grau baixo)
assortativity_degree(g)


#d)

# Distância entre nodos -> 1.65629 -> a maioria dos nodos está ligada diretamente ou por um intermediário (raramente por 2 intermediários)
mean_distance(g)


#e)

# Clustering global -> 0.4952046 -> triângulos são frequentes
transitivity(g, type="global")

# Clustering Local ?????????
transitivity(g, type="local") # n sei interpretar


#f)

# Core -> núcleos/centros das ligações (acho eu)
core <- coreness(g)
table(core)


#g) "Elabore um pequeno texto com comentários adicionais que considere pertinentes."

#1 A rede estudada apresenta um conjunto de nodos que ajudam a caracterizar a dinâmica social da festa.
#2 É uma rede densa (elevada conecção entre nodos).
#3 É globalmente conectada e compacta (distância reduzida entre nodos).
#4 Possui clustering elevado (abundância de triângulos).
#5 É ligeiramente heterogénia (existência de alguns nodos com grau muito superior à média).
#6 É dissortativa (tendência para nodos de grau elevado).
#7 No conjunto, trata-se de uma rede altamente conectada, com hubs moderados,
#7 forte coesão local e grande eficiência global.

#h) "Tendo em conta o contexto, identifique algumas características/o papel de alguns dos presentes na festa. Justifique."

#1 Os nodos com grau mais  elevado representão as pessoas na festa mais sociáveis .
#1 (possivelmente anfitriões). Estes individuos têm múltiplas ligações entre si e 
#1 integram vários triângulos.
#2 Existem nodos que se destacam pela 'betweenness' desempenham o papel de conectores sociais entre grupos.
#3 Os graus dos nodos das pessoas na festa sugerem participação social regular.
#4 Os nodos de grau mais baixo, representão pessoas com menor participação social
#4 (possivelmente chegaram atrasados ou não são tão sociáveis).
#5 A rede apresenta elevada conectividade global, forte coesão local e uma estrutura
#5 social típica de eventos com vários grupos de amigos. Conseguimos observar 
#5 papéis sociais na rede dependendo do grau dos nodos e conectividade entre nodos.