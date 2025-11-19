install.packages("igraph")
library(igraph)

#Leitura dos dados e contrução do grafo
dados <- read.table("C:/Users/GuiPrs335PC/RstudioFiles/trab.txt")
g <- graph_from_data_frame(dados, directed = FALSE)
g

ola <- "eu sou o daniel!"

# Parte I

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

#h) "Tendo em conta o contexto, identifique algumas características/o papel de alguns dos presentes na festa. Justifique."

