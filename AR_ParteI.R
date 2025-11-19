# install.packages("igraph")
library(igraph)

# Leitura dos dados e construção do grafo
dados <- read.table("trab.txt")
g <- graph_from_data_frame(dados, directed = FALSE)
g

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



### Parte II

############
#### Q1 ####
############

# Modelo Preferential-Attachment
# n(odes) = 500 nodos
# e(dges) = 1000 ligacoes
set.seed(5158)
rede <- sample_pa(n = 500, power = 1, m = 2, directed = FALSE)

ecount(rede)

vcount(rede)

plot(rede, vertex.size = 5, vertex.label = NA, edge.color = "grey")

# i) determine a densidade e classifique a rede;
# Bastante esparsa com densidade 0.008
densidade_pa <- edge_density(rede)

# ii) indique se a rede é conexa;
# TRUE
is_connected(rede)

# iii) - obtenha o grau médio e a distribuição de grau.
# Caracterize a distribuição de grau (mediana, quartis, etc.) e comente;
graus <- degree(rede)

avg_grau <- mean(graus)
summary(graus)

# Difícil visualização
hist(graus, breaks = 30, col = "skyblue",
     main = "Distribuição de Graus",
     xlab = "Grau (número de ligações)",
     ylab = "Número de nodos")

# Calcular distribuição acumulada
graus_sorted <- sort(graus)
cdf <- ecdf(graus_sorted)

# Plot CDF
plot(cdf, main = "Distribuição acumulada de grau",
     xlab = "Grau", ylab = "Fração de nodos ≤ k",
     verticals = TRUE, do.points = FALSE)


# iv) - calcule o parâmetro de heterogeneidade. 
# Indique o que pode concluir quanto à existência de hubs;
grau_medio <- mean(graus)
grau2_medio <- mean(graus^2)

H <- grau2_medio / (grau_medio^2) # H = 2.13!

Q3 <- quantile(graus, 0.75)
sum(graus > Q3)        # número de nodos acima de Q3
sum(graus > Q3) / length(graus) * 100  # percentual da rede

sum(graus > 10)
sum(graus > 10) / length(graus) * 100
hubs <- graus > 10


layout <- layout_with_fr(rede, niter = 2000, area = vcount(rede)^2 * 50)
vertex_sizes <- pmin(graus + 2, 6)  # limita tamanho máximo
plot(rede, layout = layout,
     vertex.size = vertex_sizes,
     vertex.color = ifelse(graus > quantile(graus, 0.75), "red", "skyblue"),
     vertex.label = NA,
     edge.color = "grey",
     margin = 0)

# v) estude a associação de grau e indique o que poderá concluir-se;
assor <- assortativity_degree(rede, directed=FALSE)
assor # r < 0 é disassociativa, logo os hubs não estão conetados entre si.

# vi) - determine a média dos comprimentos dos caminhos mais curtos e 
# o diâmetro da rede. # Indique o que pode concluir-se quanto à distância média;

avg_path <- mean_distance(rede, directed = FALSE)
avg_path

diam <- diameter(rede, directed = FALSE)
diam

# vii) - determine os coeficientes de clustering da rede. 
# Diga o que pode concluir-se quanto à existência de triângulos.
clust_media <- transitivity(rede, type = "average")
clust_media







