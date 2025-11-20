# install.packages("igraph")
library(igraph)

# Leitura dos dados e construção do grafo
dados <- read.table("trab.txt", header = FALSE)
g_festa <- graph_from_data_frame(dados, directed = FALSE)
g_festa

#############
## Parte I ##
#############


#a) Indique a dimensão e o número de ligações da rede. 
# Determine a densidade e classifique a rede.

# Dimensão / Número de nodos -> 113
n_nod_festa <- vcount(g_festa)
n_nod_festa

# Número de ligações -> 2196
n_lig_festa <- ecount(g_festa)
n_lig_festa 

# Densidade -> 0.3470291 -> relativamente densa
densidade_festa <- edge_density(g_festa)
densidade_festa

# b) Obtenha o grau médio e a distribuição de grau. 
# Caracterize a distribuição de grau (mediana, quartis, etc.) e comente. 
# Calcule o parâmetro de heterogeneidade. Indique o 
# que pode concluir quanto à existência de hubs.

# Grau e estatísticas
graus_festa <- degree(g_festa)
summary(graus_festa)

min_festa <- min(graus_festa)
max_festa <- max(graus_festa)
median_festa <- median(graus_festa)
media_festa <- mean(graus_festa)
q1_festa <- quantile(graus_festa,0.25)
q3_festa <- quantile(graus_festa,0.75)

# Distribuição
dist_a <- hist(graus_festa, breaks = 20, col = "lightblue", main = "Distribuição de Grau na festa",
     xlab = "Número de Interações", ylab = "Frequência")

# Heterogeneidade -> 1.222905 -> Ligeiramente Heterogénia
heterogeneity_festa <- mean(graus_festa^2)/mean(graus_festa)^2
heterogeneity_festa

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


layout <- layout_with_fr(rede, niter = 2000)
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
# o diâmetro da rede. Indique o que pode concluir-se quanto à distância média;

avg_path <- mean_distance(rede, directed = FALSE)
avg_path

diam <- diameter(rede, directed = FALSE)
diam

# vii) - determine os coeficientes de clustering da rede. 
# Diga o que pode concluir-se quanto à existência de triângulos.
clust_media <- transitivity(rede, type = "average")
clust_media