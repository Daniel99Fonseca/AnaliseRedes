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
hist(graus_festa, breaks = 20, col = "lightblue", main = "Distribuição de Grau na festa",
     xlab = "Número de Interações", ylab = "Frequência")

# Heterogeneidade -> 1.222905 -> Ligeiramente Heterogénia
heterogeneity_festa <- mean(graus_festa^2)/mean(graus_festa)^2
heterogeneity_festa

#c) Estude a associação de grau e indique o que poderá concluir-se.

# Associação de Grau -> -0.1225804
assortativity_degree(g_festa)

# d) Determine a média dos comprimentos dos caminhos mais curtos.
# Indique o que pode concluir-se quanto à distância média.

# Distância entre nodos 
dist_festa <- distances(g_festa)
mean_dist_festa <- mean(dist)
mean_dist_festa

#e) Determine os coeficientes de clustering dos nodos e da rede. 
# Diga o que pode concluir-se quanto à existência de triângulos.

# Clustering global -> 0.4952046 -> triângulos são frequentes
clust_global_festa <- transitivity(g_festa, type="global")
clust_global_festa

# Clustering Local
clust_local_festa <- transitivity(g_festa, type = "local", isolates = "zero")
# isolates = "zero" remove nodos com graus 0 e 1.
clust_local_festa
mean(clust_local_festa)

# f) Efetue a decomposição de core da rede. Indique o número de conchas 
# existentes e a dimensão de cada uma. Indique o que poderá concluir-se.

# Core -> núcleos/centros das ligações
core <- coreness(g_festa)
num_conchas <- length(unique(core))
num_conchas

dim_conchas <- table(core)
dim_conchas

# g) "Elabore um pequeno texto com comentários adicionais que considere pertinentes."

#1 A rede estudada apresenta um conjunto de nodos que ajudam a caracterizar a dinâmica social da festa.
#2 É uma rede densa (elevada conecção entre nodos).
#3 É globalmente conectada e compacta (distância reduzida entre nodos).
#4 Possui clustering elevado (abundância de triângulos).
#5 É ligeiramente heterogénia (existência de alguns nodos com grau muito superior à média).
#6 É dissortativa (tendência para nodos de grau elevado).
#7 No conjunto, trata-se de uma rede altamente conectada, com hubs moderados,
#7 forte coesão local e grande eficiência global.

# h) "Tendo em conta o contexto, identifique algumas características/o papel de alguns dos presentes na festa. Justifique."

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
#### Q1 #### seed <- 5158
############

# Modelo Preferential-Attachment
# n(odes) = 500 nodos
# e(dges) = 1000 ligacoes
set.seed(5158)
rede_q1 <- sample_pa(n = 500, power = 1, m = 2, directed = FALSE)

ecount(rede_q1)

vcount(rede_q1)

plot(rede_q1, vertex.size = 5, vertex.label = NA, edge.color = "grey")

# i) determine a densidade e classifique a rede;
# Bastante esparsa com densidade 0.008
densidade_pa <- edge_density(rede_q1)

# ii) indique se a rede é conexa;
# TRUE
is_connected(rede_q1)

# iii) - obtenha o grau médio e a distribuição de grau.
# Caracterize a distribuição de grau (mediana, quartis, etc.) e comente;
graus_q1 <- degree(rede_q1)

avg_grau <- mean(graus_q1)
summary(graus_q1)

# Difícil visualização
hist(graus_q1, breaks = 30, col = "skyblue",
     main = "Distribuição de Graus",
     xlab = "Grau (número de ligações)",
     ylab = "Número de nodos")

# Calcular distribuição acumulada
graus_sorted <- sort(graus_q1)
cdf <- ecdf(graus_sorted)

# Plot CDF
plot(cdf, main = "Distribuição acumulada de grau",
     xlab = "Grau", ylab = "Fração de nodos ≤ k",
     verticals = TRUE, do.points = FALSE)


# iv) - calcule o parâmetro de heterogeneidade. 
# Indique o que pode concluir quanto à existência de hubs;
grau_medio <- mean(graus_q1)
grau2_medio <- mean(graus_q1^2)

H <- grau2_medio / (grau_medio^2) # H = 2.13!

Q3 <- quantile(graus_q1, 0.75)
sum(graus_q1 > Q3)        # número de nodos acima de Q3
sum(graus_q1 > Q3) / length(graus_q1) * 100  # percentual da rede

sum(graus_q1 > 10)
sum(graus_q1 > 10) / length(graus_q1) * 100
hubs <- graus_q1 > 10


layout <- layout_with_fr(rede_q1, niter = 2000)
vertex_sizes <- pmin(graus_q1 + 2, 6)  # limita tamanho máximo
plot(rede_q1, layout = layout,
     vertex.size = vertex_sizes,
     vertex.color = ifelse(graus_q1 > quantile(graus_q1, 0.75), "red", "skyblue"),
     vertex.label = NA,
     edge.color = "grey",
     margin = 0)

# v) estude a associação de grau e indique o que poderá concluir-se;

assor <- assortativity_degree(rede_q1, directed=FALSE)
assor # r < 0 é disassociativa, logo os hubs não estão conetados entre si.

# vi) - determine a média dos comprimentos dos caminhos mais curtos e 
# o diâmetro da rede. Indique o que pode concluir-se quanto à distância média;

avg_path <- mean_distance(rede_q1, directed = FALSE)
avg_path

diam <- diameter(rede_q1, directed = FALSE)
diam

# vii) - determine os coeficientes de clustering da rede. 
# Diga o que pode concluir-se quanto à existência de triângulos.
clust_media <- transitivity(rede_q1, type = "average")
clust_media

##########
####Q2#### seed <- 5158
##########

prob_rem <- 0.1 #probabilidade do nodo ser removido

rem_func <- runif(ecount(rede_q1)) < prob_rem #escolhe os nodos a ser removidos (lista de [TRUE, FALSE, TRUE, TRUE...])

nod_rem <- E(rede_q1)[rem_func] #guarda os nodos removidos

rede_q2 <- delete_edges(rede_q1, nod_rem) #remover os nodos selecionados aleatóriamente da rede

ecount(rede_q1) #997 ligações
ecount(rede_q2) #895 ligações

plot(rede_q2, vertex.size = 5, vertex.label = NA, edge.color = "lightblue")

# i) indique o número de ligações removidas;
# 89 ligações removidas
rem_nod <- ecount(rede_q1) - ecount(rede_q2) 

# ii) determine a densidade e classifique a rede;
# Bastante esparsa com densidade 0.007 (Mais esparsa que rede_q1)
densidade_q2 <- edge_density(rede_q2)

# iii) indique se a rede é conexa;
# FALSE -> existem nodos isolados (1)
is_connected(rede_q2)

# iv) obtenha o grau médio e a distribuição de grau.
#  Caracterize a distribuição de grau (mediana, quartis, etc.) e comente; -> no relatório
graus_q2 <- degree(rede_q2)

avg_grau <- mean(graus_q2)
summary(graus_q2)

# v) calcule o parâmetro de heterogeneidade. Indique o que pode concluir quanto à existência de hubs;
grau_medio_q2 <- mean(graus_q2)
grau2_medio_q2 <- mean(graus_q2^2)

H_q2 <- grau2_medio / (grau_medio^2) # H <- 2.01
H_q2

# Calcular distribuição acumulada
graus_sorted_q2 <- sort(graus_q2)
cdf_q2 <- ecdf(graus_sorted_q2)

# Plot CDF
plot(cdf_q2, main = "Distribuição acumulada de grau",
     xlab = "Grau", ylab = "Fração de nodos ≤ k",
     verticals = TRUE, do.points = FALSE)

# vi) estude a associação de grau e indique o que poderá concluir-se;
assor_q2 <- assortativity_degree(rede_q2, directed=FALSE)
assor_q2 # r < 0 é disassociativa, logo os hubs não estão conetados entre si.

# vii)  determine a média dos comprimentos dos caminhos mais curtos e o diâmetro da rede. Indique o que pode concluir-se quanto à distância média;
avg_path_q2 <- mean_distance(rede_q2, directed = FALSE)
avg_path_q2

diam_q2 <- diameter(rede_q2, directed = FALSE)
diam_q2

# viii) determine o coeficiente de clustering da rede. Diga o que pode concluir-se quanto à existência de triângulos;
clust_media_q2 <- transitivity(rede_q2, type = "average")
clust_media_q2

# ix) compare os resultados obtidos antes e após a remoção de ligações;

#i) aumento de esparsidade 0.008 -> 0.007

#ii) deixa de ser conectada -> 1 nodo passou a ser isolado

#iii) Min. 1st Qu.  Median    Mean 3rd Qu.    Max.  -> Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#    2.000   2.000   3.000   3.988   4.000  33.000     0.000   2.000   2.000   3.632   4.000  27.000

#iv) Heterogenidade_q1 <- 2.13 => Heterogenidade_q2 <- 2.01 
# graus com mais ligações têm mais chance de perder ligações quando removemos nodos aleatóriamente, logo menor desigualdade de ligações entre nodos

#v) Assortatividade_q1 <- -0.115 => Assortatividade_q2 <- -0.146
# a rede passou a ter menor tendência para conectar nós com graus semelhantes

#vi) aumento da distância média e do diâmetro da rede devido à remoção de "atalhos"

#vii) a clustering média diminuiu devido à diminução de nodos que resultou na diminuição de triângulos
