# install.packages("igraph")
# install.packages("kableExtra")
library(igraph)

# Leitura dos dados e construção do grafo
dados <- read.table("trab.txt", header = FALSE)
g_festa <- graph_from_data_frame(dados, directed = FALSE)
g_festa

################################# Parte I ##############################

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

# Heterogeneidade -> 1.222905 -> Ligeiramente Heterogénia
heterogeneity_festa <- mean(graus_festa^2)/mean(graus_festa)^2
heterogeneity_festa

#c) Estude a associação de grau e indique o que poderá concluir-se.

assortativity_degree(g_festa)

knn_vals <- knn(g_festa)$knn

nos_grau1 <- which(graus_festa == 1)
nos_grau1

# d) Determine a média dos comprimentos dos caminhos mais curtos.
# Indique o que pode concluir-se quanto à distância média.

dist_festa <- distances(g_festa)
mean_dist_festa <- mean(dist_festa)
mean_dist_festa

# e) Determine os coeficientes de clustering dos nodos e da rede. 
# Diga o que pode concluir-se quanto à existência de triângulos.

# Clustering global
clust_global_festa <- transitivity(g_festa, type="global")
clust_global_festa

# Clustering Local
clust_local_festa <- transitivity(g_festa, type = "local", isolates = "zero")
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

bet <- betweenness(g_festa)
cls <- closeness(g_festa)
lc <- cluster_louvain(g_festa)

nodo_top <- which.max(bet)

membership_lc <- membership(lc)


top_hubs <- names(sort(graus_festa, decreasing = TRUE)[1:5])
top_hubs
membership_lc[top_hubs]


############################ PARTE II #######################################
############
#### Q1 #### seed <- 5158 (aluno Daniel Fonseca n. 125158)
############

set.seed(5158)
rede_q1 <- sample_pa(n = 500, m = 2, directed = FALSE)

ecount(rede_q1)

vcount(rede_q1)

# i) Determine a densidade e classifique a rede;
densidade_pa <- edge_density(rede_q1)
densidade_pa

# ii) Indique se a rede é conexa;
is_connected(rede_q1)

# iii) - Obtenha o grau médio e a distribuição de grau.
# Caracterize a distribuição de grau (mediana, quartis, etc.) e comente;
graus_q1 <- degree(rede_q1)

avg_grau <- mean(graus_q1)
summary(graus_q1)

# Difícil visualização
hist(graus_q1, breaks = 20, col = "skyblue",
     main = "Distribuição de Graus",
     xlab = "Grau (número de ligações)",
     ylab = "Número de nodos")

# Calcular distribuição acumulada
graus_sorted <- sort(graus_q1)
cdf <- ecdf(graus_sorted)


# iv) - Calcule o parâmetro de heterogeneidade. 
# Indique o que pode concluir quanto à existência de hubs;
grau_medio <- mean(graus_q1)
grau2_medio <- mean(graus_q1^2)

H <- grau2_medio / (grau_medio^2)
H

# v) Estude a associação de grau e indique o que poderá concluir-se;

assor <- assortativity_degree(rede_q1, directed=FALSE)
assor

knn_vals_q1 <- knn(rede_q1)$knn

# vi) - Determine a média dos comprimentos dos caminhos mais curtos e 
# o diâmetro da rede. Indique o que pode concluir-se quanto à distância média;

avg_path <- mean_distance(rede_q1, directed = FALSE)
avg_path

diam <- diameter(rede_q1, directed = FALSE)
diam

# vii) - Determine os coeficientes de clustering da rede. 
# Diga o que pode concluir-se quanto à existência de triângulos.

trans_global <- transitivity(rede_q1, type = "global") 
trans_global

# Coeficiente local
clust_local <- transitivity(rede_q1, type = "local")

clust_media <- transitivity(rede_q1, type = "average")
clust_media

# Percentagem de nodos com clustering > 0
pct_with_triangles <- sum(clust_local > 0) / length(clust_local) * 100
pct_with_triangles

############
#### Q2 #### seed <- 5158 (aluno Daniel Fonseca n. 125158)
############

prob_rem <- 0.1 

rem_func <- runif(ecount(rede_q1)) < prob_rem 

nod_rem <- E(rede_q1)[rem_func] 

rede_q2 <- delete_edges(rede_q1, nod_rem) 


ecount(rede_q1)
ecount(rede_q2)

# plot(rede_q2, vertex.size = 5, vertex.label = NA, edge.color = "lightblue")

# i) - Indique o número de ligações removidas;
rem_nod <- ecount(rede_q1) - ecount(rede_q2)
rem_nod

# ii) - Determine a densidade e classifique a rede;
densidade_q2 <- edge_density(rede_q2)

# iii) - Indique se a rede é conexa;
is_connected(rede_q2)

# iv) - Obtenha o grau médio e a distribuição de grau.
#  Caracterize a distribuição de grau (mediana, quartis, etc.) e comente;
graus_q2 <- degree(rede_q2)

avg_grau <- mean(graus_q2)
summary(graus_q2)

# v) - Calcule o parâmetro de heterogeneidade. 
# Indique o que pode concluir quanto à existência de hubs;
grau_medio_q2 <- mean(graus_q2)
grau2_medio_q2 <- mean(graus_q2^2)

H_q2 <- grau2_medio_q2 / (grau_medio_q2^2)
H_q2

# Calcular distribuição acumulada
graus_sorted_q2 <- sort(graus_q2)
cdf_q2 <- ecdf(graus_sorted_q2)

# Plot CDF
plot(cdf_q2, main = "Distribuição acumulada de grau",
     xlab = "Grau", ylab = "Fração de nodos ≤ k",
     verticals = TRUE, do.points = FALSE)

# vi) - Estude a associação de grau e indique o que poderá concluir-se;
assor_q2 <- assortativity_degree(rede_q2, directed=FALSE)
assor_q2


# vii) - Determine a média dos comprimentos dos caminhos mais curtos e o 
# diâmetro da rede. Indique o que pode concluir-se quanto à distância média;
avg_path_q2 <- mean_distance(rede_q2, directed = FALSE)
avg_path_q2

diam_q2 <- diameter(rede_q2, directed = FALSE)
diam_q2

# viii) - Determine o coeficiente de clustering da rede. 
# Diga o que pode concluir-se quanto à existência de triângulos;
clust_global_q2 <- transitivity(rede_q2, type = "global")
clust_global_q2

# Coeficiente local
clust_local_q2 <- transitivity(rede_q2, type = "local")

clust_media_q2 <- transitivity(rede_q2, type = "average")
clust_media_q2

# Percentagem de nodos com clustering > 0
pct_with_triangles_q2 <- sum(clust_local_q2 > 0) / length(clust_local_q2) * 100
pct_with_triangles_q2

# ix) - Compare os resultados obtidos antes e após a remoção de ligações;

x<-c("rede Q1","rede Q2","rede Q1","rede Q2","rede Q1","rede Q2","rede Q1","rede Q2","rede Q1","rede Q2")
y<-c("Densidade","Densidade","Conexa","Conexa","Grau Médio","Grau Médio","Min","Min","Max","Max")
valores <- c(0.008,0.007,"Sim","Não",3.99,3.56,2,0,47,41)
y <- factor(y, levels = unique(y))
Tabela1 <- tapply(valores, list(x, y), function(z) as.character(z))
Tabela1 <- as.matrix(Tabela1)
Tabela1

s<-c("rede Q1","rede Q2","rede Q1","rede Q2","rede Q1","rede Q2","rede Q1","rede Q2","rede Q1","rede Q2")
t<-c("Mediana","Mediana","Heterogeneidade","Heterogeneidade","Associação de grau","Associação de grau","Diâmetro","Diâmetro","Coeficiente de clustering","Coeficiente de clustering")
va<-c(3,2,2.13,2.15,-0.064,-0.057,8,9,0.038,0.021)
t <- factor(t, levels = unique(t))
Tabela2 <- tapply(va, list(s, t), function(z) as.character(z))
Tabela2 <- as.matrix(Tabela2)
Tabela2