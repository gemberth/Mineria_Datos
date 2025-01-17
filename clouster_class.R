###Ejemplo de cluster: Jerarquicos y Kmeans
##Segmentar clientes (gasto en leche, comestibles y alimentos congelados
#carga de datos:
customers_spend <- read.csv("F:/OneDrive - ULEAM/6to-Semestre/Mineria de datos/codigos_repo/customers_spend.csv", sep=";")
View(customers_spend)
colMeans(customers_spend)
colMeans(scale(customers_spend))

# Calculate Euclidean distance between customers
dist_customers <- dist(scale(customers_spend))

# Generate a complete linkage analysis 
hc_customers <- hclust(dist_customers, method = "complete")

# Plot the dendrogram
plot(hc_customers)
abline(h=4)
###
# Create a cluster assignment vector at h = 4
clust_customers <- cutree(hc_customers, h = 4)
table(clust_customers)
########################## con kmeans
km_customers <- kmeans(scale(customers_spend), centers = 3,
                       nstart = 20, iter.max = 50)
km_customers
km_customers$tot.withinss
##########################
# Compare methods
table(km_customers$cluster, clust_customers)

##################
# Generate the segmented customers data frame
segment_customers <- cbind(customers_spend, cluster = clust_customers)
View(segment_customers)
######################3
# Calcule la media para cada categor�a
library(dplyr)
segment_customers %>% 
  group_by(cluster) %>% 
  summarise_all(mean)
########################
