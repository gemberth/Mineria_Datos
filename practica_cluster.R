
#::::::::::::::::::  K-MEANS  ::::::::::::::::::::::::::

library(ggplot2)

#1. Considerar el conjunto de puntos siguientes:
#  (1.0; 2.0) (0.9; 0.5) (2.0; 5.0) (2.1; 4.5) (3.1; 3.2) (0.9; 1.3)

#Importacion de los datos 
datos <- read.csv("F:/OneDrive - ULEAM/6to-Semestre/Mineria de datos/codigos_repo/datos.csv", sep=";")
View(datos);
#################################################################################################
#2. Presentar una gráfica de los puntos en el plano x-y (gráfico de dispersión).
#Graficamos en diagrama de dispercion de los datos
plot(datos);
#Se calcula la distancia de los datos
distancia <- dist(datos)
#################################################################################################
#3. Presentar al menos 2 diferentes agrupamientos jerárquicos del dataset
# Primero . Se agrupan y se aplica el metodo de clousterd, y mostramo el dendogramas
dcompleta <- hclust(distancia, method = "complete")
plot(dcompleta)
#Se le aplica colo al borde para el estilo del dendograma
abline(h=2 )
rect.hclust(dcompleta, k=4, border = "blue")
# Segundo .
d2 <- hclust(distancia, method = "ward.D2")
plot(d2)
abline(h=2, col = "green")
dl2 <- cutree(d2, h=2)
table(dl2)
#################################################################################################
#4. Seleccionar un número de clusters, y ejecutar el agrupamiento por k-means.
clust_datos <- cutree(dcompleta, h = 3)
table(clust_datos)
km_datos <- kmeans(scale(datos), centers = 3,
                       nstart = 20, iter.max = 50)
km_datos$cluster


#################################################################################################
#5. Presentar los centroides de cada uno de los clusters.

plot(datos, col = km_datos$cluster)
points(km_datos$centers, cex= 2, col= 11,pch = 19)

# Genere el marco de datos de los datos x y segmentados
segment_customers <- cbind(datos, cluster = clust_datos)
View(segment_customers)

# Calcule la media para cada categoría
segment_customers %>% 
  group_by(cluster) %>% 
  summarise_all(mean)
########################
