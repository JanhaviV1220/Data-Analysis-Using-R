# install and load the libraies
library(cluster) # For K-Medoids(pam)
data(iris)

#Process the data
# Remove the species column(As it is the target label and is not used for clustering)
iris_data <- iris[,-5]

#Applying K-Medoids algorithm with 3 clusters
kmedoids_result <- pam(iris_data,k=3)

# Display clustering results
print(kmedoids_result)

# Visualize the results 
# Plot the clusters using clusplot()
clusplot(iris_data, kmedoids_result$cluster, color = TRUE, shade = TRUE, labels = 3, lines = 0)

# Evaluate the performance
# Compare with actual species in the dataset
table(iris$Species, kmedoids_result$cluster)

# Visualize using ggplot2
iris_clustered <- iris
iris_clustered$Cluster <- as.factor(kmedoids_result$cluster)
ggplot(iris_clustered,aes(x = Sepal.Length, y = Sepal.Width, color = Cluster))+
  geom_point(size = 4, alpha = 0.6)+
  labs(title = "K-Medoids clustering on iris dataset",x = "Sepal Length", y = "Sepal Width")

# Silhouette plot to evaluate the quality of the cluster
silhouette_score <- silhouette(kmedoids_result$cluster,dist(iris_data))
plot(silhouette_score)

# load the mclust package
library(mclust)

# calculate Adjusted Rand Index(ARI)
ari <- adjustedRandIndex(iris$Species,kmedoids_result$cluster)
cat("Adjusted Rand Index:",ari)