# Load the necessary libraries and dataset
data(iris)

# View the data
head(iris)

# Processing the data
# Removing the species column for clustering
iris_data = iris[,-5]

# Apply k-Means algorithm
kmeans_result <- kmeans(iris_data,centers = 3,nstart = 25)
print(kmeans_result)

# Visualize the results
library(cluster) # For clustering visualization
library(ggplot2) # For better visualization

# Plot the clusters
clusplot(iris_data, kmeans_result$cluster, color = TRUE, shade = TRUE, labels = 2, lines = 0)

# Evaluate the performance
table(iris$Species,kmeans_result$cluster)

# Visialize using ggplot2
iris_clustered <- iris
iris_clustered$Cluster <- as.factor(kmeans_result$cluster)
ggplot(iris_clustered,aes(x = Sepal.Length, y = Sepal.Width, color = Cluster))+
  geom_point(size = 4,alpha = 0.6)+
  labs(title = "K-Means Clustering on Iris Dataset", x = "Sepal Length", y = "Sepal Width")

# Load cluster library for silhoutte calculation
library(cluster)

# Calculate Silhoutte score
sil_score <- silhouette(kmeans_result$cluster,dist(iris_data))
plot(sil_score)

#Calculate Adjusted Rand Index(ARI)
install.packages("mclust")
library("mclust")
ari <- adjustedRandIndex(iris$Species,kmeans_result$cluster)
cat("Adjusted Rand Index:",ari)
