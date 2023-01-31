### ZAD 5 GRUPIRANJE 

# requirements:
#install.packages('factoextra')
library('factoextra')
library('stats')

# loading the dataset:
data <- read.csv("diabetes.csv")

# Scaling the data:
data_scale <- scale(data)

# Using the elbow method to visualize the optimal number of clusters
fviz_nbclust(data_scale, kmeans, method = "wss") +
  labs(subtitle = "Elbow method")

# Number of clusters
k <-3  

# For reproducibility
set.seed(123)

# Applying k-means clustering to the scaled data
model <- kmeans(data_scale,
                centers=k,
                iter.max = 10,
                nstart = 1)

# Visualizing the clusters
fviz_cluster(model, data = data_scale)
