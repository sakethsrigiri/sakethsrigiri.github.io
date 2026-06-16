#Load the Required Libraries

# Install packages if needed
# install.packages("factoextra")

library(factoextra)

# Load the Dataset / create your own sample dataset
customers <- data.frame(
  CustomerID = 1:15,
  AnnualIncome = c(15, 18, 25, 30, 35, 45, 50, 55, 60, 65, 70, 75, 80, 90, 100),
  SpendingScore = c(39, 45, 50, 60, 65, 70, 75, 40, 35, 30, 80, 85, 25, 20, 15)
)

head(customers)

# Select the variables
data <- customers[, c("AnnualIncome", "SpendingScore")]

head(data)

#Scale the Data
scaled_data <- scale(data)

head(scaled_data)

#Determine the optimal number of Clusters
wss <- numeric(10)

for (i in 1:10) {
  wss[i] <- kmeans(
    scaled_data, 
    centers = i, 
    nstart = 25
  )$tot.withinss
}

plot(
  1:10,
  wss,
  type = "b",
  pch = 19,
  col = "blue",
  xlab = "Number of Clusters (K)",
  ylab = "Within-Cluster Sum of Squares",
  main = "Elbow Method"
)

#Run the K-Means Algorithm
set.seed(123)

km <- kmeans(
  scaled_data,
  centers = 4,
  nstart = 25
)

km

#Assign Cluster labels
customers$Cluster <- km$cluster

customers

# Visualizing the clusters
fviz_cluster(
  km,
  data = scaled_data,
  geom = "point",
  ellipse.type = "convex",
  ggtheme = theme_minimal()
)

km$centers

# Summarizing each cluster
aggregate(
  customers[, c("AnnualIncome", "SpendingScore")],
  by = list (Cluster = customers$Cluster),
  mean
)


