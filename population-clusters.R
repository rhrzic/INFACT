# Packages

require(dbscan)
require(caret)
require(factoextra)

# Currently using the Heart Disease UCI (https://archive.ics.uci.edu/ml/datasets/Heart+Disease) as a standin

df <- read.csv("data/heart.csv")
df <- subset(df, select = -target)

# Preprocess

preProcess <- preProcess(df, method = c("range"))
df <- predict(preProcess, df)

# Identify EPS (minimal points is default at 4)

kNNdistplot(df)

# HDBSCAN

db <- dbscan(df, eps = 0.8, minPts = 4)
db

fviz_cluster(db, df, geom="point")

# Identify nr clusters

wss <- (nrow(df)-1)*sum(apply(df,2,var))

for (i in 2:15) wss[i] <- sum(kmeans(df, centers=i)$withinss)

plot(1:15, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")

# k-means

set.seed(1)

km <- kmeans(df, 4, nstart = 1)
km

fviz_cluster(km, df, geom="point")
