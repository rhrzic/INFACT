# Packages

require(dbscan)
require(caret)
require(factoextra)

# Currently using the Heart Disease UCI (https://archive.ics.uci.edu/ml/datasets/Heart+Disease) as a standin

df <- read.csv("data/heart.csv")

df_train <- subset(df, select = -target)

# Preprocess

preProcess <- preProcess(df_train, method = c("range"))
df_train <- predict(preProcess, df)

# Identify EPS (minimal points is default at 4)

kNNdistplot(df_train)
abline(h = 0.9)

# HDBSCAN

set.seed(1)

db <- dbscan(df_train, eps = 0.9, minPts = 4)
db

fviz_cluster(db, df_train, geom="point")

# Identify nr clusters

wss <- (nrow(df)-1)*sum(apply(df,2,var))

for (i in 2:15) wss[i] <- sum(kmeans(df, centers=i)$withinss)

plot(1:15, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")

# k-means

set.seed(1)

km <- kmeans(df, 4, nstart = 1)
km

fviz_cluster(km, df)

# OPTICS

opt <- optics(df_train, eps = 1, minPts = 4)
opt <- extractDBSCAN(opt, eps_cl = 1)

# Evaluate relationship between cluster and target

df$dbscan_cluster <- db$cluster
df$km_cluster <- km$cluster
df$opt_cluster <- opt$cluster

plot(target ~ dbscan_cluster, data=df)
plot(target ~ km_cluster, data=df)
plot(target ~ opt_cluster, data=df)
