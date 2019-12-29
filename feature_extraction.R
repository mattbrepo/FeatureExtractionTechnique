#
#
# Feature extraction
#
#
library("umap") # https://cran.r-project.org/web/packages/umap/index.html
library("tsne") # https://cran.r-project.org/web/packages/tsne/index.html

addColors <- function(x, labels) {
  df <- as.data.frame(x)
  df$colors[labels == 'Iris-setosa'] <- 'red'
  df$colors[labels == 'Iris-versicolor'] <- 'green'
  df$colors[labels == 'Iris-virginica'] <- 'blue'
  return(df)
}

# read Iris data
inData <- read.csv("./data/mtx1.txt", header=FALSE)
inLabels <- read.csv("./data/labels.txt", header=FALSE)

# Plot settings
par(mfrow=c(1, 3))

# t-SNE
rTSNE <- tsne(inData, NULL, k=2, initial_dims=50, perplexity=30, max_iter=500, min_cost=0, epoch_callback=NULL, whiten=FALSE, epoch=100)
rTSNE.df <- addColors(rTSNE, inLabels)
plot(rTSNE.df[,1], rTSNE.df[,2], col=rTSNE.df$colors, main="t-SNE")

# UMAP
rUMAP <- umap(inData)
rUMAP.df <- addColors(rUMAP$layout, inLabels)
plot(rUMAP.df[,1], rUMAP.df[,2], col=rUMAP.df$colors, main="UMAP")

# PCA
rPCA <- prcomp(inData, center=FALSE, scale=FALSE)
rPCA.df <- addColors(rPCA$x, inLabels)
plot(rPCA.df[,1], rPCA.df[,2], col=rPCA.df$colors, main="PCA")
