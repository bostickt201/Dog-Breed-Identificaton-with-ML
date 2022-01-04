################ imports used ################

library(Rtsne)
library(ggplot2)
# library(caret)
library(data.table)

################ read in data ################

#getwd()
#setwd('~/Desktop/Fall2021/STAT380/dogBreedIds')
data <- fread("./project/volume/data/raw/data.csv")

################ perform pca ################

# subset out id cols
ids <- data[,1, drop=FALSE]
data = subset(data, select = -c(id))

# run pca
pca <- prcomp(data)

# pca screeplot
screeplot(pca)

# unclass() pca results
pca_dt <- data.table(unclass(pca)$x)

# run tsne on pca data
tsne <- Rtsne(pca_dt, pca = F, perplexity = 20, check_duplicates = F)

# grab out coords
tsne_dt <- data.table(tsne$Y)
ggplot(tsne_dt, aes(x = V1, y = V2)) + geom_point()

# add id cols to tsne table
tsne_dt <- cbind(tsne_dt, ids)

################ fit Gaussian mixture model to data for all k = 1 to k = max_clusters ################

k_bic <- Optimal_Clusters_GMM(tsne_dt[,.(V1, V2)], max_clusters = 15, criterion = "BIC")

# examine change in model fit between successive k's
delta_k <- c(NA, k_bic[-1] - k_bic[-length(k_bic)])

# plot
del_k_tab <- data.table(delta_k = delta_k, k = 1: length(delta_k))

ggplot(del_k_tab, aes(x = k, y = delta_k)) + geom_point() + geom_line()+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  geom_text(aes(label = k), hjust = 0, vjust = -1)

# choose a k and run it with model
opt_k <- 4
gmm_data <- GMM(tsne_dt[,.(V1, V2)], opt_k)

################ save the model ################

saveRDS(gmm_data, "./project/volume/models/gmm.model")

################ save tsne test output ################

fwrite(tsne_dt, './project/volume/data/processed/tsne_data.csv')
