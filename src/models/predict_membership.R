################ imports used ################

library(ClusterR)
library(ggplot2)

################ read in data from model and tsne output ################

gmm_data <- readRDS("./project/volume/models/gmm.model")
tsne_dt <- fread('./project/volume/data/processed/tsne_data.csv')

################ convert log-like to prob of membership ################

l_clust <- gmm_data$Log_likelihood^10
l_clust <- data.table(l_clust)

net_lh <- apply(l_clust, 1, FUN = function(x){sum(1/x)})

cluster_prob <- 1/l_clust/net_lh

# view clusters
tsne_dt$Cluster_1_prob <- cluster_prob$V1
ggplot(tsne_dt, aes(x = V1, y = V2, col = Cluster_1_prob)) + geom_point()

tsne_dt$Cluster_2_prob <- cluster_prob$V2
ggplot(tsne_dt, aes(x = V1, y = V2, col = Cluster_2_prob)) + geom_point()

tsne_dt$Cluster_3_prob <- cluster_prob$V3
ggplot(tsne_dt, aes(x = V1, y = V2, col = Cluster_3_prob)) + geom_point()

tsne_dt$Cluster_4_prob <- cluster_prob$V4
ggplot(tsne_dt, aes(x = V1, y = V2, col = Cluster_4_prob)) + geom_point()

################ write and save submission ################

submission <- tsne_dt[, 3:7]

names(submission) <- c('id', 'breed_1', 'breed_2', 'breed_4', 'breed_3')
submission <- submission [, c('id', 'breed_1', 'breed_2', 'breed_3', 'breed_4')]

fwrite(submission, "./project/volume/data/processed/submission.csv")
