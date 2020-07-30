#Determining optimal number of clusters-----Abel Mwandonga Haro.
install.packages("pkgs")
install.packages("NbClust")
library(factoextra)
library(NbClust)
data("USArrests")
# Standardize the data
gf<-scale(USArrests)
head(gf)
# Elbow method
fviz_nbclust(gf,kmeans,method = "wss")+
  geom_vline(xintercept = 4,linetype=2)+
  labs(subtitle = "Elbow method")
# Silhouette method
fviz_nbclust(gf,kmeans,method = "silhouette")+
  geom_vline(xintercept = 4,linetype=2)+
   labs(subtitle = "silhoutte")
# Gap statistic
set.seed(123)
fviz_nbclust(gf,kmeans,nstart=25,method = "gap_stat",nboot = 50)+
  labs(subtitle = "Gap statistic")
#NbClust() function: 30 indices for choosing the best number of clusters
nb<-NbClust(gf,distance = "euclidean",min.nc = 2,max.nc = 10,method="kmeans")
fviz_nbclust(nb)

#Computing cluster validation
install.packages("fpc")
library(fpc)
library(factoextra)
library(NbClust)
data("iris")
head(iris)
#Excluding column 5(species)
hf<-iris[,-5]
head(hf)
hf<-scale(hf)
# K-means clustering
km_res<-eclust(hf,"kmeans",k=3,nstart=25,graph = FALSE)
# Visualize k-means clusters
fviz_cluster(km_res,geom = "point",ellipse.type = "norm",palette="jco",
             ggtheme = theme_minimal())
# Hierarchical clustering
hc_res<-eclust(hf,"hclust",k=3,hc_metric = "euclidean",hc_method = "ward.D2",
               graph = FALSE)
# Visualize dendrograms
fviz_dend(hc_res,show_labels = FALSE,
          palette = "jco",as.ggplot=TRUE)
#Cluster validation
fviz_silhouette(km_res,palette="jco",
                ggtheme=theme_classic())
# Silhouette information
silinfo<-km_res$silinfo
names(silinfo)
# Silhouette widths of each observation
head(silinfo$widths[,1:3],10)
# Average silhouette width of each cluster
silinfo$clus.avg.widths
# The total average (mean of all individual silhouette widths)
silinfo$avg.width
km_res$size
sil<-km_res$silinfo$widths[,1:3]
# Objects with negative silhouette
neg_sil_index<-which(sil[,"sil_width"]<0)
sil[neg_sil_index,,drop=FALSE]

# Statistics for k-means clustering
library(fpc)
km_stats<-cluster.stats(dist(hf),km_res$cluster)
# Dun index
km_stats$dunn
km_stats
table(iris$Species,km_res$cluster)

# Compute cluster stats
species<-as.numeric(iris$Species)
clust_stats<-cluster.stats(d=dist(hf),
                           species,km_res$cluster)
clust_stats$corrected.rand
clust_stats$vi

# Agreement between species and pam clusters
pam_res<-eclust(hf,"pam",k=3,graph = FALSE)
table(iris$Species,pam_res$cluster)
cluster.stats(d=dist(iris_scaled),species,pam_res$cluster)
# Agreement between species and HC clusters
res_hc<-eclust(hf,"hclust",k=3,graph = FALSE)
table(iris$Species,res_hc$cluster)
cluster.stats(d=dist(iris_scaled),species,res_hc$cluster)
