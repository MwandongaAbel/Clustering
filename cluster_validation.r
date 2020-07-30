# Clustering Validation......Abel Mwandonga Haro.
install.packages("clustertend")
library(factoextra)
library(clustertend)
data("iris")
head(iris)
#Excluding column species from data set.
gf<-iris[,-5]
# Random data generated from the iris data set
random_gf<-apply(gf,2,function(x){runif(length(x),min(x),(max(x)))})
random_gf<-as.data.frame(random_gf)
# Standardize the data sets
gf<-iris_scaled<-scale(gf)
random_gf<-scale(random_gf)
#Visualization of the data set.
install.packages("backports")
library(backports)
fviz_pca_ind(prcomp(gf), title = "PCA - Iris data",
             habillage = iris$Species, palette = "jco",
             geom = "point", ggtheme = theme_classic(),
            legend = "bottom")
#plot random gf
fviz_pca_ind(prcomp(random_gf),title="PCA_random_data",geom = "point",ggtheme=theme_classic())
# K-means on iris dataset
set.seed(123)
km_res<-kmeans(gf,3)
fviz_cluster(list(data=gf,cluster=km_res$cluster),
             ellipse.type = "norm",geom = "point",stand = FALSE,
             palette="jco",ggtheme = theme_classic())
# K-means on the random dataset
k_means_random<-kmeans(random_gf,3)
fviz_cluster(list(data=random_gf,cluster=k_means_random$cluster),
             ellipse.type = "norm",geom="point",stand = FALSE,
             palette="jco",ggtheme = theme_classic())
# Hierarchical clustering on the random dataset
fviz_dend(hclust(dist(random_gf)),k=3,k_colors = "jco",
          as.ggplot=TRUE,show_labels = FALSE)
#Hopkins
# Compute Hopkins statistic for iris dataset
set.seed(123)
hopkins(gf,n=nrow(gf)-1)
# Compute Hopkins statistic for a random dataset
hopkins(random_gf,n=nrow(gf)-1)
#visual assessment of cluster tendency (VAT)
fviz_dist(dist(gf),show_labels = FALSE)+
  labs(title="iris_data")
fviz_dist(dist(random_gf),show_labels = FALSE)+
  labs(title="Random_data")
