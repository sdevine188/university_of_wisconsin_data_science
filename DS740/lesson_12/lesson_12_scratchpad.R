library(tidyverse)
library(skimr)
library(gridExtra)
library(ggdendro)

set.seed(30)
clust = sample(1:2,6, replace=T)
clust



#//////////////////////////////////////////////


x1c = c(-1,-2,-3,2,3,1)
x2c = c(1,2,1,-1,-1,-2)
x3c = c(-2,5,4,-1,-3,-3)
x = matrix(c(x1c,x2c,x3c),ncol=3)
pc.info = prcomp(x)

pc.info$rotation[,1]


pc.info$sdev
vjs = pc.info$sdev^2
pve = vjs/sum(vjs)


pc1scores = pc.info$x[,1]  # first principal component score vector
pc2scores = pc.info$x[,2]  # second principal component score vector


#//////////////////////////////////////


library(MASS)
x = UScereal[1:7,c(2:10)]
x.scale = scale(x)
x %>% glimpse()


pc.info = prcomp(x,center=T,scale=T)
pc.info$rotation  #loadings

min(n-1,p)
summary(pc.info)

pc.info$sdev
vjs = pc.info$sdev^2
pve = vjs/sum(vjs)
cumsum(pve)


#//////////////////////////////////////////////////////////////////////


# homework 

wine <- read.csv("wine.csv") %>% as_tibble()
wine 
wine 


wine %>% skim()

x <- wine
x.scale = scale(x)
x.scale

n = dim(x)[1]
p = dim(x)[2]


#////////////////////////


# complete
dist.x.scale = dist(x.scale, method="euclidean")
hc.fit = hclust(dist.x.scale,method="complete")  # Euclidean


# distance at which merge via complete linkage occurs
hc.fit$height
hc.4321 = hc.fit$height[(n-4):(n-1)]
hc.avg = (hc.fit$height[(n-3):(n-1)]+hc.fit$height[(n-4):(n-2)])/2
# obtaining cluster labels
hc.fit$height[(n-4):(n-1)]
nclust=3
htclust = 7.56 # mean(hc.fit$height[(n-2):(n-1)])
membclust = cutree(hc.fit,k=nclust) # cutree(hc.fit,h = htclust)
membclust

# visualize
dend.form = as.dendrogram(hc.fit)
dend.merge <- ggdendrogram(dend.form, rotate = F,labels=F) + 
        labs(title = "Complete linkage") 
dend.merge

x %>% ggplot(data = ., mapping = aes(x = Alcohol, y = Dilution, color = factor(membclust))) + geom_point()


#/////////////////


# single
hc.fit_single = hclust(dist.x.scale,method="single")  # Euclidean

# visualize
dend.form_single = as.dendrogram(hc.fit_single)
dend.merge_single <- ggdendrogram(dend.form_single, rotate = F,labels=F) + 
        labs(title = "Single linkage") 
dend.merge_single


#/////////////////


# average
hc.fit_average = hclust(dist.x.scale,method="average")  # Euclidean

# visualize
dend.form_average = as.dendrogram(hc.fit_average)
dend.merge_average <- ggdendrogram(dend.form_average, rotate = F,labels=F) + 
        labs(title = "average linkage") 
dend.merge_average


#/////////////////////////////////////////////////////////////////////


############ fitting non-hierarchical models ############
# K-means with p = 3 measurements
nclust=5  # try 2 and 3 and 4 clusters as well

set.seed(12)
clustA = kmeans(x.scale,nclust)$cluster
membclust = clustA

#############  Visuals - scatterplot pairs #############  
# pairs12 <- xselect %>%
#         ggplot( aes(x=Texture.mean, y=Area.extreme)) +
#         geom_point( color=membclust)
# pairs13 <- xselect %>%
#         ggplot( aes(x=Texture.mean, y=Smoothness.extreme))  +
#         geom_point( color=membclust)
# pairs23 <- xselect %>%
#         ggplot( aes(x=Area.extreme, y=Smoothness.extreme))  +
#         geom_point( color=membclust)
# grid.arrange(pairs12, pairs13, pairs23, nrow=1)


############# consistency of clusterings with k clusters #############  
#repeat the following a few times

clustA = kmeans(x.scale,nclust)$cluster
membclust = clustA
# run the visual commands (above)

clustB = kmeans(x.scale,nclust)$cluster
membclust = clustB
# run the visual commands (above)


tablematch <- table(clustA,clustB); tablematch

# if two clusters, just look at diagonals
matchtotal <- sum(apply(tablematch,2,max))
cluster_5_match_total <- matchtotal/n # .82
cluster_5_match_total


############ fitting non-hierarchical models ############
# K-means with p = 3 measurements
nclust=4  # try 2 and 3 and 4 clusters as well

RNGkind(sample.kind = "Rejection")
set.seed(12)
clustA = kmeans(x.scale,nclust)$cluster
membclust = clustA

#############  Visuals - scatterplot pairs #############  
# pairs12 <- xselect %>%
#         ggplot( aes(x=Texture.mean, y=Area.extreme)) +
#         geom_point( color=membclust)
# pairs13 <- xselect %>%
#         ggplot( aes(x=Texture.mean, y=Smoothness.extreme))  +
#         geom_point( color=membclust)
# pairs23 <- xselect %>%
#         ggplot( aes(x=Area.extreme, y=Smoothness.extreme))  +
#         geom_point( color=membclust)
# grid.arrange(pairs12, pairs13, pairs23, nrow=1)


############# consistency of clusterings with k clusters #############  
#repeat the following a few times

clustA = kmeans(x.scale,nclust)$cluster
membclust = clustA
# run the visual commands (above)

clustB = kmeans(x.scale,nclust)$cluster
membclust = clustB
# run the visual commands (above)


tablematch <- table(clustA,clustB); tablematch

# if two clusters, just look at diagonals
matchtotal <- sum(apply(tablematch,2,max))
cluster_4_match_total <- matchtotal/n # .75
cluster_4_match_total


############ fitting non-hierarchical models ############
# K-means with p = 3 measurements
nclust=3  # try 2 and 3 and 4 clusters as well

RNGkind(sample.kind = "Rejection")
set.seed(12)
clustA = kmeans(x.scale,nclust)$cluster
membclust = clustA

# inspect membclust and compare to membclust_hc
membclust %>% tibble(cluster = .) %>% count(cluster)
membclust %>% tibble(cluster_kmeans = .,
                     cluster_hc = membclust_hc) %>% 
        count(cluster_kmeans, cluster_hc) %>%
        arrange(desc(n)) 
membclust_hc
(52 + 50 + 48) / (52 + 50 + 48 + 16 + 10 + 1 + 1)

#############  Visuals - scatterplot pairs #############  
# pairs12 <- xselect %>%
#         ggplot( aes(x=Texture.mean, y=Area.extreme)) +
#         geom_point( color=membclust)
# pairs13 <- xselect %>%
#         ggplot( aes(x=Texture.mean, y=Smoothness.extreme))  +
#         geom_point( color=membclust)
# pairs23 <- xselect %>%
#         ggplot( aes(x=Area.extreme, y=Smoothness.extreme))  +
#         geom_point( color=membclust)
# grid.arrange(pairs12, pairs13, pairs23, nrow=1)


############# consistency of clusterings with k clusters #############  
#repeat the following a few times

clustA = kmeans(x.scale,nclust)$cluster
membclust = clustA
# run the visual commands (above)

clustB = kmeans(x.scale,nclust)$cluster
membclust = clustB
# run the visual commands (above)


tablematch <- table(clustA,clustB); tablematch

# if two clusters, just look at diagonals
matchtotal <- sum(apply(tablematch,2,max))
cluster_3_match_total <- matchtotal/n # 1
cluster_3_match_total


############ fitting non-hierarchical models ############
# K-means with p = 3 measurements
nclust=2  # try 2 and 3 and 4 clusters as well

RNGkind(sample.kind = "Rejection")
set.seed(12)
clustA = kmeans(x.scale,nclust)$cluster
membclust = clustA

#############  Visuals - scatterplot pairs #############  
# pairs12 <- xselect %>%
#         ggplot( aes(x=Texture.mean, y=Area.extreme)) +
#         geom_point( color=membclust)
# pairs13 <- xselect %>%
#         ggplot( aes(x=Texture.mean, y=Smoothness.extreme))  +
#         geom_point( color=membclust)
# pairs23 <- xselect %>%
#         ggplot( aes(x=Area.extreme, y=Smoothness.extreme))  +
#         geom_point( color=membclust)
# grid.arrange(pairs12, pairs13, pairs23, nrow=1)


############# consistency of clusterings with k clusters #############  
#repeat the following a few times

clustA = kmeans(x.scale,nclust)$cluster
membclust = clustA
# run the visual commands (above)

clustB = kmeans(x.scale,nclust)$cluster
membclust = clustB
# run the visual commands (above)


tablematch <- table(clustA,clustB); tablematch

# if two clusters, just look at diagonals
matchtotal <- sum(apply(tablematch,2,max))
cluster_2_match_total <- matchtotal/n # 1
cluster_2_match_total




RNGkind(sample.kind = "Rejection")
set.seed(12)



#////////////////////////////////////////////////////////////////////



pc.info = prcomp(x,center=T,scale=T)
pc.info
pc.info %>% attributes()
pc.info$rotation %>% as.data.frame() %>% 
        rownames_to_column(var = "var") %>% as_tibble() %>%
        arrange(PC1)


############# PC summary of variance ############# 
summary(pc.info) # min(n-1,p) is number of components explaining variance (proportion of variance  > 0)
summary(pc.info)$importance
plot(pc.info)
# cumulative PVE by direct computation
pc.info$sdev
vjs = pc.info$sdev^2
pve = vjs/sum(vjs); pve
cumsum(pve)  
# cumulative PVE directly from output
CumulativePVE <- summary(pc.info)$importance[3,]; CumulativePVE

plot(CumulativePVE, type = "o", ylab="Cumulative PVE", xlab="Principal Component")


############# PC components and visuals ############# 
# loadings of principal components
pc.info$rotation  
pc.loadings1 = pc.info$rotation[,1]  # loadings for first principal component
pc.loadings2 = pc.info$rotation[,2]  # loadings for second principal component
pc1scores = pc.info$x[,1]  # first principal component score vector
pc2scores = pc.info$x[,2]  # second principal component score vector

# plotting score vectors + loadings of first two principal components
biplot(pc.info,choices=1:2,scale=0)

# scores for observation 79
cbind(pc1scores,pc2scores)[159,]
# loadings for Dimesion.means
pc.loadings1[10]; pc.loadings2[10]



#/////////////////////////////////////////////////////////


genes = read.csv("GeneExpression.csv",header=F)
genesNew = t(genes); 
dim(genesNew)
genesNew = scale(genesNew)
genesNew

x.scale <- genesNew



# complete
dist.x.scale = dist(x.scale, method="euclidean")
hc.fit = hclust(dist.x.scale,method="complete")  # Euclidean


# distance at which merge via complete linkage occurs
hc.fit$height
hc.4321 = hc.fit$height[(n-4):(n-1)]
hc.avg = (hc.fit$height[(n-3):(n-1)]+hc.fit$height[(n-4):(n-2)])/2
# obtaining cluster labels
hc.fit$height[(n-4):(n-1)]
nclust=2
# htclust = 7.56 # mean(hc.fit$height[(n-2):(n-1)])
membclust = cutree(hc.fit,k=nclust) # cutree(hc.fit,h = htclust)
membclust
membclust %>% tibble(cluster = .) %>% 
        mutate(sample = row_number()) %>%
        select(sample, cluster) %>%
        filter(sample >= 21 & sample <= 40) %>%
        count(cluster)

# visualize
dend.form = as.dendrogram(hc.fit)
dend.merge <- ggdendrogram(dend.form, rotate = F,labels=F) + 
        labs(title = "Complete linkage") 
dend.merge

x %>% ggplot(data = ., mapping = aes(x = Alcohol, y = Dilution, color = factor(membclust))) + geom_point()


#////////////////////


pc.info = prcomp(x.scale,center=T,scale=T)
pc.info
pc.info %>% attributes()
pc.info$rotation %>% as.data.frame() %>% 
        rownames_to_column(var = "var") %>% as_tibble() %>%
        arrange(PC1)


############# PC summary of variance ############# 
summary(pc.info) # min(n-1,p) is number of components explaining variance (proportion of variance  > 0)
summary(pc.info)$importance
plot(pc.info)
# cumulative PVE by direct computation
pc.info$sdev
vjs = pc.info$sdev^2
pve = vjs/sum(vjs); pve
cumsum(pve)  
# cumulative PVE directly from output
CumulativePVE <- summary(pc.info)$importance[3,]; CumulativePVE

plot(CumulativePVE, type = "o", ylab="Cumulative PVE", xlab="Principal Component")


############# PC components and visuals ############# 
# loadings of principal components
pc.info$rotation  
pc.loadings1 = pc.info$rotation[,1]  # loadings for first principal component
pc.loadings2 = pc.info$rotation[,2]  # loadings for second principal component
pc1scores = pc.info$x[,1]  # first principal component score vector
pc2scores = pc.info$x[,2]  # second principal component score vector

# plotting score vectors + loadings of first two principal components
biplot(pc.info,choices=1:2,scale=0)

# scores for observation 79
cbind(pc1scores,pc2scores)[159,]
# loadings for Dimesion.means
pc.loadings1[10]; pc.loadings2[10]


#////////////////////////////////////


genesNew2 = genesNew[21:40,]; 
dim(genesNew2)
means2 = apply(genesNew2,2,mean)
means2



genes.pc = prcomp(genesNew)
genes.pc
pc.loadings1 = genes.pc$rotation[,1]  
pc.loadings1 %>% tibble(pc1_loadings = .) %>%
        mutate(abs_pc1_loadings = abs(pc1_loadings),
               var = genesNew %>% as.data.frame() %>% names()) %>%
        arrange(desc(abs_pc1_loadings))
genesNew %>% as.data.frame() %>% names()
means2 %>% tibble(means = .) %>%
        mutate(var = genesNew %>% as.data.frame() %>% names(),
               abs_means = abs(means)) %>%
        arrange(desc(abs_means))
        
