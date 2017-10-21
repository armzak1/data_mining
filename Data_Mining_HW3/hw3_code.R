#Pr.1
load('Wholesale.rda')
rownames(df) <- df$customer_id
df$customer_id <- NULL

#Pr.2
class(df$Channel)
class(df$Region)
df$Channel <- factor(df$Channel, levels=c(1,2), labels=c('HoReCa', 'Retail'))
df$Region <- factor(df$Region, levels=c(1,2,3), labels=c('Lisbon', 'Oporto', 'Other Region'))

#Pr.3
dsmall <- head(df,n=10)
dsmall_dist <- dist(dsmall, method = 'euclidian')
dsmall_dist

#Pr.4
set.seed(1)
hc <- hclust(dsmall_dist, method = 'complete')
#This method defines the distance between clusters as  the distance between the two furthest apart members of those clusters

#Pr.5
plot(hc)
rect.hclust(hc, k = 3)
#Its distance was greater from the already formed clusters, so it got left out.

#Pr.6
hc$merge
#negative signs means a singleton, non negative sign number means the cluster formed at row [number] and in each row it
#shows that in that step the two entries have been merged

#Pr.7
dsmall[,3:8] <- scale(dsmall[,3:8], center=T, scale=T)
#center=T subtracts column means from corresponding columns of data
#scale=T divides each column of data by its standard deviation

#Pr.8
set.seed(1)
hc1 <- hclust(dist(dsmall), method = 'complete')
plot(hc1)
rect.hclust(hc1, k = 3)
#changed, significant, as we can see many things vars their clusters
#reason is that with normalized data we don't have a scale conflict, so distances are 
#different from initial data, so the dendrogram looks different

#Pr.9
wdi <- read.csv('WDI indicators.csv')
wdi <- na.omit(wdi)
wdi_scaled <- as.data.frame(scale(wdi[,3:10], center=T, scale=T))

#Pr.10
tots={}
for(i in c(1:10)){
  set.seed(1)
  tots[i]=kmeans(wdi_scaled, i)$tot.withinss
}
plot(1:10,tots, type='b', pch=19, frame=FALSE)
opt_clust <- 5
#by elbow method it's 5

#Pr.11
set.seed(1)
km <- kmeans(wdi_scaled, opt_clust)
km$cluster
wdi$cluster <- as.factor(km$cluster)


#Pr.12
View(wdi)
aggregate(.~cluster, data=wdi, FUN = 'mean')
#economically developed countries, upper end third world countries, lower end third world countries, high population
#first world countries

#Pr.13
#by looking at the means we can see that the above labels for clusters are reasonable

#Pr.14
View(wdi)
agg <- aggregate(Country~cluster, data=wdi, print)
#generally yes, surrounded with other post soviet countries

#Pr.15
library(FactoMineR)
ca <- PCA(wdi[3:10], ncp=8, graph = F)
plot(ca,choix="var", cex=0.5,invisible="quanti.sup")
summary(ca)
#dim1 explains 38.87% of variance, dim2 explains 19.72%.
#dim1 strongly correlates with neimpgnfszs and neexpgnfszs
#dim2 strongly correlates with nygdppcapcd and spdyn...
