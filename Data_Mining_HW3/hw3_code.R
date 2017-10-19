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
hc <- hclust(dsmall_dist, method = 'complete')
#TBE

#Pr.5
plot(hc)
rect.hclust(hc, k = 3)
#TBE

#Pr.6
hc$merge
#TBE

#Pr.7
dsmall[,2:8] <- scale(dsmall[,2:8], center=T, scale=T)
#TBE

#Pr.8
hc <- hclust(dist(dsmall), method = 'complete')
plot(hc)
rect.hclust(hc, k = 3)
#TBE

#Pr.9
wdi <- read.csv('WDI indicators.csv')
wdi <- na.omit(wdi)
wdi_scaled <- as.data.frame(scale(wdi[,3:10], center=T, scale=T))

#Pr.10
tots={}
for(i in c(1:15)){
  set.seed(1)
  tots[i]=kmeans(wdi_scaled, i)$tot.withinss
}
plot(1:15,tots, type='b', pch=19, frame=FALSE)
opt_clust <- 8
#probably 11?

#Pr.11
km <- kmeans(wdi_scaled, opt_clust)
km$cluster
wdi$cluster <- as.factor(km$cluster)

#Pr.12
aggregate(.~cluster, data=wdi, FUN = 'mean')
View(wdi)
