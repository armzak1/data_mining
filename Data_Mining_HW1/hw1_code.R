#ex.1
load(file = 'Cars93.rda')
dim(df)

#ex.2
#View(head(df, n=5))
head(df, n=5)

#ex.3
df[which.max(df$Max.Price),]
df[which.min(df$Min.Price),]

#ex.4
class(df$Origin)
df$Origin <- factor(df$Origin,labels=c(0,1))
#TOBECHANGED

#ex.5
class(df$Type)
df$Type
#IS FACTOR AND IS ORDINAL

#ex.6
set.seed(1)
df_small <- df[sample(nrow(df),50),]
plot(x=df_small$EngineSize, y=df_small$Price, 
     main='Scatterplot of Engine Size and Price',
     xlab='Engine Size', ylab='Price')
#POSITIVE CORRELATION

#ex.7
plot(x=df_small$MPG.city, y=df_small$Price, 
     main='Scatterplot of MPG in City and Price',
     xlab='MPG in City', ylab='Price')
#negative correlation

#ex.8
plot(x=df_small$Origin, 
     main='Bar Graph of Origin',
     xlab='Origin')
#slighly more non US cars

#ex.9
hist(df$Price, main='Histogram of Prices', breaks=10,
     xlab='Price', ylab='Number of Cars')
#histogram shows info about frequency of each price range in df(i.e. distribution)

#ex.10
boxpltres <- boxplot(df$Price, main='Boxplot of Price', ylab='Price')
length(boxpltres$out)

#ex.11
df <- df[df$Price<min(boxpltres$out),]
dim(df)


boxpltres2 <- boxplot(df$Max.Price, main='Boxplot of Max Price', ylab='Price')
df <- df[df$Max.Price<min(boxpltres2$out),]
dim(df)
#5 observations omitted as they were outliers by price and/or max price

#ex.12
aggregate(df$Luggage.room~df$Manufacturer, data = df, mean)

#ex.13
aggregate(df$Max.Price~df$Manufacturer, data=df, max)

#ex.14
library(ggplot2)
mymidwest <- midwest
p <- ggplot(mymidwest,aes(x=mymidwest$poptotal, 
                     y=mymidwest$area, color=mymidwest$state))+
  geom_point()+
  labs(x='Total Population', y='Area', 
       title = 'Scatterplot of Total Population and Area', 
       color='Color by States')

#ex.15
p+facet_grid(mymidwest$state~.)
#ADD LABELS

#ex.16
#TBD

ggplot(mymidwest,aes(x=mymidwest$percprof, 
                     y=mymidwest$percollege))+
  geom_point()+geom_smooth(method = 'lm', na.rm=TRUE)+
  labs(x='Percent Profession', y='Percentage of C.E.P', 
       title = 'Scatterplot of Percentages of CEP and Professions')

ggplot(mymidwest,aes(x=mymidwest$percblack,
                     y=mymidwest$percbelowpoverty))+
  geom_point()+geom_smooth(method = 'lm', na.rm=TRUE)+
  labs(x='Percentage of Black People', y='Percentage of People Below Poverty', 
       title = 'Scatterplot of Percentages of Black People and People Below Poverty')
  

#ex.17
ggplot(mymidwest,aes(x=mymidwest$percollege, 
                     y=mymidwest$percpovertyknown))+
  geom_point()+
  labs(x='Percentage of C.E.P', y='Known Percentage of Poverty', 
       title = 'Scatterplot of Percentages of CEP and Poverty')

#ex.18
cor(mymidwest$percollege, mymidwest$percpovertyknown)

#ex.19
num_midwest <- mymidwest[,4:13]
cor(num_midwest)

#ex.20
library(corrplot)
midwestcor <- mymidwest[,c(-2,-3,-ncol(mymidwest))]
View(midwestcor)
corrplot(cor(midwestcor), method = 'shade')
