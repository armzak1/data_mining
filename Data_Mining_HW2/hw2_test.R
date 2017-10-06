#P.1
load(file = 'UN_Armenia.rda')
str(armdata)
summary(armdata)
View(head(armdata, n=100))
armdata$amend <- NULL
armdata$para <- NULL

#P.2
class(armdata$vote)
levels(armdata$vote)
summary(armdata$vote)

#P.3
voted <- armdata$vote!='abstain' 
regr_data <- lm(formula =  armdata$importantvote ~ voted)
summary(regr_data)

#null hyp - voting doesn't depend on importance
#probably reject because of 0 p value, to be edited

#P.4
class(armdata$armpres)
regr_data2 <- lm(formula = armdata$armpres ~ armdata$vote)
summary(regr_data2)

table(armdata$vote)
?lm

#Errors, definitely rewrite

#P.5
subs <- armdata[armdata$country=='Armenia',]
table(subs[,c('issue', 'vote')])


#P.6
library(HistData)
df <- GaltonFamilies
regr_data3 <- lm(df$childHeight~df$gender)
summary(regr_data3)
