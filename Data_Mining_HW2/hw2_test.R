#P.1
load(file = 'UN_Armenia.rda')
str(armdata)
summary(armdata)
#View(head(armdata, n=100))
armdata$amend <- NULL
armdata$para <- NULL

#P.2
class(armdata$vote)
levels(armdata$vote)
summary(armdata$vote)

#P.3
voted <- armdata$vote!='abstain' 
table(armdata$importantvote,voted)
chisq_res <- chisq.test(table(armdata$importantvote,voted))
chisq_res$p.value

#null hyp - voting doesn't depend on importance
#probably reject because of 0 p value, to be edited

#P.4
class(armdata$armpres)
tbl <- table(armdata$armpres, armdata$vote)
chisq.test(tbl)$p.value

#null hyp - voting pattern is independent from president
#reject, as p value is 0. 
#to be edited

#P.5
subs <- armdata[armdata$country=='Armenia',]
table(subs[,c('issue', 'vote')])


#P.6
library(HistData)
df <- GaltonFamilies
tt <- t.test(childHeight~gender, data=df)
tt$p.value

#null hyp - means of samples from two groups are independent
#reject, as p value is 0

#P.7
autompg <- read.csv('auto-mpg.csv')
str(autompg)
View(autompg)
autompg$horsepower <- as.numeric(levels(autompg$horsepower))[autompg$horsepower]
autompg$origin <- as.factor(autompg$origin)

#P.8
set.seed(1)
train_ind <- sample(nrow(autompg),277)
am_train <- autompg[train_ind,]
am_test <- autompg[-train_ind,]

#P.9  
cor(autompg[,1:7])
plot(x=autompg$mpg, y=autompg$weight)
#first: weight, second: displacement

#P.10
max(autompg$mpg)
regr_data_mw <- lm(formula = mpg~weight, data=autompg)
regr_data_mw$coefficients
#slope is negative, which indicates a negative relationship between the two variables
#intercept ?

summary(regr_data_mw)$adj.r.squared
regr_model <- lm(formula = mpg~weight, data=am_train)
am_pred <- predict(regr_model, newdata=am_test)
sqrt(mean((am_pred-am_test$mpg)^2))

#P.11
regr_data_md <- lm(formula = mpg~displacement, data=autompg)
regr_data_md$coefficients

#again negative, but a stronger relatonship than in the previous case
#intercept ?

summary(regr_data_md)$adj.r.squared
regr_model2 <- lm(formula = mpg~displacement, data=am_train)
am_pred2 <- predict(regr_model2, newdata=am_test)
sqrt(mean((am_pred2-am_test$mpg)^2))

#P.12
head(autompg$displacement)
regr_model <- lm(formula = mpg~weight+model.year, data=am_train)
am_test_pred <- predict(regr_model, newdata=am_test)
sqrt(mean((am_test_pred-am_test$mpg)^2))

#adding variable imporved the result

am_train_pred <- predict(regr_model, newdata = am_train)
sqrt(mean((am_train_pred-am_train$mpg)^2))

#errors are not significantly different, so no underfitting or overfitting took place