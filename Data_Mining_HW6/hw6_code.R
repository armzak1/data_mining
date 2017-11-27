#Pr.1

load('ebay_train.rda')
load('ebay_test.rda')
str(ebay_train)
library(rpart)
model <- rpart(Competitive~., data=ebay_train)
library(rpart.plot)
prp(model, type=1, extra=1)
