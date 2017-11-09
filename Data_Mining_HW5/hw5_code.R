#Pr.1
df_train <- read.csv('roomtrain.txt')
df_test <- read.csv('roomtest.txt')

#Pr.2
df_train$date <- NULL
df_test$date <- NULL
df_train$Occupancy <- factor(df_train$Occupancy, levels=c(0,1), labels=c('No', 'Yes'))
df_test$Occupancy <- factor(df_test$Occupancy, levels=c(0,1), labels=c('No', 'Yes'))

#Pr.3
library(class)
df_train_sc <- scale(df_train[,1:5], scale=T, center=T)
df_test_sc <- scale(df_test[,1:5], scale=T, center=T)
knn_res <- knn(df_train_sc, df_test_sc, df_train[,6], k=5, prob=TRUE)
summary(knn_res)

#Pr.4
table(df_test[,6], knn_res)
#top pred
acc <- (1648+912)/(1648+45+60+912)


#Pr.5
prob_tb <- data.frame(class = knn_res, probs = (attr(knn_res, 'prob')))
head(prob_tb)

#Pr.6
probs <- prob_tb$probs
probs[(probs)<1]
#TBE

#Pr.7
library(caret)
set.seed(1)
ctrl <- trainControl(method='cv', number=10)
knn_cv <- train(Occupancy~., data = df_train, method = 'knn', trControl = ctrl, preProcess = c('center','scale'),
                tuneLength=10)
plot(knn_cv)
knn_cv$results
#7,8,9
