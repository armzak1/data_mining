#Pr.1
df <- read.csv('Diabetes.csv')
df$Class <- factor(df$Class, levels=c(0,1), labels=c('No','Yes'))
table(df$Class)

#Pr.2
library(caret)
set.seed(1)
training_ind <- createDataPartition(df$Class, p=0.7, list=FALSE)
df_train <- df[training_ind,]
df_test <- df[-training_ind,]

#Pr.3
model <- glm(Class~., data = df_train, family = 'binomial')
exp(model$coefficients)
#TBE

#Pr.4
df_predict <- predict(model, newdata = df_test)
prclass <- ifelse(df_predict>0.5, 'Yes', 'No')
tb <- table(df_test$Class,prclass)
tb

#Pr.5
overall_acc <- (42+134)/(134+16+38+42)
sensitivity <- (42)/(42+38)
specificity <- (134)/(134+16)
ppv <- (42)/(42+16)
npv <- (134)/(134+38)
cm <- caret::confusionMatrix(prclass, df_test$Class, positive='Yes')
library(ROCR)
p_test <- prediction(df_predict, df_test$Class)
perf <- performance(p_test,'tpr','fpr')
plot(perf)
performance(p_test, 'auc')@y.values
