################# Random forest on iris dataset ##########################
library(randomForest)
library(caret)
data("iris")

partition <- createDataPartition(iris$Species,p=.75,list = F)
train <- iris[partition,]
test <- iris[-partition,]

model.iris <- randomForest(train$Species~.,data=train, na.action=na.roughfix,importance=TRUE, ntree=1000)
model.iris

pred_train <- predict(model.iris,train)
pred_train

confusionMatrix(pred_train,train$Species)  ## accuracy 1

tab1 <- table(pred_train,train$Species)
tab1
sum(diag(tab1))/sum(tab1)                 ## 1

mean(pred_train==train$Species)           ## 1

pred_test <- predict(model.iris,test)
confusionMatrix(pred_test,test$Species)   ## accuracy 0.9444 

tab2 <- table(pred_test,test$Species)
tab2
sum(diag(tab2))/sum(tab2)       ## 0.9444 
mean(pred_test==test$Species)   ## 0.9444 

# Visualization 
plot(model.iris,lwd=2)
legend("topright", colnames(model.iris$err.rate),col=1:4,cex=0.8,fill=1:4)

