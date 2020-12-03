####### Applying random forest on fraud check dataset ###############
library(randomForest)
library(caret) 

names(Fraud_check)
f_check <- Fraud_check

## changing categorical variable into numeric
f_check$Undergrad <- as.numeric(as.factor(f_check$Undergrad))
f_check$Marital.Status <- as.numeric(as.factor(f_check$Marital.Status))
f_check$Urban <- as.numeric(as.factor(f_check$Urban))

str(Fraud_check)
str(f_check)
?filter
risky_data <- filter(f_check, f_check$Taxable.Income <= 30000)
head(risky_data)
good_data <- filter(f_check, f_check$Taxable.Income > 30000)
head(good_data)

good_data$Taxable.Income <- 'good'
risky_data$Taxable.Income <- 'risky'

final_data <- rbind(good_data,risky_data)
final_data$Taxable.Income <- as.factor(final_data$Taxable.Income)
dim(final_data)

head(final_data)
str(final_data)

partition <- createDataPartition(final_data$Taxable.Income,p=.75,list = F)
train_data <- final_data[partition,]
test_data <- final_data[-partition,]

######## Creating Random Forest Model 
model.fit <- randomForest(train_data$Taxable.Income~.,data=train_data, na.action=na.roughfix,importance=TRUE, ntree=500)
model.fit

confusionMatrix(predict(model.fit),train_data$Taxable.Income)
##confusionMatrix(predict(model.fit),test_data$Taxable.Income)

## accuracy of train data 
pred_train <- predict(model.fit,train_data)
pred_train
tab1 <- table(pred_train,train_data$Taxable.Income)
tab1
sum(diag(tab1))/sum(tab1)                     ## accuracy 0.9044444
mean(pred_train==train_data$Taxable.Income)

## accuracy of test data 
pred_test <- predict(model.fit,test_data)
pred_test
tab2 <- table(pred_test,test_data$Taxable.Income)
tab2
sum(diag(tab2))/sum(tab2)                      ## accuracy 0.7733333
mean(pred_test==test_data$Taxable.Income)