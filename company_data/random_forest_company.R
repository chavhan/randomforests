##### Random forest on company Dataset #####################
library(randomForest)
library(caret)


names(Company_Data)
dim(Company_Data)
str(Company_Data)
company_new_data <- Company_Data

company_new_data$ShelveLoc <- as.numeric(as.factor(company_new_data$ShelveLoc))
company_new_data$Urban <- as.numeric(as.factor(company_new_data$Urban))
company_new_data$US <- as.numeric(as.factor(company_new_data$Urban))
head(company_new_data)

## dividing in train and test data 
partition <- createDataPartition(company_new_data$Sales,p=.75,list = F)
train_data <- company_new_data[partition,]
test_data <- company_new_data[-partition,]

model_company <- randomForest(train_data$Sales~., data = train_data, na.action=na.roughfix,importance=TRUE, ntree=500)
model_company

pred_train <- predict(model_company,train_data)
pred_train
tab1 <- table(pred_train,train_data$Sales)
tab1
mean(pred_train==train_data$Sales)
library(MLmetrics)
sale_accuracy <- R2_Score(y_pred = predict(model_company),y_true = train_data$Sales)
sale_accuracy       ## accuracy 0.5954959

