library(DMwR2)
library(ggplot2)
library(caret)

gender_fac <- c(Female = 1, Male = 2)
married_fac <- c(Yes = 1, No = 0)
graduated_fac <- c(Yes = 1, No = 0)
spend_fac <- c(Low = 1, Average = 2, High = 3)
Cat_fac <- c(Cat_1 = 1, Cat_2 = 2, Cat_3 = 3, Cat_4 = 4, Cat_5 = 5, Cat_6 = 6)

clean_data_knn <- clean_data[,c(2:5,7:11)]
clean_data_knn$Gender <- gender_fac[clean_data$Gender]
clean_data_knn$Ever_Married <- married_fac[clean_data$Ever_Married]
clean_data_knn$Graduated <- graduated_fac[clean_data$Graduated]
clean_data_knn$Spending_Score <- spend_fac[clean_data$Spending_Score]
clean_data_knn$Var_1 <- Cat_fac[clean_data$Var_1]

clean_data_knn <- na.omit(clean_data_knn)
clean_data_knn[,1:8] <- scale(clean_data_knn[,1:8])

idxs <- sample(1:nrow(clean_data_knn), as.integer(0.8*nrow(clean_data_knn)))
train.data <- clean_data_knn[idxs,]
test.data <- clean_data_knn[-idxs,]

#-------------------------------------------------------------------------------
#Using caret: 10-fold validation
#run 100 times

trainControl10 <- trainControl(method = 'repeatedcv', number = 10, repeats = 100)
metric <- "Accuracy"
grid <- expand.grid(.k = c(1,3,5,7,9,11,13,15,17,19))

#Model 1: 8 predictors
set.seed(7)
fit.knn_model1 <- train(Segmentation~., data = train.data, method = "knn",
                        metric = metric, tuneGrid = grid, trControl = trainControl10)
print(fit.knn_model1)
plot(fit.knn_model1)

set.seed(7)
prediction_model1 <- predict(fit.knn_model1, newdata = test.data)
cf_model1 <- confusionMatrix(prediction_model1, factor(test.data$Segmentation))
print(cf_model1)


#Model 2: 7 predictors
set.seed(7)
fit.knn_model2 <- train(Segmentation~Gender+Ever_Married+Age+Graduated+
                          Work_Experience+Spending_Score+Family_Size,
                        data = train.data, method = "knn",metric = metric, 
                        tuneGrid = grid, trControl = trainControl10)
print(fit.knn_model2)
plot(fit.knn_model2)

set.seed(7)
prediction_model2 <- predict(fit.knn_model2, newdata = test.data)
cf_model2 <- confusionMatrix(prediction_model2, factor(test.data$Segmentation))
print(cf_model2)


#Model 3: 6 predictors (Don't have Var_1)
set.seed(7)
fit.knn_model3 <- train(Segmentation~Gender+Ever_Married+Age+Graduated+
                          Work_Experience+Spending_Score,
                        data = train.data, method = "knn",metric = metric, 
                        tuneGrid = grid, trControl = trainControl10)
print(fit.knn_model3)
plot(fit.knn_model3)

set.seed(7)
prediction_model3 <- predict(fit.knn_model3, newdata = test.data)
cf_model3 <- confusionMatrix(prediction_model3, factor(test.data$Segmentation))
print(cf_model3)


#Model 4: 5 predictors
set.seed(7)
fit.knn_model4 <- train(Segmentation~Gender+Ever_Married+Age+Graduated+
                          Work_Experience,
                        data = train.data, method = "knn",metric = metric, 
                        tuneGrid = grid, trControl = trainControl10)
print(fit.knn_model4)
plot(fit.knn_model4)

set.seed(7)
prediction_model4 <- predict(fit.knn_model4, newdata = test.data)
cf_model4 <- confusionMatrix(prediction_model4, factor(test.data$Segmentation))
print(cf_model4)


#Model 5: 4 predictors
set.seed(7)
fit.knn_model5 <- train(Segmentation~Gender+Ever_Married+Age+Graduated,
                        data = train.data, method = "knn",metric = metric, 
                        tuneGrid = grid, trControl = trainControl10)
print(fit.knn_model5)
plot(fit.knn_model5)

set.seed(7)
prediction_model5 <- predict(fit.knn_model5, newdata = test.data)
cf_model5 <- confusionMatrix(prediction_model5, factor(test.data$Segmentation))
print(cf_model5)


#Model 6: 3 predictors (Original Quantitative variables of data)
set.seed(7)
fit.knn_model6 <- train(Segmentation~Age+Work_Experience+Family_Size,
                        data = train.data, method = "knn",metric = metric, 
                        tuneGrid = grid, trControl = trainControl10)
print(fit.knn_model6)
plot(fit.knn_model6)

set.seed(7)
prediction_model6 <- predict(fit.knn_model6, newdata = test.data)
cf_model6 <- confusionMatrix(prediction_model6, factor(test.data$Segmentation))
print(cf_model6)


#Model 7: 2 predictors (Age+Work_Experience)
set.seed(7)
fit.knn_model7 <- train(Segmentation~Age+Work_Experience,
                        data = train.data, method = "knn",metric = metric, 
                        tuneGrid = grid, trControl = trainControl10)
print(fit.knn_model7)
plot(fit.knn_model7)

set.seed(7)
prediction_model7 <- predict(fit.knn_model7, newdata = test.data)
cf_model7 <- confusionMatrix(prediction_model7, factor(test.data$Segmentation))
print(cf_model7)

#-------------------------------------------------------------------------------

table_accuracy <- data.frame(k = c(1,3,5,7,9,11,13,15,17,19))
table_accuracy$Model1 <- c(0.4010939,0.4199460,0.4415032,0.4536717,0.4598868,
                           0.4658411,0.4689128,0.4706041,0.4709696,0.4696362)
table_accuracy$Model2 <- c(0.4125499,0.4310699,0.4454357,0.4554728,0.4605261,
                           0.4611811,0.4642391,0.4682451,0.4708648,0.4726091)
table_accuracy$Model3 <- c(0.4095812,0.4247407,0.4374641,0.4475257,0.4562382,
                           0.4596025,0.4635702,0.4658107,0.4677129,0.4694114)
table_accuracy$Model4 <- c(0.3984359,0.4066494,0.4183968,0.4280841,0.4352616,
                           0.4404806,0.4447021,0.4499842,0.4532127,0.4555609)
table_accuracy$Model5 <- c(0.4538820,0.4549574,0.4549014,0.4545721,0.4556167,
                           0.4561419,0.4559184,0.4578785,0.4599632,0.4613471)
table_accuracy$Model6 <- c(0.3918069,0.4048048,0.4142993,0.4212303,0.4242619,
                           0.4255639,0.4282719,0.4293412,0.4308139,0.4316871)
table_accuracy$Model7 <- c(0.3947365,0.4000087,0.4034448,0.4042788,0.4067283,
                           0.4094885,0.4096618,0.4081945,0.4082596,0.4063669)

table_accuracy

max(table_accuracy[,2:8])
colnames(table_accuracy)[which(table_accuracy == max(table_accuracy[,2:8]),arr.ind = T,)[,2]]
k <- table_accuracy$k[which(table_accuracy == max(table_accuracy[,2:8]),arr.ind = T,)[,1]]


#-------------------------------------------------------------------------------

# for new record
library(ipred)
library(mlbench)
library(MASS)
library(klaR)

newrecord_knn <- data.frame(Gender=2,Ever_Married=0,Age=40,Graduated=1,
                            Work_Experience=10,Spending_Score=2,Family_Size=1)
train <- clean_data_knn[1:nrow(clean_data_knn)+1,]

KNN <- ipredknn(Segmentation~Gender+Ever_Married+Age+Graduated+Work_Experience+Spending_Score+Family_Size,
                data=train,k=k)
result_predict <- predict(KNN,newrecord_knn,'class')
result_predict

#-------------------------------------------------------------------------------









