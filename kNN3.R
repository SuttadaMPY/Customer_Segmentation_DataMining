
library(DMwR2)
library(ggplot2)
library(caret)

clean_data2 = clean_data
clean_data2$Gender <- ifelse(clean_data2$Gender == 'Male', 1, 0)
clean_data2$Ever_Married <- ifelse(clean_data2$Ever_Married == 'Yes', 1, 0)
clean_data2$Graduated <- ifelse(clean_data2$Graduated == 'Yes', 1, 0)

spend_fac <- c(Low = 1, Average = 2, High = 3)
clean_data2$Spending_Score <- spend_fac[clean_data2$Spending_Score]
clean_data_bf <- clean_data
colnames(clean_data_bf)
colnames(clean_data_knn)


#----- Create Dummy Variables

# Dummy Profession
Profession.Artist <- ifelse(clean_data2$Profession == 'Artist', 1, 0)
Profession.Doctor <- ifelse(clean_data2$Profession == 'Doctor', 1, 0)
Profession.Engineer <- ifelse(clean_data2$Profession == 'Engineer', 1, 0)
Profession.Entertainment <- ifelse(clean_data2$Profession == 'Entertainment', 1, 0)
Profession.Executive <- ifelse(clean_data2$Profession == 'Executive', 1, 0)
Profession.Healthcare <- ifelse(clean_data2$Profession == 'Healthcare', 1, 0)
Profession.Homemaker <- ifelse(clean_data2$Profession == 'Homemaker', 1, 0)
Profession.Lawyer <- ifelse(clean_data2$Profession == 'Lawyer', 1, 0)
Profession.Marketing <- ifelse(clean_data2$Profession == 'Marketing', 1, 0)

clean_data_knn <- data.frame(Gender = clean_data2$Gender, 
                             Ever_Married = clean_data2$Ever_Married,
                             Age = clean_data2$Age, Graduated = clean_data2$Graduated,
                             Profession.Artist = Profession.Artist, Profession.Doctor = Profession.Doctor,
                             Profession.Engineer = Profession.Engineer,
                             Profession.Entertainment = Profession.Entertainment,
                             Profession.Executive = Profession.Executive,
                             Profession.Healthcare = Profession.Healthcare,
                             Profession.Homemaker = Profession.Homemaker,
                             Profession.Lawyer = Profession.Lawyer, 
                             Profession.Marketing = Profession.Marketing,
                             Work_Experience = clean_data2$Work_Experience,
                             Spending_Score = clean_data2$Spending_Score,
                             Family_Size = clean_data2$Family_Size, 
                             Segmentation = clean_data2$Segmentation)

clean_data_knn
write.csv(clean_data_knn, "D:/477 Data Mining/Project 477/customerseg_knn.csv",row.names = F)

head(train.data)

clean_data_knn[,1:16] <- scale(clean_data_knn[,1:16])
idxs <- sample(1:nrow(clean_data_knn), as.integer(0.8*nrow(clean_data_knn)))
train.data <- clean_data_knn[idxs,]
test.data <- clean_data_knn[-idxs,]

#-------------------------------------------------------------------------------

train_Control <- trainControl(method = 'repeatedcv', number = 10, repeats = 100)
metric <- "Accuracy"
grid <- expand.grid(.k = c(1:20))

#Model 1: 16 predictors (including dummy variables)
set.seed(7)
fit.knn_model1 <- train(Segmentation~., data = train.data, method = "knn",
                        metric = metric, tuneGrid = grid, trControl = train_Control)
print(fit.knn_model1)
plot(fit.knn_model1, main = "Relationship between k and Accuracy\n in Model 1")

set.seed(7)
prediction_model1 <- predict(fit.knn_model1, newdata = test.data)
cf_model1 <- confusionMatrix(prediction_model1, factor(test.data$Segmentation))
print(cf_model1)


#Model 2: 15 predictors (Excluding Work_Experience)
set.seed(7)
fit.knn_model2 <- train(Segmentation~Gender+Ever_Married+Age+Graduated+
                          Profession.Artist+Profession.Doctor+Profession.Engineer+
                          Profession.Entertainment+Profession.Executive+Profession.Healthcare+
                          Profession.Homemaker+Profession.Lawyer+Profession.Marketing+
                          Spending_Score+Family_Size,
                        data = train.data, method = "knn",metric = metric, 
                        tuneGrid = grid, trControl = train_Control)
print(fit.knn_model2)
plot(fit.knn_model2, main = "Relationship between k and Accuracy\n in Model 2")

set.seed(7)
prediction_model2 <- predict(fit.knn_model2, newdata = test.data)
cf_model2 <- confusionMatrix(prediction_model2, factor(test.data$Segmentation))
print(cf_model2)


#Model 3: 13 predictors (Remove Gender and Family_Size)
set.seed(7)
fit.knn_model3 <- train(Segmentation~Ever_Married+Age+Graduated+
                          Profession.Artist+Profession.Doctor+Profession.Engineer+
                          Profession.Entertainment+Profession.Executive+Profession.Healthcare+
                          Profession.Homemaker+Profession.Lawyer+Profession.Marketing+
                          Spending_Score,
                        data = train.data, method = "knn",metric = metric, 
                        tuneGrid = grid, trControl = train_Control)
print(fit.knn_model3)
plot(fit.knn_model3, main = "Relationship between k and Accuracy\n in Model 3")

set.seed(7)
prediction_model3 <- predict(fit.knn_model3, newdata = test.data)
cf_model3 <- confusionMatrix(prediction_model3, factor(test.data$Segmentation))
print(cf_model3)


#Model 4: 4 predictors (remove Profession) -> most relevant
set.seed(7)
fit.knn_model4 <- train(Segmentation~Ever_Married+Age+Graduated+Spending_Score,
                        data = train.data, method = "knn",metric = metric, 
                        tuneGrid = grid, trControl = train_Control)
print(fit.knn_model4)
plot(fit.knn_model4, main = "Relationship between k and Accuracy\n in Model 4")

set.seed(7)
prediction_model4 <- predict(fit.knn_model4, newdata = test.data)
cf_model4 <- confusionMatrix(prediction_model4, factor(test.data$Segmentation))
print(cf_model4)

#-------------------------------------------------------------------------------

table_accuracy <- data.frame(k = c(1:20))
table_accuracy$Model1 <- c(0.4522181,0.4478518,0.4628967,0.4750951,0.4817112,
                           0.4838215,0.4895008,0.4924469,0.4960618,0.4980380,
                           0.4989497,0.4993852,0.4995196,0.5000234,0.5016738,
                           0.5019982,0.5031743,0.5042535,0.5050444,0.5051856)
table_accuracy$Model2 <- c(0.4501715,0.4542703,0.4677725,0.4764389,0.4804407,
                           0.4842965,0.4879918,0.4906883,0.4914004,0.4922381,
                           0.4932465,0.4944799,0.4962815,0.4980127,0.4997499,
                           0.5014439,0.5029713,0.5045867,0.5056030,0.5074630)
table_accuracy$Model3 <- c(0.4717345,0.4732391,0.4808507,0.4881687,0.4918324,
                           0.4940300,0.4968375,0.4962623,0.4975648,0.4996938,
                           0.5028484,0.5061023,0.5057970,0.5051893,0.5050905,
                           0.5050626,0.5045494,0.5034775,0.5036223,0.5051182)
table_accuracy$Model4 <- c(0.4624382,0.4624010,0.4619709,0.4635172,0.4618576,
                           0.4606017,0.4602093,0.4606169,0.4604994,0.4608557,
                           0.4626012,0.4628337,0.4629731,0.4648745,0.4671447,
                           0.4688116,0.4704954,0.4715915,0.4732402,0.4741934)
# Change value from accuracy to misclassification rate
table_misrate <- data.frame(k = table_accuracy$k,(1 - table_accuracy[,2:5]))
                        

min(table_misrate[,2:5])
colnames(table_misrate)[which(table_misrate == min(table_misrate[,2:5]),arr.ind = T,)[,2]]
k <- table_misrate$k[which(table_misrate == min(table_misrate[,2:5]),arr.ind = T,)[,1]]

#----- Plot Relationship between k and Accuracy
plot(table_misrate$k, seq(from=0.49,to=0.57,length.out=20),col='white', xlab = 'k',
     ylab='Misclassification rate',main = 'Relationship between\nk and Misclassification rate')

points(table_misrate$k, table_misrate$Model1,col='darkgreen',pch=4)
lines(table_misrate$k, table_misrate$Model1,col='darkgreen',lty=3)

points(table_misrate$k, table_misrate$Model2,col='blue',pch=1)
lines(table_misrate$k, table_misrate$Model2,col='blue',lty=1)

points(table_misrate$k, table_misrate$Model3,col='red',pch=5)
lines(table_misrate$k, table_misrate$Model3,col='red',lty=5)

points(table_misrate$k, table_misrate$Model4,col='purple',pch=17)
lines(table_misrate$k, table_misrate$Model4,col='purple',lty=3)

legend(x = "topright",          
       legend = c("Model 1", "Model 2", "Model 3","Model 4"),
       lty = c(3, 1, 5, 3),pch = c(4, 1, 5, 17),
       col = c('darkgreen','blue','red','purple'))

#-------------------------------------------------------------------------------
