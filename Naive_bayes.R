
install.packages("caret")
library(MASS)
library(klaR)
library(ggplot2)
library(lattice)
library(caret)
library(e1071)

clean_data_nb <- clean_data[,c(2:9,11)]

# factor categorical variables
clean_data_nb$Gender <- factor(clean_data_nb$Gender)
clean_data_nb$Ever_Married <- factor(clean_data_nb$Ever_Married)
clean_data_nb$Graduated <- factor(clean_data_nb$Graduated)
clean_data_nb$Profession <- factor(clean_data_nb$Profession)
clean_data_nb$Spending_Score <- factor(clean_data_nb$Spending_Score)

model_nb <- naiveBayes(Segmentation~. , data = clean_data_nb)
model_nb

predict(model_nb, clean_data_nb[1:10,], type = "raw")
predict_nb <- predict(model_nb, clean_data_nb)
tab_nb <- table(predict_nb, clean_data_nb$Segmentation)

misrate_nb <- (sum(tab_nb)-(tab_nb[1,1]+tab_nb[2,2]+tab_nb[3,3]+tab_nb[4,4]))/sum(tab_nb)
misrate_nb


#-------------------------------------------------------------------------------

# 10 fold CV

clean_data_nb
x <- data.frame(clean_data_nb$Gender,clean_data_nb$Ever_Married,clean_data_nb$Age,
                clean_data_nb$Graduated,clean_data_nb$Profession,clean_data_nb$Work_Experience,
                clean_data_nb$Spending_Score,clean_data_nb$Family_Size)

y <- clean_data_nb$Segmentation
model.nb_10fold <- train(x,y,'nb',trControl=trainControl(method = 'cv', number = 10))
model.nb_10fold
