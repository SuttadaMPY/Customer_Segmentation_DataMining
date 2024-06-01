## Classification Tree with rpart (mower)
#install.packages("rpart")
library(rpart)
install.packages("rpart.plot")
library(rpart.plot)

# read data
clean_data2 = clean_data
clean_data2$Gender = factor(clean_data2$Gender, labels = c(0, 1))
clean_data2$Ever_Married = factor(clean_data2$Ever_Married, labels = c(0, 1))
clean_data2$Graduated = factor(clean_data2$Graduated, labels = c(0, 1))
clean_data2$Spending_Score = factor(clean_data2$Spending_Score, labels = c(1,2,3))
clean_data2
colnames(clean_data2)
clean_data2 <- subset(clean_data2, Var_1 != "")

# grow tree (default minsplit = 20 and xval = 10 fold cross validation)
fit <- rpart(Segmentation~Gender+Ever_Married+Age+Graduated+
               Work_Experience+Spending_Score+Family_Size,
             method="class", data=clean_data2, control=rpart.control(minsplit = 20, xval = 10))

# show result 
printcp(fit) # display the results
rpart.plot(fit)

# plot tree
plot(fit, uniform=TRUE, main="Classification Tree for Customer Segmentation")
text(fit, use.n=TRUE, all=TRUE, cex=.8)

# prune the tree
pfit<- prune(fit, cp=   fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"])
# show result
printcp(pfit) # display the results

# plot the pruned tree
plot(pfit, uniform=TRUE, main="Pruned Classification Tree for Mower")
text(pfit, use.n=TRUE, all=TRUE, cex=.8)
