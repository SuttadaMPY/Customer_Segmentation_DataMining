##------Data processing 
data_train = read.csv("D:/477 Data Mining/Project 477/Train.csv")
data_test = read.csv("D:/477 Data Mining/Project 477/Test.csv")

#Displays Data Information
dim(data_train)
dim(data_test)
colnames(data_train)
colnames(data_test)

#Displays Data Head (Top Rows) and Tail (Bottom Rows) of the Dataframe (Table)
head(data_train)
tail(data_train)
head(data_test)
tail(data_test)

#Displays Data Description using Statistics 
summary(data_train)
summary(data_test)

#Remove Duplicate Rows from Train data if present
data_train_dc <- data_train[!duplicated(data_train), ]
nrow(data_train)  
nrow(data_train_dc) #No Duplicates at all

#Remove null values
colSums(is.na(data_train))
data_train_df<-data.frame(data_train)
(colMeans(is.na(data_train_df)))*100 # so,null values are very less so we can simply drop these values

omit_data <- na.omit(data)
colSums(is.na(omit_data))

#Displays Unique Values in Each Column
nrow(omit_data)

unique(omit_data$ID)
aggregate(data.frame(count = omit_data$ID), list(value = omit_data$ID), length)

unique(omit_data$Gender)
aggregate(data.frame(count = omit_data$Gender), list(value = omit_data$Gender), length)

unique(omit_data$Ever_Married)
aggregate(data.frame(count = omit_data$Ever_Married), list(value = omit_data$Ever_Married), length)

unique(omit_data$Age)
aggregate(data.frame(count = omit_data$Age), list(value = omit_data$Age), length)

unique(omit_data$Graduated)
aggregate(data.frame(count = omit_data$Graduated), list(value = omit_data$Graduated), length)

unique(omit_data$Profession)
aggregate(data.frame(count = omit_data$Profession), list(value = omit_data$Profession), length)

unique(omit_data$Work_Experience)
aggregate(data.frame(count = omit_data$Work_Experience), list(value = omit_data$Work_Experience), length)

unique(omit_data$Spending_Score)
aggregate(data.frame(count = omit_data$Spending_Score), list(value = omit_data$Spending_Score), length)

unique(omit_data$Family_Size)
aggregate(data.frame(count = omit_data$Family_Size), list(value = omit_data$Family_Size), length)

unique(omit_data$Var_1)
aggregate(data.frame(count = omit_data$Var_1), list(value = omit_data$Var_1), length)

unique(omit_data$Segmentation)
aggregate(data.frame(count = omit_data$Segmentation), list(value = omit_data$Segmentation), length)


clean_data <- subset(omit_data, Graduated != "" & Ever_Married != "" 
                     & Profession != "" &Family_Size != "" &Var_1 != "")
colSums(is.na(clean_data)) 



