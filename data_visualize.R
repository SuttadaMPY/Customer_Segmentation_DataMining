update.packages(checkBuilt=TRUE, ask=FALSE)

library(ggplot2)

data <- read.csv("D:/477 Data Mining/Project 477/Train.csv")
summary(data)

data[!complete.cases(data),] # show only rows that have missing values
str(data) # check type of each column
colSums(is.na(data)) # show number of missing values in each column

omit_data <- na.omit(data)
colSums(is.na(omit_data))

clean_data <- subset(omit_data, Graduated != "" & Ever_Married != "" 
                     & Profession != "" & Family_Size != "")

#------------------------------------------------------------------------------

##Pie chart of Gender
gender_count <- table(clean_data$Gender)
lbls <- paste(names(gender_count),"\n",gender_count,sep="")
gender_pie <- pie(gender_count,labels=lbls,main="Pie chart of Gender",xlab="Gender")


## Pie chart of Ever Married
married_count <- table(clean_data$Ever_Married)
lbls <- paste(names(married_count),"\n",married_count,"\n",sep="")
married_pie <- pie(married_count,labels=lbls,main="Pie chart of Ever Married",xlab="Married")


## Histogram of Age
range_ages <- c('<21', '21-30','31-40','41-50','51-60','>60')
br <- c(0,20,30,40,50,60,89)
clean_data['age_group'] = cut(clean_data$Age,br,range_ages,include.lowest = T)


age_count <- as.data.frame(table(clean_data$age_group))
colnames(age_count) <- c('Age_group','Freq')
ggplot(data=age_count, aes(x=Age_group,y=Freq)) +
  geom_bar(stat="identity", aes(fill=Age_group))+
  geom_text(aes(label=Freq), vjust=-0.3, size=3.5)


## Pie chart of Graduated
graduated_count <- table(clean_data$Graduated)
lbls <- paste(names(graduated_count),"\n",graduated_count,"\n",sep="")
graduated_pie <- pie(graduated_count,labels=lbls,main="Pie chart of Graduated",xlab="Graduated")


## Histogram of Profession
profession_count <- as.data.frame(table(clean_data$Profession))
ggplot(data=profession_count, aes(x=Var1,y=Freq)) +
  geom_bar(stat="identity", aes(fill=Var1))+
  geom_text(aes(label=Freq), vjust=-0.3, size=3.5)


## Pie chart of Spending_Score
spend_count <- table(clean_data$Spending_Score)
lbls <- paste(names(spend_count),"\n",spend_count,"\n",sep="")
spend_pie <- pie(spend_count,labels=lbls,main="Pie chart of Spending_Score",xlab="Spending_Score")


## Histogram of famsize
fam_count <- as.data.frame(table(clean_data$Family_Size))
colnames(fam_count) <- c('Family_Size','Freq')
ggplot(data=fam_count, aes(x=Family_Size,y=Freq)) +
  geom_bar(stat="identity", aes(fill=Family_Size))+
  geom_text(aes(label=Freq), vjust=-0.3, size=3.5)

