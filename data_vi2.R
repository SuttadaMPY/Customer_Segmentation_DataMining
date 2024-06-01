##----Data Vis
##Var1
library(RColorBrewer)
var_1tb <- table(clean_data$Var_1,clean_data$Segmentation)
var_1tb

coul1 <- brewer.pal(7, "Set2") 
barplot(var_1tb,
        legend.text = TRUE,
        args.legend = list(x = "topleft"),
        beside = TRUE,
        main = "Var_1",
        ylab = "count",
        xlab = "Segmentation",col=coul1) 

##Gender
gendertb <- table(clean_data$Gender,clean_data$Segmentation)
gendertb
barplot(gendertb,
        legend.text = TRUE,
        args.legend = list(x = "top"),
        beside = TRUE,
        main = "Gender",
        ylab = "count",
        xlab = "Segmentation",col = c("#C3D5EB", "#FECBCB")) 


##Ever Married
marriedtb <- table(clean_data$Ever_Married,clean_data$Segmentation)
marriedtb
barplot(marriedtb,
        legend.text = TRUE,
        args.legend = list(x = "topleft"),
        beside = TRUE,
        main = "Ever_Married",
        ylab = "count",
        xlab = "Segmentation",col = c("#FDF8AF", "#B19CD8")) 

##Age
coul2 <- brewer.pal(4, "Set2")
boxplot(Age~Segmentation,data=clean_data, main="Boxplot", 
        xlab="Segmentation", ylab="Age",col = coul2 )

summary(clean_data$Age)
range_ages <- c('17-30', '31-45','46-60','>60')
br <- c(17,30,45,60,90)
clean_data['age_group'] = cut(clean_data$Age,br,range_ages,include.lowest = T)

agetb <- table(clean_data$age_group,clean_data$Segmentation)
agetb
coul3 <- brewer.pal(4, "Set2")
barplot(agetb,
        legend.text = TRUE,
        args.legend = list(x = "top"),
        beside = TRUE,
        main ="Age_group",
        ylab = "count",
        xlab = "Segmentation",col = coul3) 

#Graduated
graduatedtb <- table(clean_data$Graduated,clean_data$Segmentation)
graduatedtb 
barplot(graduatedtb ,
        legend.text = TRUE,
        args.legend = list(x = "topleft"),
        main="Graduated",
        beside = TRUE,
        ylab = "count",
        xlab = "Segmentation",col = c("#B19CD8", "#FECBCB"))

#Profession
professiontb <- table(clean_data$Segmentation,clean_data$Profession)
professiontb
coul4 <- brewer.pal(4, "Set3")
barplot(professiontb,
        legend.text = TRUE,
        args.legend = list(x = "topright"),
        main="Profession",
        beside = TRUE,
        ylab = "count",
        xlab = "Profession",col = coul4)

#Work Experience
coul5 <- brewer.pal(4, "Set2")
boxplot(Work_Experience~Segmentation,data=clean_data, main="Boxplot", 
        xlab="Segmentation", ylab="Work_Experience",col = coul5 )

summary(clean_data$Work_Experience)
range_workex <- c('Low Experience','Medium Experience','High Experience')
br <- c(-1,1,7,15)
clean_data['Work_Experience_group'] = cut(clean_data$Work_Experience,br,range_workex,include.lowest = T)
clean_data['Work_Experience'] = clean_data$Work_Experience

workextb <- table(clean_data$Work_Experience_group,clean_data$Segmentation)
workextb
coul6 <- brewer.pal(3, "Set2")
barplot(workextb,
        legend.text = TRUE,
        args.legend = list(x = "topleft",bty = "n",
                           inset = c(0,-0.1)),
        main="Work_Experience_group",
        beside = TRUE,
        ylab = "count",
        xlab = "Segmentation",col = coul6)

#Spending Score
spendingtb <- table(clean_data$Spending_Score,clean_data$Segmentation)
spendingtb
coul7 <- brewer.pal(3, "Set3")
barplot(spendingtb,
        legend.text = TRUE,
        args.legend = list(x = "topleft",bty = "n",
                           inset = c(0,-0.1)),
        main="Spending_Score",
        beside = TRUE,
        ylab = "count",
        xlab = "Segmentation",col = coul7)

#Family Size
clean_data$Family_Size
coul8 <- brewer.pal(4, "Set3")
boxplot(Family_Size~Segmentation,data=clean_data, main="Boxplot", 
        xlab="Segmentation", ylab="Family_Size",col = coul8 )

summary(clean_data$Family_Size)
range_family<- c('Small Family','Big Family','Joint Family')
br <- c(0,4,6,10)
clean_data['Family_Size_group'] = cut(clean_data$Family_Size,br,range_family,include.lowest = T)

familytb <- table(clean_data$Family_Size_group,clean_data$Segmentation)
familytb
coul9 <- brewer.pal(3, "Pastel1")
barplot(familytb,
        legend.text = TRUE,
        args.legend = list(x = "topleft",bty = "n",
                           inset = c(0,-0.2)),
        main="Family_Size_group",
        beside = TRUE,
        ylab = "count",
        xlab = "Segmentation",col = coul9)

#Segmentation
coul10 <- brewer.pal(4, "Pastel2")
counts <- table(clean_data$Segmentation)
barplot(counts, main="Bar chart of Segmentation", 
        xlab="Segmentation",col = coul10)

#-----converting binary variables to numeric
clean_data2 = clean_data
clean_data2$Gender = factor(clean_data2$Gender, labels = c(0, 1))
clean_data2$Ever_Married = factor(clean_data2$Ever_Married, labels = c(0, 1))
clean_data2$Graduated = factor(clean_data2$Graduated, labels = c(0, 1))
clean_data2$Spending_Score = factor(clean_data2$Spending_Score, labels = c(1,2,3))
clean_data2

